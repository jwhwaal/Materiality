library(quanteda)
library(readtext)
library(quanteda.textstats)
library(quanteda.corpora)
library(seededlda)
library(topicmodels)
library(stringr)
library(tidyverse)
library(readr)

#load bulk data
load("mat_mr.Rda")

#construct corpus
corp_mr <- quanteda::corpus(mat_mr)
documents <- split(mat_mr, mat_mr$doc_id)
lapply(names(documents), function(x){
  write_csv(documents[[x]], paste(x, ".csv", sep = ""))
})

#make corpus subset based on language
corp_en <- corpus_subset(corp_mr, language == "EN")

#BEGIN VANAF HIER
#read texts and make corpus from the hashed text
texts <- read_csv("texts.txt")
corp_en <- corpus(texts) 
ndoc(corp_en)
t <- textstat_summary(corp_en)

# split corpus into segments based on the ## paragraph marker
corp_p <- corpus_segment(corp_en, pattern = "##") 
head(corp_p)
str(corp_p)
docvars(corp_p)

corp_p %>% convert(.,to="data.frame") %>% write_excel_csv(., "corpus_p.txt")
docvars(corp_p) %>% group_by(company, year) %>% summarize()

#make a list of unique company designators
company_list <- unique(docvars(corp_en)) %>% select(company)
write_excel_csv(company_list, "companylist.txt")

#read company names
library(readxl)
company_names <- read_excel("company_names.xlsx") 
cl <- company_names$retailer %>% tokens() %>% tokens_split() %>% unlist()
words <- c("*-time", "*-timeUpdated", "GMT", "BST", "*.com", "ltd", "group", 
           "holdings", "inc", "business","sek", "eur", "ica", "coop", 
           "colruyt", "axfood", "ahold", "delhaize",
           "million", "appendix", "see", "axfood's", "gruppen's","co-op", "asda",
           "aldi", "per", "year", "also", "can", "use", "chf", "finland",
           "switzerland", "delhaize's", "years", "euro", "sweden", "kesko",
           "spain", "steghaus", "mbb", "italian", "thanks", "new", "u.s.",
           "transgourmet", "gmbh", "g.m.b.h.", "esselunga", "milan",
           "euro", "naturama", "della", "banco", "alimentar", "co-operative",
           "zurich", "basel", "bern", "franc", "billion", "italia",
           "april", "januari", "u.", "adriatico", "belgium", 
           "france", "swiss", "belgian", "virya","norgesgruppen",
           "baltic", "kesko")
cl <- append(cl, words)
unique(company_names$retailer)

#tokenize
toks_nostop <- corp_p %>% 
  tokens(remove_punct = TRUE, remove_symbols = TRUE, 
         remove_numbers = TRUE, remove_url = TRUE) %>% 
  tokens_select(min_nchar = 3) %>%
  tokens_remove(stopwords("en")) %>%
  tokens_remove(pattern = cl) 


# apply sdg dictionary to tokens object
toks_sdg <- tokens_keep(toks_nostop, pattern = dict, window = 3)

# create document feature matrices
# base dfm 
dfm <- dfm(toks_nostop) %>%dfm_remove(pattern = cl)
head(dfm, 25)

dfm_stem <- dfm_wordstem(dfm)

#LDA model
#convert corpus to paragraphs
#https://www.youtube.com/watch?v=4YyoMGv1nkc

# compute document variables
nt <- ntoken(dfm_stem) %>% as.data.frame() %>% rownames_to_column() 
colnames(nt) <- c("document", "number")
nt <- nt %>% mutate(company = substr(document, 12,19)) %>%
  left_join(.,company_names) 
nt %>% group_by(retailer) %>% summarize(no_of_tokens = sum(number))

unique(theta$company)
unique(company_names$company)
load("lda.Rdata")
#write results to data frame
theta <- as.data.frame(lda$theta)
theta <- tibble::rownames_to_column(theta, "document")
theta <- theta %>% mutate(company = substr(document, 12,19),
       year = substr(document, 1,4),
       type = substr(document, 6,7)) %>%
  left_join(., company_names, by = c("company" = "company")) %>%
rename(., "social_supply_chain" = topic5,
       "customer_store" = topic1,
       "waste_emission" = topic6,
       "employee_relations" = topic7,
       "finance_accounting" = topic4,
       "charity" = topic2,
       "governance" = topic3,
       "risk" = topic8,
       "product_brand_certification" = topic9)

#theta[which(is.na(theta$retailer)==TRUE),]

p1 <- theta %>% select(-c("year", "type")) %>%
  pivot_longer(., cols = 2:10, names_to = "topic", values_to = "theta") %>%
  group_by(retailer, topic) %>%
  summarize(value = mean(theta)) %>%
  ggplot(aes(retailer, value)) +
  geom_col() +
  facet_wrap(vars(topic)) +
  coord_flip()
p1
#https://tm4ss.github.io/docs/Tutorial_6_Topic_Models.html

p2 <- theta %>% select(-c("year")) %>%
  filter(type %in% c("AR", "SR")) %>%
  pivot_longer(., cols = 2:10, names_to = "topic", values_to = "theta") %>%
  group_by(type, topic) %>%
  summarize(value = mean(theta)) %>%
  ggplot(aes(type, value)) +
  geom_col() +
  facet_wrap(vars(topic)) +
  coord_flip()
p2


# use LDAvis to explore topics
library(LDAvis)
library(servr)
json <- createJSON(phi = lda$phi,
                   theta = lda$theta, 
                   doc.length = quanteda::ntoken(dfm_stem),
                   vocab = quanteda::featnames(dfm_stem), 
                   term.frequency = quanteda::featfreq(dfm_stem))
serVis(json, out.dir = 'vis', 
       open.browser = T)


# do dictionary searches 
# sdg dfm
#tokenize
toks <- corp_en %>% 
  tokens(remove_punct = TRUE, remove_symbols = TRUE, 
         remove_numbers = TRUE, remove_url = TRUE) %>% 
  tokens_select(min_nchar = 3) %>%
  tokens_remove(stopwords("en")) %>%
  tokens_remove(pattern = cl) %>%
  tokens_compound(pattern = "glob")

#dfm on large dictionary
dfm_s <- dfm(toks) %>% 
  dfm_lookup(., dict) %>% 
  dfm_weight(., scheme = "prop") #weighted frequencies corrects for different doc size
head(dfm_s,25)
dfm_sdg <- dfm_s %>% convert(., to = "data.frame")



sdg <- dfm_sdg %>% pivot_longer(., cols = sdg01:cert, names_to = "theme", values_to = "value") %>%
  mutate(company = substr(doc_id, 12,19),
         year = substr(doc_id, 1,4),
         type = substr(doc_id, 6,7)) %>%
  left_join(.,company_names)
p3 <- sdg %>% arrange(country) %>% 
  ggplot(aes(retailer, value, fill = theme)) +
  geom_col()
p3 + coord_flip()

# create a labeller
theme_names <- list(
  'sdg01'="No Poverty",
  'sdg02'="Zero Hunger",
  'sdg03'="Good Health and Well-Being",
  'sdg04'="Quality Education",
  'sdg05'="Gender Equality",
  'sdg06'="Clear Water and Sanitation",
  'sdg07'="Affordable and Clean Energy",
  'sdg08'="Decent Work and Economic Growth",
  'sdg09'="Industry, Innovation and Infrastructure",
  'sdg10'="Reduced Inequalities",
  'sdg11'="Sustainable Cities and Communities",
  'sdg12'="Responsible Consumption and Production",
  'sdg13'="Climate Action",
  'sdg14'="Life Below Water",
  'sdg15'="Life on Land",
  'sdg16'="Peace, Justice & Strong Institutions",
  'sdg17'="Partnership for the Goals",
  'sdg'="SDG mentioning",
  'gri'="GRI mentioning",
  'cert'= "Certification mentioning"
)

theme_labeller <- function(variable,value){
  return(theme_names[value])
}

p4 <- sdg %>% mutate(theme = as.factor(theme), ownership = as.factor(ownership)) %>%
  filter(ownership != "employee_cooperative") %>%
  group_by(ownership, theme) %>%
  summarize(score = mean(value)) %>%
  ggplot(aes(ownership, score)) +
  geom_col()
p4 + coord_flip() + facet_wrap(vars(theme), labeller=theme_labeller)



temp <- sdg %>% mutate(theme = as.factor(theme), ownership = as.factor(ownership)) %>%
  filter(ownership != "employee_cooperative") %>%
  select(theme, value, ownership) %>%
  group_by(ownership, theme) %>%
  summarize(score = mean(value)) 

temp %>%
  ggplot(aes(ownership, theme)) +
  geom_tile(aes(fill=score))

sdg %>% filter(ownership != "employee_cooperative") %>%
  ggplot(aes(theme, no_of_words)) +
  geom_boxplot() +
  facet_grid(vars(ownership))


sdg_wide <- dfm_sdg %>% pivot_longer(., cols = sdg01:sdg17, names_to = "theme", values_to = "value") %>%
  mutate(company = substr(doc_id, 12,19),
         year = substr(doc_id, 1,4),
         type = substr(doc_id, 6,7),
         sdg= as.factor(ifelse(sdg>0,1,0))) %>%
  left_join(.,company_names)

  sdg_wide %>% ggplot(aes(theme, value)) +
  geom_boxplot() +
  facet_grid(vars(sdg))

  
  # number of reports mentioning sdgs
mean(sdg_wide$sdg==1)
mean(dfm_sdg$sdg>0) 
sum(dfm_sdg$sdg>0)      


# select corpus parts with SDG mentioning
toks <- corp_p %>% 
  tokens(remove_punct = TRUE, remove_symbols = TRUE, 
         remove_numbers = TRUE, remove_url = TRUE) %>% 
  tokens_select(min_nchar = 3) %>%
  tokens_remove(stopwords("en")) %>%
  tokens_remove(pattern = cl) %>%
  tokens_compound(pattern = "glob")

dfm_par <- dfm(toks) %>% dfm_remove(pattern = cl) %>%
  dfm_lookup(., dict_small, valuetype = "fixed") 
str(dfm_par)

sdg_df <- dfm_par %>% convert(., to = "data.frame")
sdg_texts <- sdg_df %>% filter(sdg>0) %>% select(doc_id) 

sdg_corpus <- corp_p %>% convert(., to = "data.frame")
sdg_corpus <- sdg_corpus %>% right_join(.,sdg_texts)
write_excel_csv(sdg_corpus, "sdg_corpus.txt")
