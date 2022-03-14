library(quanteda)
library(readtext)
library(quanteda.textstats)
library(quanteda.corpora)
library(seededlda)
library(topicmodels)
library(stringr)
library(tidyverse)

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


#split in paragraphs
corp_mr_par <- corp_en %>%
  corpus_reshape(.,to = "paragraphs")

head(corp_mr_par)
corp_mr_par %>% as_tibble() %>% write.csv(., "corpus_en.txt")
docvars(corp_mr_par) %>% group_by(company, year) %>% summarize()

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


#tokenize
toks_nostop <- corp_mr_par %>% 
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


load("lda.Rdata")
#write results to data frame
theta <- as.data.frame(lda$theta)
theta <- tibble::rownames_to_column(theta, "document")
theta <- theta %>% mutate(company = substr(document, 12,19),
       year = substr(document, 1,4),
       type = substr(document, 6,7)) %>%
  left_join(., company_names) %>%
rename(., "social_supply_chain" = topic7,
       "customer_store" = topic8,
       "waste_emission" = topic1,
       "employee_relations" = topic2,
       "finance_accounting" = topic4,
       "charity" = topic6,
       "governance" = topic9,
       "risk" = topic5,
       "product_brand_certification" = topic3)

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

  
df_long <- df_slda %>% mutate(company = substr(document, 12,19),
                              year = substr(document, 1,4),
                              type = substr(document, 6,7)) %>%
  mutate(company = str_remove(company, pattern = "#+")) %>%
  select(-other) %>%
  pivot_longer(., cols = sdg1:sdg17, names_to = "sdg", values_to = "theta")

# do dictionary searches 

# sdg dfm
toks_nostop_comp <- toks_nostop %>% tokens_compound(pattern = "glob")

dfm_s <- dfm(toks_nostop_comp) %>% dfm_lookup(., dict) 
head(dfm_s,25)
dfm_sdg <- dfm_s %>% convert(., to = "data.frame")

sdg <- dfm_sdg %>% pivot_longer(., cols = sdg01:gc, names_to = "theme", values_to = "no_of_words") %>%
  mutate(company = substr(doc_id, 12,19),
         year = substr(doc_id, 1,4),
         type = substr(doc_id, 6,7)) %>%
  left_join(.,company_names)
p3 <- sdg %>% ggplot(aes(retailer, no_of_words, fill = theme)) +
  geom_col()
p3 + coord_flip()


p4 <- sdg %>% ggplot(aes(country, no_of_words)) +
  geom_col()
p4 + coord_flip() + facet_wrap(vars(theme))
