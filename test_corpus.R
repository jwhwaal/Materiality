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
  corpus_segment(pattern = "•")
docvars(corp_mr_par) %>% group_by(company, year) %>% summarize()

#make a list of unique company designators
company_list <- unique(docvars(corp_en)) %>% select(company)
write_excel_csv(company_list, "companylist.txt")

#read company names
library(readxl)
company_names <- read_excel("company_names.xlsx") 
cl <- company_names$retailer %>% tokens() %>% tokens_split() %>% unlist()
words <- c("sek", "eur", "ica", "coop", "colruyt", "axfood", "ahold", "delhaize",
           "million", "appendix", "see", "axfood's", "gruppen's","co-op", "asda",
           "aldi", "per", "year", "also", "can", "use", "chf", "finland",
           "switzerland", "delhaize's", "years", "euro", "sweden", "kesko",
           "spain", "steghaus", "mbb", "italian", "thanks", "new", "u.s.",
           "transgourmet", "gmbh", "g.m.b.h.", "esselunga", "milan",
           "euro", "naturama", "della", "banco", "alimentar", "co-operative",
           "zurich", "basel", "bern", "franc", "billion", "italia",
           "april", "januari", "u.", "adriatico")
cl <- append(cl, words)


#tokenize
toks_nostop <- corp_mr_par %>% 
  tokens(remove_punct = TRUE, remove_symbols = TRUE, 
         remove_numbers = TRUE, remove_url = TRUE) %>% 
  tokens_select(min_nchar = 3) %>%
  tokens_remove(stopwords("en")) %>%
  tokens_remove(pattern = cl) %>%
  tokens_remove(c("*-time", "*-timeUpdated", "GMT", "BST", "*.com", "ltd", "group", 
                  "holdings", "inc", "business"))  


# apply sdg dictionary to tokens object
toks_sdg <- tokens_keep(toks_nostop, pattern = dict, window = 3)

# create document feature matrices
# base dfm 
dfm <- dfm(toks_nostop) %>%dfm_remove(pattern = cl)
head(dfm, 25)
# sdg dfm
dfm_s <- dfm_lookup(dfm, dict) 
head(sdg_df,25)
dfm_stem <- dfm_wordstem(dfm)

#LDA model
#convert corpus to paragraphs
#https://www.youtube.com/watch?v=4YyoMGv1nkc


# LDA
a2 <- Sys.time()
lda <- textmodel_lda(dfm_stem, k = 9, verbose = T, alpha = 1)
summary(lda)
df_lda <- as.data.frame(lda$theta) %>% 
  tibble::rownames_to_column(., "document")
b2 <- Sys.time()
t2 <- b2-a2
t2
topics(lda, 10)



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



