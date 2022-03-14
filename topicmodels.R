library(tidyverse)
library(quanteda)
library(topicmodels)

# load raw texts
load("mat_mr.Rda")


#create corpus
corp <- corp_en

# clean raw text and tokenize
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


dfm <- dfm(toks_nostop) %>% dfm_remove(stopwords("english")) %>% 
  dfm_trim(min_docfreq = 5) %>% dfm_wordstem()

dtm <- convert(dfm, to = "topicmodels")
set.seed(1, sample.kind = "Rounding")
a <- Sys.time()
m <- LDA(dtm, method = "Gibbs", k = 10, control = list(alpha = 0.1))
b <- Sys.time()
b-a
m
terms(m, 10)


topic <- 5
words <- posterior(m)$terms[topic, ]
topwords <- head(sort(words, decreasing = T), n = 50)
head(topwords)

topic.docs = posterior(m)$topics[,topic]
topic.docs <- sort(topic.docs, decreasing = T)

topdoc <- names(topic.docs)[1]
corp[docnames(corp)==topdoc]

corp[company=="COLRUYT#"]


