library(quanteda)
library(readtext)
library(quanteda.textstats)
library(quanteda.corpora)
library(seededlda)
library(topicmodels)
library(stringr)
library(tidyverse)
# LDA
a2 <- Sys.time()
set.seed(1234, sample.kind = "Rounding")
lda <- textmodel_lda(dfm_stem, k = 9, verbose = T, alpha = 1)
summary(lda)
df_lda <- as.data.frame(lda$theta) %>% 
  tibble::rownames_to_column(., "document")
b2 <- Sys.time()
t2 <- b2-a2
t2
terms <- seededlda::terms(lda) %>% as_tibble()
write_excel_csv(terms, "terms.txt")
save(lda, file = "lda.Rdata")