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


# seeded LDA to extract terms that have to do with tone
#https://provalisresearch.com/products/content-analysis-software/wordstat-dictionary/
#https://www.catscanner.net/dictionaries/


a2 <- Sys.time()
set.seed(1234, sample.kind = "Rounding")
slda <- textmodel_seededlda(dfm_stem, dictionary=tone,
                      valuetype = c("glob", "regex", "fixed"),
                      case_insensitive = TRUE,
                      residual = FALSE,
                      weight = 0.01,
                      max_iter = 2000,
                      alpha = NULL,
                      beta = NULL,
                      verbose = quanteda_options("verbose"))
summary(slda)
df_lda <- as.data.frame(slda$theta) %>% 
  tibble::rownames_to_column(., "document")
b2 <- Sys.time()
t2 <- b2-a2
t2
terms <- seededlda::terms(slda) %>% as_tibble()
write_excel_csv(terms, "terms.txt")
save(slda, file = "slda.Rdata")