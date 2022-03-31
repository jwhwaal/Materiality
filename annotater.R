library(udpipe)
#download model
dl <- udpipe_download_model(language = "english")
udmodel_en <- udpipe_load_model(file = dl$file_model)

input <- list(toks_nostop)
## Or put every token of each document in 1 string separated by a space and use tokenizer: horizontal
##   Mark that if a token contains a space, you need to replace the space 
##   with the 'NO-BREAK SPACE' (U+00A0) character to make sure it is still considered as one token
txt <- sapply(input, FUN=function(x){
  x <- gsub(" ", intToUtf8(160), x) ## replace space with no-break-space
  paste(x, collapse = " ")
})
x<- as.character(txt)
y <- udpipe_annotate(udmodel_en, x = as.character(txt), tokenizer = "horizontal",
                     doc_id = paste("doc", seq_along(x), sep = ""))
y <- as.data.frame(y)


library(textrank)
library(udpipe)
stats <- textrank_keywords(y$lemma, 
                           relevant = y$upos %in% c("NOUN", "ADJ"), 
                           ngram_max = 8, sep = " ")
stats <- subset(stats$keywords, ngram > 1 & freq >= 5)
library(wordcloud)
wordcloud(words = stats$keyword, freq = stats$freq)
