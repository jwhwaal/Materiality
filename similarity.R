library(quanteda.textstats)

# similarities for documents
tstat1 <- textstat_simil(dfm_s, method = "cosine", margin = "documents")
as.matrix(tstat1)
as.list(tstat1)


tstat4 <- textstat_dist(dfm_par, margin = "documents")
as.matrix(tstat4)
plot(hclust(as.dist(tstat4)))

