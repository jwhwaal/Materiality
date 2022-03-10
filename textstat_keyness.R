sdg <- c("sdg", "agenda", "sustainable", "development")
toks_inside <- tokens_keep(toks_sdg, pattern = sdg, window = 10)
toks_inside <- tokens_remove(toks_sdg, pattern = sdg) # remove the keywords
toks_outside <- tokens_remove(toks_sdg, pattern = sdg, window = 10)


dfmat_inside <- dfm(toks_inside)
dfmat_outside <- dfm(toks_outside)

tstat_key_inside <- textstat_keyness(rbind(dfmat_inside, dfmat_outside), 
                                     target = seq_len(ndoc(dfmat_inside)))
head(tstat_key_inside, 50)