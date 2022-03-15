library(quanteda)
library(readtext)
library(readr)

#read some pdf files to make a corpus

start <- Sys.time()
mat_mr <- readtext(paste0('./docs/*.pdf'), 
                   docvarsfrom = "filenames", 
                   docvarnames = c("year", "type", "language", "company"),
                   sep = "_")
eind <- Sys.time()
tijd <- eind - start
tijd

fl <- list.files('./docs/')
write.csv(fl, "filelist.txt")
length(fl)

save(mat_mr, file = "mat_mr.Rda")
write.csv(mat_mr, "sr_retail.txt")


# or read from txt files converted with tools.pdfforge.org/extract-text (better formating)
text_df <- readtext(paste0('./docs/raw/*.txt'), 
                    docvarsfrom = "filenames", 
                    docvarnames = c("year", "type", "language", "company"),
                    dvsep = "_")
write_excel_csv(text_df, file = "text_df.txt")

text_df$text %>% str_replace()

