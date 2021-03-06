library(readtext)
library(readr)
library(stringr)
library(tidyverse)

#read some pdf files to make a corpus

start <- Sys.time()
mat_mr <- readtext(paste0('./docs/*.pdf'), 
                   docvarsfrom = "filenames", 
                   docvarnames = c("year", "type", "language", "company"),
                   sep = "_")
eind <- Sys.time()
tijd <- eind - start
tijd

fl <- list.files('./docs/raw')
write.csv(fl, "filelist.txt")
length(fl)

save(mat_mr, file = "mat_mr.Rda")
write.csv(mat_mr, "sr_retail.txt")


# or read from .txt files converted with tools.pdfforge.org/extract-text (better formatting)
#check if all file names are uniform
fl <- list.files('./docs/raw')
write.csv(fl, "filelist.txt")
length(fl)
texts <- readtext(paste0('./docs/raw/*.txt'), 
                    docvarsfrom = "filenames", 
                    docvarnames = c("year", "type", "language", "company"),
                    dvsep = "_")

#then clean text_df : ".\n" by "##" and "\n" by space.
text <- texts %>% mutate(text = str_replace_all(text, "\\.\\n", "\\.## "))
texts <- text %>% mutate(text = str_replace_all(text, "\\n", ' '))
write.csv(text, "texts.txt")
