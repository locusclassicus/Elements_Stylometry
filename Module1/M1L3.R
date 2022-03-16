## load packages
install.packages("gutenbergr")
library(gutenbergr)

## access metadata 
gutenberg_authors ## note lots of NAs! NA is a logical constant of length 1 which 
## contains a missing value indicator != "NA" 
gutenberg_metadata
gutenberg_works() ## you can specify meta-fields while downloading!
latin.works <- gutenberg_works(languages = "la") ## it's not very tidy!
caesar <- gutenberg_works(author == "Caesar, Julius", languages = "la") 
class(caesar)

## https://tibble.tidyverse.org/reference/tbl_df-class.html

## find text's id
library(dplyr)
gutenberg_metadata %>%
  filter(title == "De Bello Gallico") ## not found

library(stringr)
gutenberg_works(str_detect(title, "De Bello Gallico"), languages = "la")

## https://cran.r-project.org/web/packages/gutenbergr/vignettes/intro.html

BG <- gutenberg_download(218, meta_fields = "title") 

## using tidy format
library(tidytext)
words <- BG %>%
  unnest_tokens(word, text)

words <- words[-c(1:18), ]
save(words, file = "./data/Caesar.Rdata")
## count
book_words <- words %>% count(word, sort = TRUE)
total_words <- sum(book_words$n)
save

## if you need a character vector
BG.str <- words$word
write.table(BG.str, file = "BG.txt", quote = F, row.names = F, col.names = F)


