# exploring sentence and word length in Cicero's letters

# libraries
library(tidytext)
library(tidyverse)

# have a look at one letter 
cicero_att %>% slice(1) %>% pull(text)

# clean
cicero_att$text <- gsub("[\r\n†]", "", cicero_att$text)

cicero_att$text <- gsub("\\s+[A-Za-z]{1}\\.", "", cicero_att$text)
cicero_att$text <- gsub("^[A-Za-z]{1}\\.", "", cicero_att$text)

# delete other abbreviations
cicero_att$text <- gsub("[dIiVvXxLC] Kalend\\.|Kal\\.|Id\\.|Idus|Non\\.", "", cicero_att$text)

cicero_att$text <- gsub("Ian\\.|Ianuar\\.", "", cicero_att$text)
cicero_att$text <- gsub("Febr\\.", "", cicero_att$text)
cicero_att$text <- gsub("Mart\\.", "", cicero_att$text)
cicero_att$text <- gsub("Apr\\.", "", cicero_att$text)
cicero_att$text <- gsub("Mai\\.", "", cicero_att$text)
cicero_att$text <- gsub("Iun\\.", "", cicero_att$text) # Quintilis is not abbreviated
cicero_att$text <- gsub("Sext\\.|Sextilis\\.", "", cicero_att$text)
cicero_att$text <- gsub("Sept\\.", "", cicero_att$text)
cicero_att$text <- gsub("Octobr\\.", "", cicero_att$text)
cicero_att$text <- gsub("Nov\\.", "", cicero_att$text)
cicero_att$text <- gsub("Dec\\.", "", cicero_att$text)


cicero_att$text <- gsub("[Cc]oss\\.", "", cicero_att$text)
cicero_att$text <- gsub("[Pp]l\\.", "", cicero_att$text)
cicero_att$text <- gsub("[Pp]r\\.", "", cicero_att$text)

cicero_att$text <- gsub("\\*{3}", "", cicero_att$text)
cicero_att$text <- gsub("\\*\\s+", "", cicero_att$text)

cicero_att$text <- gsub("[[:punct:]]", "", cicero_att$text)

# We now have a text ready for lemmatization. But at later stage, we shall
# need to know the length of each letter in words, so we calculate it here

# split into words
cicero_words <- cicero_att %>% 
  unnest_tokens(word, text, token = "words")

# unite all ids
cicero_words <- cicero_words %>% 
  unite("letter_id", c("book_nr", "letter_nr"), remove = T) 

# words per letter
cicero_letter_length <- cicero_words %>% 
  group_by(letter_id) %>% summarise(n=n())
save(cicero_letter_length, file = "CiceroLetterLength.Rdata")

# histogram
library(MASS)
truehist(cicero_letter_length$n)

