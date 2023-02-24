# exploring sentence and word length in Cicero's letters; 
# no diachronic changes visible

load("/Users/olga/R_Workflow/Elements_Stylometry/data/CiceroLetters.Rdata")

# letters per year
cicero_letters %>% group_by(date) %>% 
  ggplot(aes(date, )) + geom_histogram()

# clean
cicero_letters$text <- gsub("[\r\n†]", "", cicero_letters$text)
cicero_letters$text <- gsub("a\\. d\\.", "", cicero_letters$text)
cicero_letters$text <- gsub("Kalend\\.", "", cicero_letters$text)
cicero_letters$text <- gsub("Kal\\.", "", cicero_letters$text)
cicero_letters$text <- gsub("\\s+[A-Za-z]\\.", "", cicero_letters$text)
cicero_letters$text <- gsub("^[A-Za-z]\\.", "", cicero_letters$text)

# delete abbreviations
cicero_letters$text <- gsub("Id\\.", "", cicero_letters$text)
cicero_letters$text <- gsub("[Pp]rid\\.", "", cicero_letters$text)
cicero_letters$text <- gsub("Mart\\.", "", cicero_letters$text)
cicero_letters$text <- gsub("Non\\.", "", cicero_letters$text)
cicero_letters$text <- gsub("Dec\\.", "", cicero_letters$text)
cicero_letters$text <- gsub("Febr\\.", "", cicero_letters$text)
cicero_letters$text <- gsub("coss\\.", "", cicero_letters$text)
cicero_letters$text <- gsub("\\s+pl\\.", "", cicero_letters$text)
cicero_letters$text <- gsub("\\*{3}", "", cicero_letters$text)
cicero_letters$text <- gsub("Mai\\.", "", cicero_letters$text)
cicero_letters$text <- gsub("Ian\\.", "", cicero_letters$text)
cicero_letters$text <- gsub("\\s+pr\\.", "", cicero_letters$text)
cicero_letters$text <- gsub("Apr\\.", "", cicero_letters$text)
cicero_letters$text <- gsub("Iun\\.", "", cicero_letters$text)
cicero_letters$text <- gsub("Sext\\.", "", cicero_letters$text)
cicero_letters$text <- gsub("Sept\\.", "", cicero_letters$text)
cicero_letters$text <- gsub("Octobr\\.", "", cicero_letters$text)
cicero_letters$text <- gsub("Nov\\.", "", cicero_letters$text)

# split into words
cicero_words <- cicero_letters %>% 
  unnest_tokens(word, text, token = "regex", pattern = "\\s") 
# otherwise a problem with letter 5 book 5 (betacode)

# sentence length
cicero_letter_length <- cicero_words %>% 
  group_by(book_nr, letter_nr, dest, date) %>% 
  summarise(letter_length = n())

save(cicero_letter_length, file = "CiceroLetterLength.Rdata")

# # split into sentences
# cic_tokenized <- cicero_letters %>% unnest_tokens(input = text, 
#               output = sent, token = "regex", 
#               pattern = "\\.|\\?|\\!")
# 
# cic_tokenized <- cic_tokenized %>% 
#   group_by(book_nr, letter_nr, date, dest) %>% 
#   mutate(sent_id = row_number()) %>% ungroup()
# 
# cic_tokenized2 <- cic_tokenized %>% 
#   group_by(book_nr, letter_nr, dest, date, sent_id) %>% 
#   unnest_tokens(input=sent, output=word,token="words") %>% 
#   ungroup()
# 
# # count sent length
# cic_sent_length <- cic_tokenized2 %>% 
#   group_by(book_nr, letter_nr, dest, date, sent_id) %>% 
#   summarise(n = n()) %>% ungroup()
# 
# cic_mean_sent <- cic_sent_length %>% 
#   group_by(date) %>% summarise(mean=mean(n))
# 
# cic_mean_sent %>% ggplot(aes(date, mean)) + geom_line()
# 
# 
