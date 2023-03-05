library(tidyr)

# latin affectus
# url <- "https://raw.githubusercontent.com/CIRCSE/Latin_Sentiment_Lexicons/master/LatinAffectusv2.tsv"
# filename = "LatinAffectus.tsv"
# download.file(url, filename)

# read
sent_lex <- read_tsv("LatinAffectus.tsv")

# join
cic_joined<- cic_lemmas %>% 
  inner_join(sent_lex, by = "lemma") %>% 
  filter(!has_polarity == "neutral")

# group and count
stat_polarity <- cic_joined %>% group_by(book_nr, letter_nr, date, has_polarity) %>% 
  summarise(total = sum(polarity_score))

#stat polarity weighted
cicero_words <- cicero_att %>%
  unnest_tokens(word, text, token = "words")
cicero_letter_length <- cicero_words %>%
  group_by(book_nr, letter_nr) %>% summarise(n=n())

stat_polarity <- stat_polarity %>% 
  left_join(cicero_letter_length, by = c("book_nr", "letter_nr")) 

stat_polarity <- stat_polarity %>% mutate(ratio = total/n)

# stat polarity sum
cic_sentiment <- stat_polarity %>% group_by(book_nr, letter_nr, date) %>% 
  summarise(sentiment = sum(ratio))

# plot mean per year
cic_sentiment %>% group_by(date) %>% 
  summarise(mean_sent = mean(sentiment)) %>% 
  ggplot(aes(date, mean_sent)) +
  geom_bar(stat = "identity")

# plot  per letter_id
cic_sentiment$letter_nr <- gsub("A", ".1", cic_sentiment$letter_nr)
cic_sentiment$letter_nr <- gsub("B", ".2", cic_sentiment$letter_nr)
cic_sentiment$letter_nr <- gsub("C", ".3", cic_sentiment$letter_nr)
cic_sentiment$letter_nr <- gsub("D", ".4", cic_sentiment$letter_nr)


cic_sentiment <- cic_sentiment %>%
  mutate(book_nr = as.numeric(book_nr), letter_nr = as.numeric(letter_nr)) 
  

cic_sent_grouped <- cic_sentiment %>% 
  arrange(book_nr, letter_nr) %>% ungroup()

cic_sent_grouped <- cic_sent_grouped %>% mutate(index = row_number()) %>% 
  mutate(sign = case_when(sentiment > 0 ~ "pos", 
                          sentiment < 0 ~ "neg"))

save(cic_sent_grouped, file = "CiceroSent.Rdata")
  
cic_sent_grouped %>%  
  ggplot(aes(index, sentiment, fill = sign)) +
  geom_bar(stat = "identity") 

# add dates to the plot
label_data_min <- cic_sent_grouped %>% 
  arrange(sentiment) %>% slice_min(n = 10, sentiment)

label_data_max <- cic_sent_grouped %>% 
  arrange(sentiment) %>% slice_max(n = 10, sentiment)


cic_sent_grouped %>%  
  ggplot(aes(index, sentiment, fill = sign)) +
  geom_bar(stat = "identity", show.legend = F) + 
  geom_text(aes(label = date), vjust = 1.5, colour = "grey30", 
            data = label_data_min) + 
  geom_text(aes(label = date), vjust = -1.5, colour = "grey30", 
            data = label_data_max) 

# is our lexicon biased?
sent_lex %>% group_by(has_polarity) %>% count()

