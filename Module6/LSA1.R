# библиотеки
library(tidytext)
library(tidyverse)
library(stopwords)
library(readr)
library(udpipe)

# link to data
# https://www.kaggle.com/datasets/alexandertesemnikov/kinopoisktop250russiandataset

# read file
reviews <- read_csv("kinopoisk-top250.csv") %>% select(movie, overview)

# lemmatize
# rus <- udpipe_download_model(language = "russian")
udmodel_rus <- udpipe_load_model(file = "russian-gsd-ud-2.5-191206.udpipe")

review_lemmas <-  c()

for (i in 1:nrow(reviews)) {
  overview <- reviews %>% slice(i) %>% pull(overview)
  movie <- reviews %>% slice(i) %>% select(movie)
  annotated_overview <- as.data.frame(udpipe_annotate(udmodel_rus, x = overview)) %>% 
    dplyr::select(lemma)
  lemmas_info <- movie %>% bind_cols(annotated_overview)
  review_lemmas <- review_lemmas %>% bind_rows(lemmas_info)
}

# удалить пунктуацию, все строчные
punctuation <- c(".", ",", "!", "?", "'", '"', "(", ")", " ", "-", ";", ":", "—", "…") 
review_words <- review_lemmas %>% filter(!lemma %in% punctuation)
review_words$lemma <- tolower(review_words$lemma)

# удалить стоп-слова
stop = stopwords("ru", source = "stopwords-iso")
stop_tbl <- as_tibble(stop)
colnames(stop_tbl) <- "lemma"

review_clean <- review_words %>% 
  anti_join(stop_tbl)

# матрица встречаемости
review_count <- review_clean %>% group_by(movie) %>% 
  count(lemma)

review_wide <- review_count %>% pivot_wider(names_from = movie, 
                              values_from = n, 
                              values_fill = 0)




  

