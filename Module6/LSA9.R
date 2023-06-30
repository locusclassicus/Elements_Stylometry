# убедившись, что все работает попробуем применить LSA к корпусу Платона
# опираемся на книгу https://smltar.com/ 
# желательно вспомнить Module 2 Lesson 5 (скользящие окна)
# Module 2 Lesson 6 о том, что такое PMI

library(stylo)
library(tidyverse)
library(ggplot2)
library(stopwords)
library(slider)
library(widyr)
library(furrr)
library(tidytext)
library(purrr)
library(tictoc)

# read files
corpus <- load.corpus.and.parse(corpus.dir = "Diorisis_Plato_lemma")
corpus <- as_tibble(stack(corpus)) %>% 
  relocate(values, .after = ind) %>% 
  rename(word = values, title = ind)

# no need to lemmatize: we have extracted lemmas from xmls

# select most common words
corpus <- corpus %>% 
  add_count(word) %>% 
  filter(n > 20) %>% 
  select(-n)

# delete stopwords
stop = stopwords("grc", source = "ancient")
stop_tbl <- as_tibble(stop) %>% mutate(word = value)
corpus <- corpus %>% anti_join(stop_tbl, by = "word") %>% filter(word != "null")

# nest words
corpus_nest <- corpus %>% nest(words = c(word))

# slide window function 
slide_windows <- function(tbl, window_size) {
  skipgrams <- slider::slide(tbl, 
                             ~.x, 
                             .after = window_size - 1, 
                             .step = 1, 
                             .complete = TRUE)
  
  
  safe_mutate <- safely(mutate)
  
  out <- map2(skipgrams, 1:length(skipgrams), 
              ~ safe_mutate(.x, window_id = .y))
  
  out %>% transpose() %>% pluck("result") %>% compact() %>% bind_rows()
}

# before planning a parallel session, it's useful to revise map functions
# see H. Wickham & G. Grolemund "R for Data Science" p. 325 ff. 

# this function would take too long! 
tic()
tidy_pmi <- corpus_nest %>% 
  mutate(words = map(words, slide_windows, 10L)) %>% 
  unnest(words) %>% unite(window_id, title, window_id) %>% 
  pairwise_pmi(word, window_id)
toc() 

# parallel session
plan(multisession)
tic()
tidy_pmi <- corpus_nest %>% 
  mutate(words = future_map(words, slide_windows, 10L)) %>% 
  unnest(words) %>% unite(window_id, title, window_id) %>% 
  pairwise_pmi(word, window_id)
toc() 

# have a look at tidy_pmi
tidy_pmi

# svd 
tidy_word_vectors <- tidy_pmi %>% widely_svd(item1, item2, pmi,
                                             nv = 50, maxit = 1000)
tidy_word_vectors

# plot main topics
tidy_word_vectors %>% 
  filter(dimension <= 9) %>%
  group_by(dimension) %>% 
  top_n(10, abs(value)) %>% 
  ungroup() %>% 
  mutate(item1 = reorder_within(item1, value, dimension)) %>% 
  ggplot(aes(item1, value, fill = as.factor(dimension))) +
  geom_col(alpha = 0.8,  show.legend = F) +
  facet_wrap(~dimension, scales = "free_y", ncol = 3) +
  scale_x_reordered() +
  coord_flip() +
  labs(
    x = NULL, 
    y = "Value",
    title = "First 9 principal components for the Platonic Corpus",
    subtitle = paste("Top words contributing to the components that explain",
                     "the most variation")
  )

# nearest neighbors
nearest_neighbors <- function(df, token) {
  df %>% widely(
    ~ {
      y <- .[rep(token, nrow(.)), ]
      res <- rowSums(. * y) /
        (sqrt(rowSums(. ^ 2)) * sqrt(sum(.[token, ] ^ 2)))
      
      matrix(res, ncol = 1, dimnames = list(x = names(res)))
    },
    sort = TRUE
  ) (item1, dimension, value) %>% 
    select(-item2)
}

# find word neighbors
stoicheion <- tidy_word_vectors %>% nearest_neighbors("στοιχεῖον")
stoicheion


