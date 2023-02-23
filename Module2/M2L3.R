## counting tf and tf-idf 
## in Lesson 10 Module 1, we also downloaded and parsed 
## the whole Corpus Aristotelicum

## so we read in using tidy tools
list_of_files <- list.files(path = "./Aristotle", recursive = TRUE,
                            pattern = "\\.txt$", 
                            full.names = TRUE)
library(vroom)
Aristotle.df <- vroom(list_of_files, id = "FileName", delim = "\n", 
                      col_names = F)
## clean up id's 
library(stringr)
Aristotle.df$FileName <- str_remove_all(Aristotle.df$FileName, "./Aristotle/")
Aristotle.df$FileName <- str_remove_all(Aristotle.df$FileName, ".txt")
Aristotle.tidy <- Aristotle.df %>% separate(FileName, c("author", "book"), sep = "_")
Aristotle.tidy <- rename(Aristotle.tidy, word = X1)
save(Aristotle.tidy, file = "./data/AristotleTidy.Rdata")

## term frequency
Aristotle.words <- Aristotle.tidy %>% count(book, word, sort = TRUE) %>% filter(word != "NULL")

## total words 
total_words <- Aristotle.words %>% group_by(book) %>% summarize(total = sum(n))

## bind
Aristotle.words <- left_join(Aristotle.words, total_words)

## plot
library(ggplot2)
ggplot(Aristotle.words, aes(n/total, fill = book)) + 
  geom_histogram(show.legend = F) + xlim(NA, 0.0009) +  
  facet_wrap(~book, ncol=2, scales = "free_y")

## tf_idf
Aristotle.words <- Aristotle.words %>% bind_tf_idf(word, book, n)
## save it for the next lesson
save(Aristotle.words, file = "./data/AristotleTF_IDF.Rdata")

## plot tf-idf
Aristotle.words %>% select(-total) %>% arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(book) %>% top_n(15) %>% ungroup() %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = F) + labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") + coord_flip()
