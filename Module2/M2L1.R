## visualize word frequencies for content words in Augustine book-wise
## with ggplot and make a word-cloud

## in Lesson 8 Module 1, we made a df with Augustine's Confessions
## let's load it now
load("/Users/olga/R_Workflow/Elements_Stylometry/data/ConfessionsDF.Rdata")

## get rid of rownames
rownames(df) <- c()

##  split text
library(dplyr)
library(tidytext)
conf.books <- df %>% unnest_tokens(word, text)

## load stopwords
library(stopwords)
stop = stopwords("la", source = "ancient")
stop.tbl <- as_tibble(stop)
colnames(stop.tbl) <- "word"

## delete stopwords
conf.content <- conf.books %>% anti_join(stop.tbl)

## count all
conf.count.all <- conf.content %>% count(word, sort = T)

## plot all
library(ggplot2)
conf.count.all %>% filter(n > 49) %>% 
    ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()

## same for each book
conf.count.book <- conf.content %>% group_by(b.nr) %>% 
  count(word, sort = T) 

conf.filtered <- conf.count.book %>%
  filter(b.nr %in% c("1", "2", "11", "12")) %>% 
  mutate(b.nr = factor(b.nr, levels = c("1", "2", "11", "12"))) 

conf.sliced <- conf.filtered %>% arrange(desc(n), .by_group = T ) %>%
  slice_head(n = 10) %>% rename(book = b.nr)
## some tips https://www.qiushiyan.dev/post/reorder-varible-within-facets/

p <- ggplot(data = conf.sliced, aes(x = reorder_within(word, n, book), 
                                    y = n, fill = book)) 

p +  geom_col() +  coord_flip() + scale_x_reordered() +
  facet_wrap(facets = ~book, nrow = 2, scales = "free") + 
  labs(title = "Confessions: Word Frequencies by Books",
       y = "nr. of words",
       x = NULL)


## wordcloud
library(wordcloud)
wordcloud(words = conf.count.all$word, freq = conf.count.all$n, 
          min.freq = 50, max.words = 100, random.order = TRUE, 
          rot.per = .15, vfont=c("script","plain"))

wordcloud(words = conf.count.all$word, freq = conf.count.all$n, 
          min.freq = 50, max.words = 100, random.order = TRUE, 
          rot.per = .15, vfont=c("serif","plain"))
