## visualize bigrams in Augustine with ggplot and make a network

## we start with the same Confessions df we saved earlier
load("/Users/olga/R_Workflow/Elements_Stylometry/data/ConfessionsDF.Rdata")
rownames(df) <- c()

## but this time split into n-grams
library(tidytext)
library(dplyr)
conf.books <- df %>% unnest_tokens(bigram, text, token = "ngrams", n=2)

## compare 
## x <- as.tibble(c("Now is the hour of our discontent"))
## unnest_tokens(x, output = output, value, token="character_shingles", n = 4)

## load stopwords 
library(stopwords)
stop = stopwords("la", source = "ancient")
stop.tbl <- as_tibble(stop)
colnames(stop.tbl) <- "word"

## separate bigrams
library(tidyr)
bigrams_separated <- conf.books %>% separate(bigram, c("word1", "word2"), sep = " ")

## filter bigrams
bigrams_filtered <- bigrams_separated %>% 
                    filter(!word1 %in% stop.tbl$word) %>%
                    filter(!word2 %in% stop.tbl$word)

## remove NAs caused by blank lines or other tokenisation issues
bigrams_clean <- bigrams_filtered %>%
  filter(!is.na(word1)) %>% 
  filter(!is.na(word2))

## count bigrams
bigram_count <- bigrams_clean %>% count(word1, word2, sort = T)

## unite bigrams
bigrams_united <- bigrams_clean %>% unite(bigram, word1, word2, sep = " ")

## select most frequent
bigrams_selected <- bigrams_united %>% rename(book = b.nr) %>% group_by(book) %>% 
  count(bigram, sort = T) %>% arrange(desc(n), .by_group = T ) %>% 
  slice_head(n = 10) %>% filter(book %in% c("1", "2", "11", "12")) %>%
  mutate(book = factor(book, levels = c("1", "2", "11", "12"))) 

## plot as earlier
library(ggplot2)
p <- ggplot(data = bigrams_selected, aes(x = reorder_within(bigram, n, book), 
                                    y = n, fill = book)) 

p +  geom_col() +  coord_flip() + scale_x_reordered() +
  facet_wrap(facets = ~book, nrow = 2, scales = "free") + 
  labs(title = "Confessions: Bigram Frequencies by Books",
       y = "nr. of words",
       x = NULL)

## or make a network
library(igraph)
bigram_graph <- bigram_count %>% filter(n > 3) %>% graph_from_data_frame()
library(ggraph)
ggraph(bigram_graph, layout = "fr") + geom_edge_link(aes(edge_alpha = n), 
                                                     arrow = grid::arrow(type = "closed", length = unit(2, "mm")), 
                                                     end_cap = circle(2, "mm")) +
  geom_node_point(color= "lightblue", size = 5) + 
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) + theme_void()
## read more on layouts: https://www.data-imaginist.com/2017/ggraph-introduction-layouts/ 
## https://cran.r-project.org/web/packages/ggraph/vignettes/Layouts.html



