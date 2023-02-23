## In Lesson 2 Module 2, we examined most frequent bigrams
## We might also be interested in words that co-occur within documents,
## even if not next to each other
## https://bookdown.org/Maxine/tidy-text-mining/counting-and-correlating-pairs-of-words-with-widyr.html

load("/Users/olga/R_Workflow/Elements_Stylometry/data/AristotleTidy.Rdata")

library(stopwords)
stop = stopwords("grc", source = "ancient")
stop.tbl <- as_tibble(stop)
colnames(stop.tbl) <- "word"


Arist.section.words <- Aristotle.tidy %>% filter(book == "Metaphysics") %>%
  filter(!word %in% stop.tbl$word) %>% filter(!word == "NULL") %>% 
  mutate(section = (row_number() %/% 10) + 1 ) 
  

## pairwise count
library(widyr)
word_pairs <- Arist.section.words %>% pairwise_count(word, section, sort = T)
word_pairs %>% filter(item1 == "Πλάτων")

## pairwise correlation
words_cors <- Arist.section.words %>% group_by(word) %>% filter(n() >=20) %>%
  pairwise_cor(word, section, sort = T)

selected_words <- c("ἀρχή", "εἶδος", "δύναμις", "μονάς")

## barplot
library(ggplot2)
words_cors %>% filter(item1 %in% selected_words) %>% group_by(item1) %>% 
  top_n(6) %>% ungroup() %>% mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation, fill = item1)) +
  geom_bar(stat = "identity", show.legend = F) +
  facet_wrap(~item1, scales = "free") + coord_flip()
## For geom_bar(), the default behavior is to count the rows for each x value. 
## It doesn't expect a y-value, since it's going to count that up itself 
## How aggregation is to be performed is specified as an argument to geom_bar(), 
## which is stat = "count" for the default value. 
## If you explicitly say stat = "identity" in geom_bar(), you're telling ggplot2 
## to skip the aggregation and that you'll provide the y values. 

## network
library(ggraph)
library(igraph)
words_cors %>% filter(correlation > 0.3) %>% graph_from_data_frame() %>%
  ggraph(layout = "fr") + geom_edge_link(aes(edge_alpha = correlation), show.legend = F) + 
  geom_node_point(color = "salmon", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) + theme_void()

## colorblind safe color palette
words_cors %>% filter(item1 %in% selected_words) %>% group_by(item1) %>% 
  top_n(6) %>% ungroup() %>% mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation, fill = item1)) +
  geom_bar(stat = "identity", show.legend = F) +
  facet_wrap(~item1, scales = "free") + coord_flip() + 
  scale_fill_viridis(option = "B", discrete = T) ## in package ggraph
## https://cran.r-project.org/web/packages/viridis/viridis.pdf
## https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html

## color brewer
## https://ggplot2.tidyverse.org/reference/scale_brewer.html
words_cors %>% filter(item1 %in% selected_words) %>% group_by(item1) %>% 
 top_n(6) %>% ungroup() %>% mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation, fill = item1)) +
  geom_bar(stat = "identity", show.legend = F) +
  facet_wrap(~item1, scales = "free") + coord_flip() + 
  scale_fill_brewer(type = "div", palette = 1)