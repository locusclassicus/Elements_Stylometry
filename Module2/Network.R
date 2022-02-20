## networks
library(stylo)
Phlb.1n <- load.corpus.and.parse(files = "Philebus.txt", corpus.dir = getwd(), markup.type= "plain",
                                              corpus.lang = "Other", features = "w", 
                                              ngram.size = 1, preserve.case = FALSE,
                                              encoding = "UTF-8")
Phlb.v1 <- unlist(Phlb.1n)
nulls <- is.null(Phlb.v1)
Phlb.clean <- Phlb.v1[!nulls]

## stopwords
library(stopwords)
grc_sw = stopwords("grc", source = "ancient")
grc_sw = grc_sw[275:length(grc_sw)]


## delete sw
Phlb.sw <- Phlb.clean %in% grc_sw
Phlb.no.sw <- Phlb.clean[!Phlb.sw]
Phlb.2n <- make.ngrams(Phlb.no.sw, ngram.size = 2)
Phlb.2n.tbl <- tibble(text = Phlb.2n)
tidy.count = Phlb.2n.tbl %>% count(text, sort = TRUE) 

library(igraph)

bigram_graph <- tidy.count %>%
  filter(n > 5) %>%
  graph_from_data_frame()

library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)



a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
