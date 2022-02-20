## stopwords
library(stopwords)
stopwords_getsources()

stopwords_getlanguages("ancient")
stopwords_getlanguages("perseus")

grc_sw = stopwords("grc", source = "ancient")[275:length(grc:sw)]
tbl.sw = tibble(text = grc_sw)

grc_ps = stopwords("grc", source = "perseus")

### topic modelling
library(dplyr)
Apol = my_corpus[[2]]
text_df <- tibble(text = Apol)

### delete stopwords
tidy_df <- text_df %>% anti_join(tbl.sw)
tidy.count = tidy_df %>% count(text, sort = TRUE) 

## plot 
library(ggplot2)
tidy_df %>%
  count(text, sort = TRUE) %>%
  filter(n > 20) %>%
  mutate(text = reorder(text, n)) %>%
  ggplot(aes(n, text)) +
  geom_col() +
  labs(y = NULL)

## wordcloud
install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("wordcloud2")
library(wordcloud2)

## dtm
library(tm)
Apol.sw <- Apol %in% grc_sw
Apol.no.sw <- Apol[!Apol.sw]
doc <- Corpus(VectorSource(Apol.no.sw))
dtm <- TermDocumentMatrix(doc) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

set.seed(1234)
par(mfrow = с(10, 10, 10, 10))
wordcloud(words = df$word, freq = df$freq, min.freq=2,
          random.order=FALSE, max.words=100, 
          colors=brewer.pal(8, "Dark2"))

wordcloud2(data=df, size=1, color='random-dark', shape = "circle")

