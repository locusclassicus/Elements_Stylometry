# load model
library(udpipe)
# greek1 <- udpipe_download_model(language = "ancient_greek")
udmodel_greek1 <- udpipe_load_model(file = "ancient_greek-perseus-ud-2.5-191206.udpipe")

# vertor of all words
vec_greek <- greek_att %>% pull(greek)

# lemmatize
lemmas1 <- as.data.frame(udpipe_annotate(udmodel_greek1, x = vec_greek))

# try another model
# greek2 <- udpipe_download_model(language = "ancient_greek-proiel")
udmodel_greek2 <- udpipe_load_model(file = "ancient_greek-proiel-ud-2.5-191206.udpipe")

lemmas2 <- as.data.frame(udpipe_annotate(udmodel_greek2, x = vec_greek))
lemmas <- lemmas2[,c("lemma", "upos")]

# greek words
att_lemmas <- greek_att %>% bind_cols(lemmas)

# delete stop words
library(stopwords)
stop = stopwords("grc", source = "ancient")
stop.tbl <- as_tibble(stop)
colnames(stop.tbl) <- "lemma"

## delete stopwords
att_lemmas_no_sw <- att_lemmas %>% anti_join(stop.tbl)

# stats
word_list <- att_lemmas_no_sw %>% group_by(lemma) %>% 
  summarise(n = n())

word_list <- word_list %>% mutate(nchar = nchar(lemma)) %>% 
  filter(nchar > 3) %>% dplyr::select(-nchar)


# wordcloud
library(wordcloud2)
wordcloud2(word_list, size = 1, minSize = 0, gridSize =  0,
           fontFamily = 'Segoe UI', fontWeight = 'bold',
           color = 'random-dark', backgroundColor = "white",
           minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE,
           rotateRatio = 0.4, shape = 'circle', ellipticity = 0.65,
           widgetsize = NULL, figPath = NULL, hoverFunction = NULL)

# by books
att_lemmas %>% group_by(book_nr) %>% 
  summarise(n = n()) %>% arrange(as.numeric(book_nr))
