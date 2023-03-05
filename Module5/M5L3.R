library(udpipe)

## save model
latin <- udpipe_download_model(language = "latin")

## load model
udmodel_latin <- udpipe_load_model(file = "latin-perseus-ud-2.5-191206.udpipe")

## annotate text
cic_lemmas <- c()

for (i in 1:nrow(cicero_att)) {
  letter <- cicero_att %>% slice(i) %>% pull(text)
  letter_info <- cicero_att %>% slice(i) %>% 
    dplyr::select(book_nr, letter_nr, date)
  annotated_letter <- as.data.frame(udpipe_annotate(udmodel_latin, x = letter)) %>% 
    dplyr::select(lemma)
  lemmas_info <- letter_info %>% bind_cols(annotated_letter)
  cic_lemmas <- cic_lemmas %>% bind_rows(lemmas_info)
}

