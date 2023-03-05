# stats by words

cic_stats_words <- cic_joined %>% group_by(lemma, has_polarity) %>% 
  summarise(n = n()) %>% ungroup()

# 10 most common negative and positive words in Cicero

cic_stats_words %>% group_by(has_polarity) %>% 
  top_n(15) %>% ungroup() %>% 
  mutate(lemma = reorder(lemma,n)) %>% 
  ggplot(aes(lemma, n, fill = has_polarity)) +
  geom_col(show.legend = F) +
  facet_wrap(~has_polarity, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()
  
# another way to do it
cic_stats_words %>% group_by(has_polarity) %>% 
  top_n(10) %>% ungroup() %>% 
  mutate(n = ifelse(has_polarity == "negative", -n, n)) %>% ## here
  mutate(lemma = reorder(lemma,n)) %>% 
  ggplot(aes(lemma, n, fill = has_polarity)) +
  geom_col(show.legend = F) +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()
  




