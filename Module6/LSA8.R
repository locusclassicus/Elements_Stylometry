library(tidyverse)
library(tidytext)

# теперь попробуем рассмотреть сами векторы

# применим svd
svd_review <- irlba(as.matrix(review_df), nv = 50, maxit = 1000)
dim(svd_review$u)

# преобразуем в опрятные данные
U <- svd_review$u
colnames(U) <- c(1:50)
U_tbl <- as_tibble(U) %>% mutate(lemma = lemmas, .before = `1`)

long_U <- U_tbl %>% 
  pivot_longer(-lemma, names_to = "topic", values_to = "value")

long_U %>% 
  group_by(topic) %>%
  filter(as.numeric(topic) < 10) %>% 
  top_n(15, abs(value)) %>% 
  ungroup() %>% 
  mutate(lemma = reorder_within(lemma, value, topic)) %>% 
  ggplot(aes(lemma, value, fill = topic)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~topic, scales = "free_y", ncol = 3) +
  scale_x_reordered() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Value",
    title = "9 главных компонент для рецензий Кинопоиска"
  )
  

  
  
  
  
  