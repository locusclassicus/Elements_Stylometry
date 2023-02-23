## continuing from M2L5
## calculate point-wise mutual information (PMI)
## PMI is a measure of association = log of the probability of 
## finding two words together, normalized for the probability of finding each 
## of them alone
library(widyr)

dat <- tibble(group = rep(1:5, each = 2),
              letter = c("a", "b",
                         "a", "c",
                         "a", "c",
                         "b", "e",
                         "b", "f"))
pairwise_pmi(dat, letter, group, sort = TRUE)

## apply function
## the "L" suffix  denotes an integer
tidy_pmi <- nested_words %>% 
  mutate(words = map(words, slide_windows, 4L)) %>% 
  unnest(words) %>% unite(window_id, book, window_id) %>% select(-author) %>%
  pairwise_pmi(word, window_id)
## unite() is a function from tidyr that pastes multiple columns into one

tidy_pmi

tidy_pmi %>% filter(item1 == "μονάς") %>% filter(pmi > 1) %>% arrange(-pmi)

tidy_cor <- nested_words %>% 
  mutate(words = map(words, slide_windows, 4L)) %>% 
  unnest(words) %>% unite(window_id, book, window_id) %>% select(-author) %>%
  pairwise_cor(word, window_id)
