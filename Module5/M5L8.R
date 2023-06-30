# continued from lesson 7
# stat_polarity$letter_nr <- gsub("A", ".1", stat_polarity$letter_nr)
# stat_polarity$letter_nr <- gsub("B", ".2", stat_polarity$letter_nr)
# stat_polarity$letter_nr <- gsub("C", ".3", stat_polarity$letter_nr)
# stat_polarity$letter_nr <- gsub("D", ".4", stat_polarity$letter_nr)
# save(stat_polarity, file = "Cicero_StatPolarity.Rdata")
library(dplyr)
load("/Users/olga/R_Workflow/Elements_Stylometry/data/Cicero_StatPolarity.Rdata")

stat_polarity <- stat_polarity %>% mutate(book_nr = as.numeric(book_nr),
                        letter_nr = as.numeric(letter_nr))

polarity_greek <- stat_polarity %>% 
  left_join(greek_att_sum, by = c("book_nr", "letter_nr"))

# impute nas
idx <- is.na(polarity_greek$total_greek)
polarity_greek$total_greek[idx] <- 0

# absolute emotions value
emo_greek <- polarity_greek %>% 
  group_by(book_nr, letter_nr) %>% 
  mutate(abs_emo = sum(abs(total)))

# weighted values 
emo_greek_w <- emo_greek %>% 
  left_join(cicero_letter_length, by = c("book_nr", "letter_nr")) %>% 
  mutate(emo_w = abs_emo/n, gr_w = total_greek/n) %>% 
  dplyr::select(-has_polarity, -total, -total_greek, -n, -abs_emo) %>% 
  distinct()

emo_greek_w %>% filter(! gr_w == 0) %>% 
  ggplot(aes(emo_w, gr_w, color = as.factor(date))) + 
  geom_point(size = 3, alpha = 0.5)

