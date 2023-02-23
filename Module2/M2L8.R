## tydying dtm objects
library(tidytext)
cicero_td <- tidy(cicero_dtm)
library(dplyr)
total <- cicero_td %>% group_by(document) %>% summarise(total = n())
frequency <- cicero_td %>% left_join(total, by = "document") %>% 
  mutate(freq = count/total)

## spread
library(tidyr)
frequency <- frequency %>% select(document, term, freq) %>%
  spread(document, freq, fill = 0) %>% arrange(CiceroND.txt)

## plot
library(scales)
library(ggplot2)
ggplot(frequency, aes(CiceroND.txt, CiceroOff.txt)) + 
  geom_jitter(alpha = 0.1, size =2.5, width = 0.25, height = 0.25, color = "blue") +
  geom_text(aes(label = term), check_overlap = T, vjust = 1.5) +
  scale_x_log10(labels = label_percent()) + scale_y_log10(labels= label_percent()) +
  geom_abline(color = "magenta")
