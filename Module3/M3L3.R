## 
comb <- function(n,k) {
  factorial(n) / (factorial(k)*factorial(n-k))
}

## What is the probability of 2 heads in 10 coin flips where 
## probability of heads is 0.3?
library(dplyr)
library(ggplot2)
library(scales)

data.frame(heads = 0:10, prob = dbinom(x = 0:10, size = 10, prob = 0.3)) %>%
  mutate(Heads = ifelse(heads == 2, "2", "other")) %>%
  ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
  geom_col() +
  geom_text(
    aes(label = round(prob,2), y = prob + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) +
  labs(title = "Probability of X = 2 successes.",
       subtitle = "b(10, .3)",
       x = "Successes (x)",
       y = "probability") 

## cumulative 
dbinom(0, 3, 0.5) + dbinom(1, 3, 0.5) + dbinom(2, 3, 0.5)

library(dplyr)
library(ggplot2)

## What is the probability of >=5 heads in 10 coin flips where probability of heads is 0.5?
data.frame(heads = 0:10, 
           pmf = dbinom(x = 0:10, size = 10, prob = 0.5),
           cdf = pbinom(q = 0:10, size = 10, prob = 0.5, 
                        lower.tail = TRUE)) %>%
  mutate(Heads = ifelse(heads <= 5, "<=5", "other")) %>%
  ggplot(aes(x = factor(heads), y = cdf, fill = Heads)) +
  geom_col() +
  geom_text(
    aes(label = round(cdf,2), y = cdf + 0.01),
    size = 3,
    vjust = 0
  ) +
  labs(title = "Probability of X <= 5 successes.",
       subtitle = "b(10, .5)",
       x = "Successes (x)",
       y = "probability") 

###
qbinom(0.62, 10, 0.5)


## prepare tibble
library(XML)
filenames <- list.files(path = "./PlatoDiorisisXML")
words.tbl <- tibble()
for (i in filenames){
  url = paste("./PlatoDiorisisXML/", i, sep = "")
  doc <- xmlTreeParse(url, useInternalNodes = TRUE, isURL = F)
  rootnode  <- xmlRoot(doc)
  text <- xpathSApply(rootnode, "//TEI.2/text/body/sentence/word/lemma", xmlGetAttr, 'entry')
  tbl <- tibble(title = gsub(".xml","", i), word = unlist(text, use.names = F))
  words.tbl <- words.tbl %>% bind_rows(tbl)
}

##count frequencies
word.counts <- words.tbl %>% group_by(word) %>% count() %>% ungroup()
total <- word.counts %>% summarize(total = sum(n))
my_words = c("καί", "ὁδός", "φημί")
my_words_counts <- word.counts %>% mutate(total = total$total) %>% 
  filter(word %in% my_words )%>% mutate(rtf = n/total)

## theoretical distributions
set.seed(1)
binomial_data_kai <- rbinom(500, 1000, 0.0543)
data_kai <- tibble(word = "καί", data = binomial_data_kai) 
binomial_data_phemi <- rbinom(500, 1000, 0.00743)
data_phemi <- tibble(word = "φημί", data = binomial_data_phemi) 
binomial_data_hodos <- rbinom(500, 1000, 0.000185)
data_hodos <- tibble(word = "ὁδός", data = binomial_data_hodos)
binomial_data <- bind_rows(data_kai, data_phemi, data_hodos)

binomial_data %>% ggplot(aes(data, fill = factor(word))) + 
  geom_histogram(color = "grey") + facet_wrap(~word, scales = "free")

## observed distributions
section.words <- words.tbl %>% mutate(section = (row_number() %/% 1000)+1) %>% 
  filter(word %in% my_words) %>% mutate(word = factor(word)) %>% 
  group_by(section, word, .drop = F) %>% count() 
  
section.words %>% ggplot(aes(x = n, fill = word)) + 
  geom_histogram(color="grey") + facet_wrap(~word, scales = "free")

## compare observed and theoretical 
sw.kai <- section.words %>% filter(word == "καί")
sw.phemi <- section.words %>% filter(word == "φημί")
sw.hodos <- section.words %>% filter(word == "ὁδός")

par(mfrow = c(1, 3))
qqplot(data_kai$data, sw.kai$n)
qqplot(data_phemi$data, sw.phemi$n)
qqplot(data_hodos$data, sw.hodos$n)

## poisson 
## theoretical distributions
set.seed(1)
poisson_data_kai <- rpois(1000, 54.3) ## 1000 * 0.0543
poisson_data_kai <- tibble(word = "καί", data =poisson_data_kai) 
poisson_data_phemi <- rpois(1000, 7.43) ## 1000 * 0.00743
poisson_data_phemi <- tibble(word = "φημί", data =poisson_data_phemi) 
poisson_data_hodos <- rpois(1000, 0.18) ## 1000 * 0.00018
poisson_data_hodos <- tibble(word = "ὁδός", data =poisson_data_hodos)
poisson_data <- bind_rows(poisson_data_kai, poisson_data_phemi, poisson_data_hodos)

poisson_data %>% ggplot(aes(data, fill = factor(word))) + 
  geom_histogram(color = "grey") + facet_wrap(~word, scales = "free")

## qqplot
par(mfrow = c(1, 3))
qqplot(poisson_data_kai$data, sw.kai$n, xlab = "theoretical", ylab = "observed", main = "καί")
qqplot(poisson_data_phemi$data, sw.phemi$n, xlab = "theoretical", ylab = "observed", main = "φημί")
qqplot(poisson_data_hodos$data, sw.hodos$n, xlab = "theoretical", ylab = "observed", main = "ὁδός")

