library(pwr)
pwr.t.test(d = .2,
           sig.level = 0.05,
           power = 0.8)



load("/Users/olga/R_Workflow/Elements_Stylometry/data/PlatoWords.Rdata")
load("/Users/olga/R_Workflow/Elements_Stylometry/data/AristotleTidy.Rdata")
library(dplyr)
unique(words.tbl$title)
spuria <- c("Alcibiades1", "Alcibiades2", "Cleitophon", "Crito", "Epinomis",
            "Epistles", "Euthyphro", "Hipparchus", "HippiasMajor", "HippiasMinor",
            "Ion", "Lovers", "Menexenus", "Minos", "Theages")
Plato <- words.tbl %>% filter(!title %in% spuria)

my_word <- "ὁ"

section.words.pl <- Plato %>% mutate(section = (row_number() %/% 100)+1) %>% 
  filter(word %in% my_word) %>% mutate(word = factor(word)) %>% 
  group_by(section, word, .drop = F) %>% count() %>% 
  mutate(author = "Plato") %>% ungroup()

section.words.ar <- Aristotle.tidy %>% mutate(section = (row_number() %/% 100)+1) %>% 
  filter(word %in% my_word) %>% mutate(word = factor(word)) %>% 
  group_by(section, word, .drop = F) %>% count() %>% 
  mutate(author = "Aristotle") %>% ungroup()

## sample 10 sections 100 words each
set.seed(11)
sample.ar <- section.words.ar %>% select(-section, -word) %>% 
  sample_n(size = 5)
sample.pl <- section.words.pl %>% select(-section, -word) %>% 
  sample_n(size = 5)
t.test(sample.ar$n, sample.pl$n)

## effect size
library(lsr)
both_groups <- bind_rows(sample.ar, sample.pl)
d <- cohensD(n ~ author,
        data = both_groups)
# d = 1.3
num <- mean(sample.ar$n) - mean(sample.pl$n) 
den <- sqrt((sd(sample.ar$n)^2 + sd(sample.pl$n)^2)/2) ## pooled sd
num/den # d = 1.52

pwr.t.test(n = 5, sig.level = 0.05, d = d)
pwr.t.test(power = 0.8, sig.level = 0.05, d = d)


## boots 
bootP <- function(x, boots = 100) {
  n <- nrow(x)
  replicate(boots, {
    ind <- sample.int(n, n, replace=T)
    mean(x$n[ind])
  })
}

mean.boot.pl <- bootP(sample.pl, 100)
mean.boot.ar <- bootP(sample.ar, 100)


## chisq.test
load("/Users/olga/R_Workflow/Elements_Stylometry/data/DialogueType.Rdata")
dialogues <- dialogues[,-1 ]
mx <- table(dialogues)

res <- chisq.test(mx, correct = F)
obs <- res$observed / (5+11+1+8)

## effect size
ES.w2(obs) ## 0.2263416
cohen.ES(test = "chisq", size = "small")
cohen.ES(test = "chisq", size = "medium")

pwr.chisq.test(w=ES.w2(obs), df=1, N = 25)
pwr.chi <- pwr.chisq.test(w=ES.w2(obs), df=1, power = 0.8)
plot(pwr.chi)

## https://rpsychologist.com/cohend/

