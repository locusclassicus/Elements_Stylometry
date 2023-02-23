library(dplyr)
load("/Users/olga/R_Workflow/Elements_Stylometry/data/PlatoWords.Rdata")
spuria <- c("Alcibiades1", "Alcibiades2", "Cleitophon", "Crito", "Epinomis",
            "Epistles", "Euthyphro", "Hipparchus", "HippiasMajor", "HippiasMinor",
             "Ion", "Lovers", "Menexenus", "Minos", "Theages")
Plato <- words.tbl %>% filter(!title %in% spuria)
my_words = "ὁ"
section.words.pl <- Plato %>% mutate(section = (row_number() %/% 100)+1) %>% 
  filter(word %in% my_words) %>% mutate(word = factor(word)) %>% 
  group_by(section, word, .drop = F) %>% count() %>% 
  ungroup() %>% select(-word)

load("/Users/olga/R_Workflow/Elements_Stylometry/data/AristotleTidy.Rdata")
section.words.ar <- Aristotle.tidy %>% mutate(section = (row_number() %/% 100)+1) %>% 
  filter(word %in% my_words) %>% mutate(word = factor(word)) %>% 
  group_by(section, word, .drop = F) %>% count() %>%
  ungroup() %>% select(-word)

## sample
set.seed(10)
pl.sample <- section.words.pl %>% sample_n(10) %>% select(n) %>% 
  mutate(author = "Plato")
ar.sample <- section.words.ar %>% sample_n(10) %>% select(n) %>%
  mutate(author = "Aristotle")
tbl <- bind_rows(pl.sample, ar.sample)
  
## rate
rate.tbl <- tbl %>% arrange(n) %>% mutate(row = row_number())

rate.tbl <- rate.tbl %>% group_by(n) %>% mutate(rank = mean(row)) %>% 
  ungroup()

## sum ranks
rate.tbl %>% group_by(author) %>% summarise(total = sum(rank))
# aristotle = 141, plato = 69

## u values
u1 = 10*10 + ((10*(10+1))/2) - 141 # 14 aristotle
u2 = 10*10 + ((10*(10+1))/2) - 69 # 69 plato

u <- min(u1, u2)

## ties 
ties <- rate.tbl %>% group_by(n) %>% summarise(ties = n()) %>% 
  filter(ties > 1) %>% pull(ties)

## sum T_i
vec_t <- c()
 for (i in ties) {
  ti <- (i^3 - i)/12
  vec_t <- c(vec_t, ti)
}
sum_t <- sum(vec_t)

# sigma

N = 20
n1 = 10
n2 =10

num = u - (n1*n2)/2

den1 = (n1*n2)/(N*(N-1))
den2 = ((N^3 - N) / 12) - sum_t
den = sqrt(den1 * den2)
z = num / den # -2.738905

# correction
correction <- sign(num) * 0.5 
zcorr <- (num - correction) / den # -2.700864


pnorm(z) * 2 ## 0.006164422
pnorm(zcorr) ## 0.006915952

# stats package
wilcox.test(pl.sample$n, ar.sample$n) # p = 0.006916

p = 0.006916
z <- qnorm(p/2) ## -2.700862

#  smaller samples, less power
wilcox.test(pl.sample$n[1:5], ar.sample$n[1:5])

# plot
bigsample.ar <- section.words.ar %>% sample_n(size = 3000) %>% mutate(author = "Aristotle")
bigsample.pl <- section.words.pl %>% sample_n(size = 3000) %>% mutate(author = "Plato")
big.tbl <- bind_rows(bigsample.ar, bigsample.pl) %>% select(-section)

means <- big.tbl %>% group_by(author) %>% summarize(means = mean(n)) %>% pull(means)

big.tbl %>% ggplot(aes(n, fill = author)) + geom_bar() + 
  geom_vline(xintercept = means[1], color = "grey39") + 
  geom_vline(xintercept = means[2], color = "grey39") +
  theme_minimal()

