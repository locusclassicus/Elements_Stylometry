library(gridExtra)
library(ggplot2)

## https://github.com/dmutti/datasciencecoursera/blob/master/statistical-inference/swirl-09-t-confidence-intervals.md
k <- 1000
xvals <- seq(-4, 4, length = k)
## plot 1
d <- data.frame(y = c(dnorm(xvals), dt(xvals, 2)),
                  x = xvals,
                  dist = factor(rep(c("Normal", "T"), c(k,k))))
p1 <- ggplot(d, aes(x = x, y = y)) + geom_line(aes(colour = dist), show.legend = F) +
   xlab("t") + ylab("density") + ggtitle("df=2") + theme_classic()

## plot 2
d <- data.frame(y = c(dnorm(xvals), dt(xvals, 5)),
                x = xvals,
                dist = factor(rep(c("Normal", "T"), c(k,k))))
p2 <- ggplot(d, aes(x = x, y = y)) + geom_line(aes(colour = dist), show.legend = F) +
  xlab("t") + ylab("density") + ggtitle("df=5") + theme_classic()

grid.arrange(p1, p2, nrow = 1)

## example
library(dplyr)
library(MASS)
load("/Users/olga/R_Workflow/Elements_Stylometry/data/PlatoWords.Rdata")

## delete spuria 
unique(words.tbl$title)
spuria <- c("Alcibiades1", "Alcibiades2", "Cleitophon", "Crito", "Epinomis",
            "Epistles", "Euthyphro", "Hipparchus", "HippiasMajor", "HippiasMinor",
            "Ion", "Lovers", "Menexenus", "Minos", "Theages")
Plato <- words.tbl %>% filter(!title %in% spuria)

my_words = "ὁ"
section.words.pl <- Plato %>% mutate(section = (row_number() %/% 100)+1) %>% 
  filter(word %in% my_words) %>% mutate(word = factor(word)) %>% 
  group_by(section, word, .drop = F) %>% count() %>% mutate(author = "Plato")

truehist(section.words$n)

## sample
sample.count <- function(x) {
  my_counts <- c()
  for (i in 1:x){
  my_sample <- Plato %>% sample_n(100) %>% filter(word %in% my_words) 
  n <- my_sample %>% summarize(n = n())
  my_counts <- c(my_counts, as.numeric(n))
  }
  my_counts
}

set.seed(10)
counts <- sample.count(5)

## t-statistic
mu = mean(section.words.pl$n) ## 9.37
xbar = mean(counts) # 9.2
s = sd(counts)

t = (xbar - mu) / (s / sqrt(5)) ## 0.96

2*(1-pt(abs(t), 4)) ## > alpha => not significant


## conf interv

tstat <- qt(0.025, 4) # -2.776445
xbar + c(-1,1) * t * (s / sqrt(5))

## t-test 
t.test(counts, mu= mu)

## article in Aristotle
my_words = "ὁ"
section.words.ar <- Aristotle.tidy %>% mutate(section = (row_number() %/% 100)+1) %>% 
  filter(word %in% my_words) %>% mutate(word = factor(word)) %>% 
  group_by(section, word, .drop = F) %>% count() 

truehist(section.words$n)


## two-sample t-test
my_words <- "ὁ"

set.seed(10)
Arist.counts <- c()
for (i in 1:5) {
  n <- Aristotle.tidy %>% sample_n(100) %>% 
    filter(word %in% my_words) %>% summarize(n = n()) %>% as.numeric()
  Arist.counts <- c(Arist.counts, n)
}
  
set.seed(10)
Plato.counts <- c()
for (i in 1:5) {
  n <- Plato %>% sample_n(100) %>% 
    filter(word %in% my_words) %>% summarize(n = n()) %>% as.numeric()
  Plato.counts <- c(Plato.counts, n)
}
    

## t-test

num = mean(Arist.counts) - mean(Plato.counts)
den = (((sd(Arist.counts))^2)/5) + (((sd(Plato.counts))^2)/5)
t = num/ sqrt(den)

t.test(Arist.counts, Plato.counts)

## density plot
article.df <- bind_rows(section.words.ar, section.words.pl)

qplot(n, data = article.df, colour = author, geom = "density")









