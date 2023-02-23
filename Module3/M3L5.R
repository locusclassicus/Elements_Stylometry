library(dplyr)
load("/Users/olga/R_Workflow/Elements_Stylometry/data/SentL.Rdata")
Rep <- sent.length %>% filter(title == "Republic")
mu <- mean(Rep$sent.l)
sigma <- sd(Rep$sent.l)

## Laws
Laws <- sent.length %>% filter(title == "Laws")
set.seed(123)
Laws.sample <- sample(Laws$sent.l, 1000)
xbar <- mean(Laws.sample)
sem <- sigma /sqrt(1000)
z <- (xbar - mu) / sem

## p-value
2*pnorm(q=z, lower.tail=FALSE)
pnorm(q=z, lower.tail=T)
pnorm(q=z, lower.tail=F)

## Charmides
Chrm <- sent.length %>% filter(title == "Charmides")
set.seed(123)
Chrm.sample <- sample(Chrm$sent.l, 100)
xbar <- mean(Chrm.sample)
sem <- sigma /sqrt(100)
z <- (xbar - mu) / sem

## p-value
2*pnorm(q=z, lower.tail=T)


