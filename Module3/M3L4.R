mean(sent.length$sent.l)
hist(sent.length$sent.l)
sl <- sent.length$sent.l

## the bigger is n, the less is variance of sample means
sample.means <- function(x, n) {
  my_means <- c()
  for(i in 1:x) {
    my_sample <- sent.length %>% sample_n(n)
    my_mean <- mean(my_sample$sent.l)
    my_means <- c(my_means, my_mean)
  }
  my_var <- round(var(my_means), 2)
  hist(my_means, xlab = paste0("sample size ", n), main = paste0("var = ", my_var))
}

set.seed(4)
sample.means(1000, 10)
sample.means(1000, 100)
sample.means(1000, 1000)

## sample variance vs. population variance
var(sl) / 10
var(sl) / 100
var(sl) / 1000

## sample sd vs. population sd
sd(sl) / sqrt(10)
sd(sl) / sqrt(100)
sd(sl) / sqrt(1000)

## confidence interval 
sample10 <- sample(sl, 10, replace = FALSE)
sample100 <- sample(sl, 100, replace = FALSE)
sample1000 <- sample(sl, 1000, replace = FALSE)

mean(sample10)
mean(sample100)
mean(sample1000)

library(ggplot2)
library('gridExtra')

dat = data.frame(
  score = c(sample10, sample100, sample1000),
  group = rep(c('10', '100', '1000'), c(10, 100, 1000))
)

## dynamite plot
ggplot(dat) +
  aes(x = group, y = score, fill = group) +
  stat_summary(geom = "bar", fun = "mean") +
  stat_summary(geom = "errorbar", fun.data = "mean_se", fun.args = list(mult = 2), width = .2)

mean_se(sample10)
sem = sd(sample10) / sqrt(10)
mean(sample10) + 2*sem
mean(sample10) - 2*sem

## plotting 95% CI
my_sem <- tibble()
  for(i in 1:20) {
    my_sample <- sample(sl, 1000)
    my_dat <- mean_se(my_sample, mult = 2)
    my_sem <- bind_rows(my_sem, my_dat)
  }
my_sem <- my_sem %>% mutate(x = row_number(), .before = y)

ggplot(my_sem, aes(x, y)) + 
  geom_point(color = "red") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), color = "#636363") + 
  geom_hline(yintercept = mean(sl))


## two groups
Rep <- sent.length %>% filter(title == "Republic")
Laws <- sent.length %>% filter(title == "Laws")

Rep1000 <- sample(Rep$sent.l, 1000)
Laws1000 <- sample(Laws$sent.l, 1000)

dat2 = data.frame(
  score = c(Rep1000, Laws1000),
  group = rep(c('Rep', 'Laws'), c(1000, 1000))
)

ggplot(dat2) +
  aes(x = group, y = score, fill = group) +
  stat_summary(geom = "bar", fun = "mean") +
  stat_summary(geom = "errorbar", fun.data = "mean_se", fun.args = list(mult = 2), width = .2)
