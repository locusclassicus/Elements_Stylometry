x <- seq(-3, 3, 1)[-4]
y <- dnorm(x)

## properties of the normal distribution
library(ggplot2)
x <- seq(-3, 3, 1)[-4]
y <- dnorm(x)
ggplot(NULL, aes(c(-3,3))) +
  geom_area(stat = "function", fun = dnorm, fill = "#377eb8", xlim = c(-3, -1), alpha = 0.3) +
  geom_area(stat = "function", fun = dnorm, fill = "#e41a1c", xlim = c(-1, 1), alpha = 0.3) +
  geom_area(stat = "function", fun = dnorm, fill = "#377eb8", xlim = c(1, 3), alpha = 0.3) +
  labs(x="z", y="") + scale_x_continuous(breaks = -3:3) + scale_y_continuous() +
  geom_segment(aes(x = x, y = 0, xend = x, yend = y), colour = "#999999", linetype = 2, size = 1)

## normal distributions with different means and sds
library(tidyr)
tbl <- tibble(x = rnorm(1000, mean = 3, sd = 4), y = rnorm(1000, mean = 1, sd = 1.5), z = rnorm(1000, mean = -2.5, sd = 2.5))
tbl %>% gather("type", "value", x:z) %>% ggplot(aes(value)) + 
  geom_density(aes(fill = type), linetype = 2, alpha = 0.5)

## some math
f <- function(x) {
  (1/(1*sqrt(2 * pi))) * (exp(1)^(-((x - 0)^2) / (2 * 1^2)))
  }
f(1) = dnorm(1)

## pnorm

