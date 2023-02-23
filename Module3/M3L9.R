library(sjPlot)
library(ggplot2)
library(dplyr)
library(tidyr)

data.frame(f = 0:1000 / 100) %>% 
  mutate(df_10_20 = df(x = f, df1 = 10, df2 = 20),
         df_05_10 = df(x = f, df1 = 5, df2 = 10), 
        df_20_50 = df(x = f, df1 = 20, df2 = 50)) %>%
  gather(key = "df", value = "density", -f) %>%
  ggplot() +
  geom_line(aes(x = f, y = density, color = df)) +
  labs(title = "F at Various Degrees of Freedom",
       x = "F",
       y = "Density") 

dist_f(deg.f1 = 6, deg.f2 = 45) + theme_sjplot()

x <- rnorm(10)
y <- rnorm(12)
var.test(x, y)

x <- rnorm(10, sd = 1)
y <- rnorm(12, sd = 3)
var.test(x, y)
var(x)/var(y)

##  So if the F-statistic is less than 1, assume you need the lower tail. If it's bigger than 1, assume you need the upper tail.
## https://stats.stackexchange.com/questions/94914/how-to-get-f-test-p-value

dist_f(p = 0.05, deg.f1 = 9, deg.f2 = 11)

pval <- pf(0.1619919, df1=9, df2=11)
PVAL <- 2 * min(pval, 1 - pval)

## to compare models
diamonds <- ggplot2::diamonds	
diamonds.mod1 <- lm(price ~ carat, data = diamonds)
diamonds.mod2 <- lm(depth ~ carat, data = diamonds)

tss <- sum(((diamonds$price - mean(diamonds$price))^2))
rss <- deviance(diamonds.mod1)
param <- length(diamonds$price)

num = (tss -rss) / 1
den = rss / (param - 1 - 1)
num/ den

eval(3.041e+05) # 304100






