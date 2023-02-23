## calculate sent. length in the Platonic corpus
library(XML)
library(dplyr)
filenames <- list.files(path = "./PlatoDiorisisXML")
sent.length <- tibble(title = character(), sent.l = numeric())

  for (i in filenames){
    url = paste("./PlatoDiorisisXML/", i, sep = "")
    doc <- xmlTreeParse(url, useInternalNodes = TRUE, isURL = F)
    rootnode  <- xmlRoot(doc)
    sent <- getNodeSet(doc, "//TEI.2/text/body/sentence")
    for (n in 1:length(sent)) {
      nr.words <- sum(names(xmlChildren(sent[[n]])) == "word")
      tbl <- tibble(title = gsub(".xml","", i), sent.l = nr.words)
      sent.length <- sent.length %>% bind_rows(tbl)
    }
  }

sent.length <- sent.length %>% filter(sent.l != 0)

## histogram
hist(sent.length$sent.l, breaks = 50)

## the median
median(sent.length$sent.l)

## plotting the median
library(ggplot2)
sent.length %>% ggplot(aes(x = sent.l)) + geom_histogram(fill = "lightblue", 
                                                         binwidth = 5) +
  geom_vline(aes(xintercept=median(sent.l)), color = "purple")

## box and whiskers
my_dial <- c("Sophist", "Timaeus")
sent.length %>% filter(title %in% my_dial) %>%
  ggplot(aes(x = title, y = sent.l, fill = title)) + geom_boxplot(alpha = 0.3)

## quantiles
Tim <- sent.length %>% filter(title == "Timaeus") 
median(Tim$sent.l) 
quantile(Tim$sent.l)
quantile(Tim$sent.l, seq(0, 1, 0.1))
IQR(Tim$sent.l)

## skewed data
hist(Tim$sent.l)

## mean
sent.length %>% ggplot(aes(x = sent.l)) + geom_histogram(fill = "lightblue", 
                                                         binwidth = 5) +
  geom_vline(aes(xintercept=median(sent.l)), color = "purple") + 
  geom_vline(aes(xintercept=mean(sent.l)), color = "magenta")

## variance
sl <- sent.length$sent.l
var(sl)
sum((sl - mean(sl))^2) / (length(sl)-1) 

## standard deviation
sqrt(var(sl))
sd(sl)

## estimating the population mean
sample.means <- function(x) {
  my_means <- c()
  for(i in 1:x) {
    my_sample <- sent.length %>% sample_n(2000, replace = TRUE)
    my_mean <- mean(my_sample$sent.l)
    my_means <- c(my_means, my_mean)
  }
  hist(my_means)
  abline(v = mean(my_means), col = "red", lwd = 3)
  abline(v = mean(sent.length$sent.l), col = "blue", lwd =3)
}

set.seed(1)
sample.means(10)
sample.means(100)
sample.means(1000)

## compare
mean(sent.length$sent.l)



