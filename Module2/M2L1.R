## M2L1 models for lexical richness (based on Baayen & Savoy)
## before this lesson, one needs to know confidence intervals
## load packages
library(languageR)
library(stylo)

## load text
Laws <- load.corpus.and.parse(file = "Laws.txt", corpus.dir = getwd(), markup.type= "plain", 
corpus.lang = "Other", sampling = "no.sampling", encoding = "UTF-8")

## analyze growth rate
laws.growth <- growth.fnc(text = Laws[[1]], size = 1000, nchunks = 103)
laws.g <- laws.growth@data$data
head(laws.g)
plot(laws.growth)

## compare two chunks of unequal size
laws1 = Laws[[1]][1:10000]
laws2 = Laws[[1]][10001:30000]
compare.richness.fnc(laws1, laws2)

## the difference is not significant if chunks are of equal length
laws1 = Laws[[1]][1:10000]
laws2 = Laws[[1]][10001:20000]
compare.richness.fnc(laws1, laws2)

## so why don't we compare texts of a fixed size? 
## Savoy p. 27: resulting TTR values will not be the same! 
## let us take 50  normal 5000-word samples from the Laws

my_samples <- make.samples(Laws[[1]], sample.size = 5000, 
                           sampling = "normal.sampling")

## we know the number of tokens in each sample, so what we need is a number of "types"
## this is easily done with
length(table(my_samples[[1]]))
## or 
length(make.frequency.list(my_samples[[1]]))

## calculate ttr for specific samples
ttr.samples <- c()
tokens = 5000
for(i in 1:length(my_samples)){
  types <- length(table(my_samples[[i]]))
  tt <- types/tokens
  ttr <- c(ttr, tt)
}

## general ttr 
types.gen <- length(table(Laws[[1]]))
tokens.gen <- length(Laws[[1]])
ttr.gen <- types.gen/tokens.gen

## ttr from growth object
laws.growth <- growth.fnc(text = Laws[[1]], size = 5000, nchunks = 20)
ttr.growth <- laws.growth@data$data$TypeTokenRatio

## plot
par(mfrow = c(1,1))
plot(ttr, type = "b", ylim = c(0.05, 0.25), col = "darkblue")
abline(h = ttr.gen, col = "darkred", lty = 2)
lines(ttr.growth, col = "purple", lty = 2)

## add confidence intervals

## sum up with some notes on Campbell 


