## tm library
library(tm)
vignette("tm")

## tm utilizes Corpus as its main structure (VCorpus or PCorpus)
## to get sources, type
getSources()

## data import
my_corpus <- VCorpus(DirSource("./Cicero/", pattern = "txt", encoding = "UTF-8"), 
                     readerControl = list(language = "lat"))
my_corpus

## inspect 
inspect(my_corpus[1])
inspect(my_corpus[[1]])

## the package enable to eliminate extra white spaces and convert to
## lower case, but our text is already transformed

## load stopwords
library(stopwords)
stopWords = stopwords::stopwords("la", source = "ancient")

## remove stopwords
cicero <- tm_map(my_corpus, removeWords, c(stopWords, "nbsp"))

## metadata
meta(cicero, tag = "author", type = "corpus") <- "cicero"
meta(cicero, tag = "book", type = "indexed") <- c("ND", "Off")

## filter
idx <- meta(cicero, "book") == "ND"
ND <- cicero[idx]

## creat dtm
cicero_dtm <- DocumentTermMatrix(cicero)
inspect(cicero_dtm)

## mfw
words_frequency <- colSums(as.matrix(cicero_dtm))

## order
ord <- order(words_frequency, decreasing = TRUE)
head(words_frequency[ord])

## operations on DTM
findFreqTerms(cicero_dtm, 50) ## at least 50 times
head(Terms(cicero_dtm))
