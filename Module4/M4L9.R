# load corpus
library(stylo)
my_corpus <- load.corpus.and.parse(files = "all", corpus.dir = "corpus2", 
                                   markup.type= "plain", corpus.lang = "Other", 
                                   sampling = "no.sampling",
                                   features = "c", ngram.size = 3, 
                                   preserve.case = FALSE,
                                   encoding = "UTF-8")
# select features
ngrams <- make.frequency.list(my_corpus, value = FALSE, head = NULL, relative = T)
# count frequencies
freq <- make.table.of.frequencies(my_corpus, features = ngrams, absent.sensitive = TRUE, 
                                  relative = T)
freq <- as.data.frame.matrix(as.table((freq)))

# cng with two vectors
#subset
x <- freq[1,]
y <- freq[2,]

#select features
idx <- which(x != 0)
x <- x[idx]
y <- y[idx]

# compute distance
sum(((2*(x-y))/(x+y))^2) 

# write function
fn <- function(x, y) {
  idx <- which(x != 0)
  sum(((2*(x[idx]-y[idx]))/(x[idx]+y[idx]))^2)
}
cng <- proxy::dist(freq, method = fn)
find.nearest(as.matrix(cng))

# canberra distance on same data
cnbr <- philentropy::distance(freq, method = "canberra", use.row.names = T)
find.nearest(cnbr)

# clark distance on same data
clrk <- philentropy::distance(freq, method = "clark", use.row.names = T)
find.nearest(clrk)

