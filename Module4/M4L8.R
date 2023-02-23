# labbe distance
# corpus: abs.freqs, all words
library(stylo)
my_corpus <- load.corpus.and.parse(files = "all", corpus.dir = "corpus", markup.type= "plain",
                                   corpus.lang = "Other", sampling = "no.sampling",
                                   features = "w", ngram.size = 1, preserve.case = FALSE,
                                   encoding = "UTF-8")

# select features
features <- make.frequency.list(my_corpus, value = FALSE, head = NULL, 
                           relative = F) 
# prepare table of frequencies
freq <- make.table.of.frequencies(my_corpus, features = features, 
                                  absent.sensitive = F, 
                                  relative = F) # note relative = F
                                  # dim = 6*6030
rowSums(freq)
freq <- as.data.frame.matrix(as.table((freq)))

# distance calculation example 
# note the abs() function!
(sum(abs((freq[1,] - freq[2,])))) # Herodotus_Hist_2 #7790
(sum(abs((freq[1,] - freq[3,])))) # Thucydides_Hist_1 #11538
(sum(abs((freq[1,] - freq[4,])))) # Thucydides_Hist_2 #11310
(sum(abs((freq[1,] - freq[5,])))) # Xenophon_Anab_1 #11516
(sum(abs((freq[1,] - freq[6,])))) # Xenophon_Hell_2 #11480

# this is basically a Manhattan distance, so
labbe1 <- as.matrix(dist(freq, "manhattan"))

# find nearest
find.nearest <- function(dm) {
  for (i in 1:nrow(dm)) {
    new.dm <- dm[i,-i]
    nearest <- which.min(new.dm)
    output <- paste0(rownames(dm)[i], " is nearest to ", names(nearest))
    print(output)
  }}
find.nearest(labbe1) ## it works, cool! 

# if texts are not of similar length
sum(freq[1,]) # gives text length for text A
(sum(abs((freq[1,] - freq[2,])))) / (2 * sum(freq[1,]))

# proxy creates distance matrices from any custom function
# try it with Manhattan first
library(proxy)
fn <- function(x, y) sum(abs(x - y))
labbe2 <- as.matrix(proxy::dist(freq, method = fn))

# now tweak!
fn <- function(x, y) sum(abs(x - y)) / (2*sum(x))
labbe3 <- as.matrix(proxy::dist(freq, method = fn))
find.nearest(labbe3) ## great!


