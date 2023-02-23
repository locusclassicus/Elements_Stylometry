# KLD
# example from Savoy, Machine learning methods for stylometry, p. 40
library(philentropy)
A1 = c(0.1, 0.2, 0.7)
A2 = c(0.333, 0.333, 0.333)
A3 = c(0.45, 0.35, 0.2)
Q = c(0.5, 0.3, 0.2)
data = rbind(A1, A2, A3, Q)

# philentropy, vectors
kullback_leibler_distance(Q, A1, testNA = F, 
                          unit = "log2", epsilon = 0.00001)

# philentropy, matrix
KLD1 <- distance(
  data, method = "kullback-leibler", test.na = F,
  unit = "log2", epsilon = 0.00001, use.row.names = T, as.dist.obj = T)

# pairwise KLD (note the asymmetry), note that in KLD1 several measures are absent

sum(Q * log2(Q/A1))
sum(Q * log2(Q/A2))
sum(Q * log2(Q/A3))
sum(A1 * log2(A1/Q))
sum(A1 * log2(A1/A3))
sum(A1 * log2(A1/A2))
sum(A2 * log2(A2/Q))
sum(A2 * log2(A2/A1))
sum(A2 * log2(A2/A3))
sum(A3 * log2(A3/Q))
sum(A3 * log2(A3/A1))
sum(A3 * log2(A3/A2))

# proxy solution
fn = function(x, y) sum(x * log2(x/y))
KLD2 <- proxy::dist(data, method = fn)
KLD2
# gives the rest of the measures

#Compare 
KLD1 # e.g., Q||A1
KLD2 # e.g., A1||Q

# transform
KLD.join <- as.matrix(KLD1)
KLD.join[lower.tri(KLD.join)] <- KLD2
KLD.join # now the matrix contains no duplicates

# calculate H 
entropy <- function(data) {
  apply(data, 1, function(x) -sum(x * log2(x)))
}
H <- entropy(data)
H

# calculate cross-entropy
cross.entropy.m <- function(data){
  data %*% -log2(t(data)) 
}
CH <- cross.entropy.m(data)

# check 
round((KLD.join + H),4) == round(CH, 4) # great!

# find nearest
find.nearest <- function(dm) {
  for (i in 1:nrow(dm)) {
    new.dm <- dm[i,-i]
    nearest <- which.min(new.dm)
    output <- paste0(rownames(dm)[i], " is nearest to ", names(nearest))
    print(output)
  }}
find.nearest(KLD.join)

# prepare frequencies
# load corpus
my_corpus <- load.corpus.and.parse(files = "all", corpus.dir = "corpus", markup.type= "plain",
                                   corpus.lang = "Other", sampling = "no.sampling",
                                   features = "w", ngram.size = 1, preserve.case = FALSE,
                                   encoding = "UTF-8")
# select features
mfw <- make.frequency.list(my_corpus, value = FALSE, head = NULL, relative = T)
mfw <- mfw[1:30]
# count frequencies
freq <- make.table.of.frequencies(my_corpus, features = mfw, absent.sensitive = TRUE, 
                                  relative = F)
freq <- as.data.frame.matrix(as.table((freq)))
freq <- t(apply(freq, 1, function(x) x/sum(x)))
rowSums(freq)

# calculate KLD1
KLD1 <- distance(
  freq, method = "kullback-leibler", test.na = F,
  unit = "log2", epsilon = 0.00001, use.row.names = T, as.dist.obj = T)

# calculate KLD2
KLD2 <- proxy::dist(freq, method = fn)

# join matrices
KLD.join <- as.matrix(KLD1)
KLD.join[lower.tri(KLD.join)] <- KLD2
KLD.join

# find nearest
find.nearest(KLD.join)

# jeffreys distance
fn <- function(x, y) {sum((x-y)*log(x/y))}
JF1 <- round(as.matrix(proxy::dist(freq, method = fn)), 4)
JF1

JF2 <- round(distance(freq, method = "jeffreys", use.row.names = T, 
                as.dist.obj = F),4)

JF1 == JF2
find.nearest(JF1) 

# jeffreys and KLD
kl1 <- kullback_leibler_distance(Q, A1, testNA = F, 
                          unit = "log2", epsilon = 0.00001)
kl2 <- kullback_leibler_distance(A1, Q, testNA = F, 
                          unit = "log2", epsilon = 0.00001)
jf <- jeffreys(A1, Q, testNA = F, unit = "log2", epsilon = 0.00001)
kl1 + kl2 == jf
