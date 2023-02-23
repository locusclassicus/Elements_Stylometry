# set wd

# prepare corpus
# https://github.com/locusclassicus/GreekDelta/tree/master/
# description at https://classics.nsu.ru/schole/assets/files/16-2-alieva.pdf 


# load libraries
# https://rdrr.io/cran/stylo/
library(stylo)
library(philentropy)

# load corpus
my_corpus <- load.corpus.and.parse(files = "all", corpus.dir = "corpus", markup.type= "plain",
                        corpus.lang = "Other", sampling = "no.sampling",
                        features = "w", ngram.size = 1, preserve.case = FALSE,
                        encoding = "UTF-8")

# select features
mfw <- make.frequency.list(my_corpus, value = FALSE, head = NULL, relative = TRUE)
mfw <- mfw[1:30]

# count frequencies
freq <- make.table.of.frequencies(my_corpus, features = mfw, absent.sensitive = TRUE, 
                                  relative = TRUE)
freq.df <- as.data.frame.matrix(as.table((freq)))
freq.df


# philentropy, manhattan
d.manh <- distance(freq.df, method = "manhattan", use.row.names = T)

# find nearest
find.nearest <- function(dm) {
  for (i in 1:nrow(dm)) {
    new.dm <- dm[i,-i]
    nearest <- which.min(new.dm)
    output <- paste0(rownames(dm)[i], " is nearest to ", names(nearest))
    print(output)
  }}

# apply to distance matrix
find.nearest(d.manh)

# philentropy, euclidean
d.eucl <- distance(freq.df, method = "euclidean", use.row.names = T)
find.nearest(d.eucl)

# philentropy, cosine
d.cos <- 1-distance(freq.df, method = "cosine", use.row.names = T)
find.nearest(d.cos)


# visualize euclidean (different methods give different pictures!)
par(mar=c(2,2,2,10))
d.clust <- hclust(as.dist(d.eucl), method = "complete")
d.dend <- as.dendrogram(d.clust)
plot(d.dend, horiz = T)

par(mar=c(2,2,2,10))
d.clust <- hclust(as.dist(d.eucl), method = "ward.D")
d.dend <- as.dendrogram(d.clust)
plot(d.dend, horiz = T)

# on clustering more below!

# transform distance matrix
library(dplyr)
library(tidyr)
dist.tbl <- d.eucl %>% as_tibble(rownames="A") 
long.dist <- dist.tbl %>% pivot_longer(-A, names_to = "B", values_to = "distance")


# heatmap
library(ggplot2)
long.dist %>% 
  ggplot(aes(x=A, y=B, fill=distance)) + geom_tile() + 
  geom_text(aes(label = round(distance, 2))) + 
  ylab(NULL) + xlab(NULL)

