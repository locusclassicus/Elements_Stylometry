# example from video https://www.youtube.com/watch?v=CBTFT1vu9rQ&t=9s
x = c(1,1,1)

y = c(1,0,2)
z = c(1,0,0) 

y_sim <- sum(y * (1 / (1 + z)))
z_sim <- sum(z * (1 / (1 + y)))

# prepare corpus
library(stylo)
my_corpus <- load.corpus.and.parse(files = "all", corpus.dir = "corpus", 
                                   markup.type= "plain", corpus.lang = "Other", 
                                   sampling = "no.sampling",
                                   features = "w", ngram.size = 1, 
                                   preserve.case = FALSE,
                                   encoding = "UTF-8")
# select features
ngrams <- make.frequency.list(my_corpus, value = FALSE, head = NULL, relative = T)
# count frequencies
freq <- make.table.of.frequencies(my_corpus, features = ngrams, absent.sensitive = TRUE, 
                                  relative = T)
freq <- as.data.frame.matrix(as.table((freq)))

# subset vectors
x = freq[1,] # this will be our query text, 6030 vars
y = freq[2,]
z = freq[3,]

# select features that are present in the query text
idx <- which(x != 0)
x <- x[,idx] #2235 vars

# for each non-zero token in x, select same tokens in y and z
y <- y[,idx]
z <- z[,idx]

# compute Ramezani similarity
y_sim <- sum(y * (1 / (1 + z)))
z_sim <- sum(z * (1 / (1 + y)))

# result 
output_y <- paste("Similarity of ", rownames(x), "and ", rownames(y), "is ", y_sim)
output_z <- paste("Similarity of ", rownames(x), "and ", rownames(z), "is ", z_sim)
  
# print 
print(output_y)
print(output_z)

# similarity function with simple data
x = as.data.frame(matrix(c(1,1,1), nrow = 1))
y = as.data.frame(matrix(c(1,0,2), nrow = 1))
z = as.data.frame(matrix(c(1,0,0), nrow = 1))
doc = x 
rownames(doc) <- "x"
docs = rbind(y, z)
rownames(docs) <- c("y", "z")

fn <- function(doc, docs){
  #subset 
  idx <- which(doc != 0)
  doc <- doc[,idx]
  docs <- docs[,idx]
  # similarity
  sim <- c()
  for (i in 1:nrow(docs)) {
  s <- sum(docs[i,] * (1 / (1 + colSums(docs[-i,]))))
  sim <- c(sim, s)
  }
  sim
  output <- data.frame(doc = rep(rownames(doc), nrow(docs)),
                       docs = rownames(docs),
                       sim = sim)
  output
} 

fn(doc, docs) # this reproduces the examples from video

# similarity on real corpus
doc <- freq[1,]
docs <- freq[-1,]
fn(doc, docs) # note the max score (not min!)

# tweak the similarity function 
dist.ramezani <- function(data){
  result <- data.frame()
  # subset text
  for(n in 1:nrow(data)){
    doc <- data[n,]
    docs <- data
  #subset words
  idx <- which(doc != 0)
  doc <- doc[,idx]
  docs <- docs[,idx]
  # similarity
  sim <- c()
  for (i in 1:nrow(data)) {
    s <- sum(docs[i,] * (1 / (1 + colSums(docs[-i,]))))
    sim <- c(sim, s)
  }
  output <- data.frame(doc = rep(rownames(doc), nrow(docs)),
                       docs = rownames(docs),
                       sim = sim)
  result <- rbind(result, output)
  }
  # transform long data to distance matrix
  result <- reshape(result, idvar = "doc", timevar = "docs", direction = "wide")
  rownames(result) <- result$doc 
  result <- result[,-1]
  colnames(result) <- rownames(result)
  return(result)
}

test <- dist.ramezani(freq) 

# find nearest 
find.nearest.sim <- function(dm) {
  for (i in 1:nrow(dm)) {
    new.dm <- dm[i,-i]
    nearest <- which.max(new.dm) # note max (it's a similarity measure)
    output <- paste0(rownames(dm)[i], " is nearest to ", names(nearest))
    print(output)
    output
  }}

find.nearest.sim(test)


