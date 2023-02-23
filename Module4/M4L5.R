# take frequencies from the previous lesson
freq <- make.table.of.frequencies(my_corpus, features = mfw, absent.sensitive = TRUE, 
                                  relative = TRUE)
freq.df <- as.data.frame.matrix(as.table((freq)))
freq.df

# to get Burrows' Delta, scale frequencies before calculating
# manhattan distance (and divide by the number of features)

d.burr1 <- distance(scale(freq.df), method = "manhattan", use.row.names = T)/30
# or, with stylo,
d.burr2 <- as.matrix(dist.delta(freq))
d.burr1 == d.burr2

find.nearest(d.burr2)

# to get Argamon's Delta, do the same with the euclidean metrics
d.arg1 <- distance(scale(freq.df), method = "euclidean", use.row.names = T)/30
d.arg2 <- as.matrix(dist.argamon(freq))
d.arg1 == d.arg2

# Cosine distance in Stylo
d.cos1 <- 1-distance(freq.df, method = "cosine", use.row.names = T)
d.cos2 <- as.matrix(dist.cosine(freq))
d.cos1 == d.cos2 
round(d.cos1, 3) == round(d.cos2, 3)

# Würzburg Delta does the same trick + scaling
d.wurz1 <- 1-distance(scale(freq.df), method = "cosine", use.row.names = T)
d.wurz2 <- as.matrix(dist.wurzburg(freq))
round(d.wurz1, 3) == round(d.wurz2, 3)

# check if anything fixed our little problem
find.nearest(d.burr1)
find.nearest(d.arg1)
find.nearest(d.cos1)
find.nearest(d.wurz1)




