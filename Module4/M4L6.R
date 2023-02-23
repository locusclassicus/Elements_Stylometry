t1 = c(3, 1)
t2 = c(1, 3)
t3 = c(0, 4)
M = rbind(t1, t2, t3) 

d <- as.matrix(dist.minmax(M))
find.nearest(d)

# in philentropy
distance(M, method="tanimoto", use.row.names = T)

## now to real texts
d.ruz1 <- as.matrix(dist.minmax(freq))
find.nearest(d.ruz1)

# in philentropy
d.ruz2 <- distance(freq.df, method="tanimoto", use.row.names = T)
round(d.ruz1, 3) == round(d.ruz2, 3)

#  1 - tan = ruz ==> tan = 1 - ruz (which exactly what Stylo does)

