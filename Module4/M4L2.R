t1 = c(rep("кошка", 3), "собака")
t2 = c(rep("собака", 3), "кошка")
t3 = rep("собака", 4)

t1 = c(3, 1)
t2 = c(1, 3)
t3 = c(0, 4)

sqrt(sum((t2-t1)^2)) # = sqrt(8)
sqrt(sum((t3-t1)^2)) # = sqrt(18)
sqrt(sum((t3-t2)^2)) # = sqrt(2)

M = rbind(t1, t2, t3) 
dist(M, method = "euclidean")
as.matrix(dist(M, method = "euclidean"))


