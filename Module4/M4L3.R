t1 = c(3, 1)
t2 = c(1, 3)
t3 = c(0, 4)

l2 <- function(x) sqrt(sum(x^2))

cos1_2 <- (t1 %*% t2) / l2(t1) / l2(t2)
(acos(cos1_2) * 180) / pi

cos3_2 <- (t3 %*% t2) / l2(t3) / l2(t2)
(acos(cos3_2) * 180) / pi

cos3_1 <- (t3 %*% t1) / l2(t3) / l2(t1)
(acos(cos3_1) * 180) / pi

M = cbind(t1, t2, t3) 
library(lsa)
cosine(M)

library(philentropy)
M = rbind(t1, t2, t3) 
distance(M, method = "cosine")


