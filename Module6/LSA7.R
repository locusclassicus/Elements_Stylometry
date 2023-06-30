# применим svd
svd_review <- svd(review_df)

# Каждый столбец матрицы V^t соответствует документу из набора. 
# Горизонтально в ней идут синг. векторы (т.к. она транспонирована)

# Убедимся в этом
as.integer(review_df[,1]) == as.integer(round((svd_review$u %*% 
                                                 diag(svd_review$d, nrow=length(svd_review$d)) %*% 
                                                 t(svd_review$v)[,1]), 2))

# Если мы умножим S на V^t, то получим эмбеддинг документов
doc_emb <- t(diag(svd_review$d, nrow=length(svd_review$d)) %*% 
  t(svd_review$v))

# вернем названия рядов
movies <-  colnames(review_df)
rownames(doc_emb) <- movies

# расстояние между документами
mx <- as.matrix(dist(doc_emb[,1:50]), method = "euclidean")

# ищем соседей
find_nearest(mx, "Назад в будущее", 3)
find_nearest(mx, "Служебный роман", 3)

# с библиотекой irlba
svd_review <- irlba(as.matrix(review_df), nv = 50, maxit = 1000)
doc_emb <- t(diag(svd_review$d, nrow=length(svd_review$d)) %*% 
               t(svd_review$v))
rownames(doc_emb) <- movies
mx <- as.matrix(dist(doc_emb), method = "euclidean")
find_nearest(mx, "Назад в будущее", 3)
find_nearest(mx, "Служебный роман", 3)

# еще один способ построить эмбеддинги документов 
doc_matrix <- t(review_df) %*% word_emb
mx <- as.matrix(dist(doc_matrix, method = "euclidean"))
find_nearest(mx, "Шерлок Холмс и доктор Ватсон: Знакомство", 10)
find_nearest(mx, "Служебный роман", 10)

