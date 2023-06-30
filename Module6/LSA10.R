# чтобы найти ближайшие документы, сначала посчитаем частотность слов
word_matrix <- corpus %>% 
  count(title, word) %>% 
  cast_sparse(title, word, n)

embedding_matrix <- tidy_word_vectors %>% 
  cast_sparse(item1, dimension, value)

doc_matrix <- word_matrix %*% embedding_matrix %>% as.matrix()

# distance 
dist_mx <- as.matrix(dist(doc_matrix))

# find nearest
function(mx, x, y) {
  idx<- which(rownames(mx) == x)
  mx_subset <- mx[idx, -idx] 
  neighbours <- sort(mx_subset, decreasing = F)
  names(head(neighbours, y))
}

# try it
find_nearest(dist_mx, "Philebus", 3)

