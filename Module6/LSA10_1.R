# матрица совместной встречаемости: Платон
word_matrix <- corpus %>% 
  count(title, word) %>% 
  cast_sparse(title, word, n) %>% as.matrix() %>% t()

# column and row names
words = rownames(word_matrix)
titles = colnames(word_matrix)

# усеченное svd: Платон
A = irlba(word_matrix, nv = 10, maxit = 1000)
S = diag(A$d, nrow = length(A$d))

Vt <- S %*% t(A$v) #doc embeddings
colnames(Vt) <- titles

U <- A$u %*% S #word embeddings
rownames(U) <- words

# query doc
# q = c("δικαιοσύνη", "ἀδικία")
 q = c("λύπη", "ἡδονά")
# q = c( "σῶμα", "ψυχή")
# q = c( "στοιχεῖον", "ἀήρ", "πῦρ")
q_doc <-  colSums(U[rownames(U) %in% q, ]) / 2
q_doc

# join
all_df <- rbind(t(Vt), q_doc)

# distance 
dist_mx <- all_df %>%
  philentropy::distance(method = "cosine")
rownames(dist_mx) <- c(titles, "q")
colnames(dist_mx) <- c(titles, "q")

# nearest doc
nearest <- function(dist_mx, y) {
  mx_subset <- 1-dist_mx[nrow(dist_mx), -nrow(dist_mx)] 
  neighbours <- sort(mx_subset, decreasing = F)
  names(head(neighbours, y))
}

result <- nearest(dist_mx, 5)
result
q
