# теперь возьмем нашу матрицу совместной встречаемости из урока 1
# сначала превратим первый столбец в имена рядов
lemmas <- review_wide$lemma
review_df <- as.data.frame(review_wide)[,-1]
rownames(review_df) <- lemmas

# применим svd
svd_review <- svd(review_df)

# Для вычисления первой строки исходной матрицы нужна первая строка 
# матрицы U и полностью Σ, V^t. Убедимся в этом:
as.integer(review_df[1,]) == as.integer(round((svd_review$u[1,] %*% 
         diag(svd_review$d, nrow=length(svd_review$d)) %*% 
         t(svd_review$v)), 2))

# Получается, что за ряды в нашей исходной матрице "отвечает" U,
# а ряды при этом соот. словам. Поэтому можно сказать, что наша U хранит
# векторные описания слов

# при этом мы помним, что столбцы U -- это разные сингулярные векторы (вертикально)
# и каждому вектору соот. его "вес", который хранится в S 
# (малые синг. значения хранят меньше информации)
# если мы умножим U на S, то получим эмбеддинг слов

word_emb <- round((svd_review$u %*% diag(svd_review$d, nrow=length(svd_review$d))), 2)

# вернем названия рядов
rownames(word_emb) <- lemmas

# так мы можем узнать, какие слова ближе всего друг другу
# посчитаем евклидово расстояние
mx <- as.matrix(dist(word_emb, method = "euclidean"))

# функция для поиска соседей
find_nearest <- function(mx, x, y) {
  idx<- which(rownames(mx) == x)
  mx_subset <- mx[idx, -idx] 
  neighbours <- sort(mx_subset, decreasing = F)
  names(head(neighbours, y))
}

# применим функцию к нашей матрице расстояний
find_nearest(mx, "казино", 10)

# таким образом, используя эмбеддинги слов, 
# мы нашли ближайшие слова

# но мы использовали полностью все сингулярные векторы и все 
# сингулярные значения, хотя не все векторы одинакого информативны

#  усеченное svd 
word_emb <- round((svd_review$u %*% 
                     diag(svd_review$d, nrow=length(svd_review$d))), 2)[,1:10]
rownames(word_emb) <- lemmas
mx <- as.matrix(dist(word_emb, method = "euclidean"))
find_nearest(mx, "дарт", 10)

# с использованием библиотеки irlba
library(irlba)
svd_review <- irlba(as.matrix(review_df), nv = 50, maxit = 1000)
dim(svd_review$u)
dim(svd_review$v)

# эмбеддинги слов
word_emb <- svd_review$u %*% diag(svd_review$d, nrow=50)

# матрица расстояния 
words_mx <- as.matrix(dist(word_emb, method = "euclidean"))
rownames(words_mx) <- lemmas
colnames(words_mx) <- lemmas

find_nearest(words_mx, "жемчуг", 10)
