# мы уже видели, что, когда мы умножаем матрицу на вектор, мы получаем новый вектор
# этот вектор может изменить направление или длину
x = c(1, 1)
M = matrix(c(2, 0.5, 3, 1), nrow = 2)
y = M %*% x
y

# function that plots a vector
plot_vector <- function(vec1, vec2 = NA) {
  if (length(vec1) > 2 | length(vec2) > 2) { stop("too many dimensions") }
  df <- data.frame(x_start = 0, y_start = 0, x_end = c(vec1[1], vec2[1]), y_end = c(vec1[2], vec2[2]))
  df <- df %>% bind_cols(n = row_number(.))
  labels = c((deparse(substitute(vec1))), (deparse(substitute(vec2))))
  ggplot(df, aes(x_start, y_start, xend = x_end, yend = y_end, color = as.factor(n))) + 
    geom_segment(arrow = arrow(angle = 20, length = unit(0.1, "inches")),
                 show.legend = F, size = 1.2, alpha = 0.7 ) +
    # scale_x_continuous(limits = c(0, max(x)+1)) +
    # scale_y_continuous(limits = c(0, max(y)+1)) +
    theme_bw() +
    xlab("X") + 
    ylab("Y") +
    tune::coord_obs_pred() + 
    geom_text(aes(x=x_end, y=y_end, label=labels), show.legend = F,
              vjust = -1, hjust = 2, size = 5)
}

plot_vector(x, y)

# Определитель матрицы — скалярная величина, которая характеризует 
# ориентированное «растяжение» или «сжатие» многомерного евклидова пространства 
# после преобразования матрицей; имеет смысл только для квадратных матриц.

# сохраним координаты векторов единичного квадрата

S <- matrix(c(0,1,1,0), nrow=2)

ggplot() +
  geom_segment(aes(x = 0, y = 0, xend = S[[1,1]], yend = S[[2,1]] )) +
  geom_segment(aes(x = 0, y = 0, xend = S[[1,2]], yend = S[[2,2]])) +
  geom_segment(aes(x = S[[1,2]], y = S[[2,2]], xend = S[[2,1]], yend = S[[1,2]])) +
  geom_segment(aes(x = S[[1,1]], y = S[[2,1]], xend = S[[2,1]], yend = S[[1,2]])) + 
  tune::coord_obs_pred() 

# теперь преобразуем эти векторы  матрицей 
M = matrix(c(1, 0, 1, 3), nrow= 2)
S_new <- M %*% S

# и снова нарисуем
plot_figures <- function(S, S_new) {
  ggplot() +
    geom_segment(aes(x = 0, y = 0, xend = S[[1,1]], yend = S[[2,1]])) +
    geom_segment(aes(x = 0, y = 0, xend = S[[1,2]], yend = S[[2,2]])) +
    geom_segment(aes(x = S[[1,2]], y = S[[2,2]], xend = S[[2,1]], yend = S[[1,2]])) +
    geom_segment(aes(x = S[[1,1]], y = S[[2,1]], xend = S[[2,1]], yend = S[[1,2]])) +
    # new figure
    geom_segment(aes(x = 0, y = 0, xend = S_new[[1,1]], yend = S_new[[2,1]] )) +
    geom_segment(aes(x = 0, y = 0, xend = S_new[[1,2]], yend = S_new[[2,2]])) +
    geom_segment(aes(x = S_new[[1,2]], y = S_new[[2,2]], xend = S_new[[1,1]]+S_new[[1,2]], yend = S_new[[2,1]]+S_new[[2,2]])) +
    geom_segment(aes(x = S_new[[1,1]], y = S_new[[2,1]], xend = S_new[[1,1]]+S_new[[1,2]], yend = S_new[[2,1]]+S_new[[2,2]])) +
    tune::coord_obs_pred()
}

plot_figures(S, S_new)

# сравним площадь двух фигур
# площадь S мы знаем: 1
# площадь S_new считаем как произведение высоты и стороны,  
# к которой проведена высота

# узнаем определитель матрицы
det(M)

# если определитель равен нулю, то матрица называется вырожденной 
# это бывает, если векторы линейно зависимы
M = matrix(c(2, 3, 4, 6), nrow= 2)
det(M)
S_new <- M %*% as.matrix(S)
plot_figures(S, S_new)

# если определитель отрицательный, то меняется ориентация фигуры
M = matrix(c(-2, 2, 1, 1), nrow= 2)
det(M)
S_new <- M %*% as.matrix(S)
plot_figures(S, S_new)

# чтобы совершить обратное преобразование, нужна обратная матрица
M_inv = solve(M)
S <- round((M_inv %*% S_new), 2)
S

