# воспроизвести https://www.engr.uvic.ca/~seng474/svd.pdf

df = data.frame(d1 = c(c(1, 1), rep(0, 6)),
                d2 = c(c(0, 1, 1, 1), rep(0, 4)),
                d3 = c(1, 0, 0, 1, 0, 1, 0, 0),
                d4 = c(rep(0, 4), rep(1, 4)),
                d5 = c(rep(0, 7), c(1)))
rownames(df) <- c("romeo", "juliet", "happy", "dagger", "live",
                  "die", "free", "new-hampshire")             
df

# svd (full)
A = svd(df)
S = diag(A$d, nrow = length(A$d))

# truncated svd
S[,3:5] <- 0
S

Vt <- S %*% t(A$v) #doc embeddings
colnames(Vt) <- c("d1", "d2", "d3", "d4", "d5")
U <- A$u %*% S #word embeddings
rownames(U) <- c("romeo", "juliet", "happy", "dagger", "live",
                  "die", "free", "new-hampshire") 
U


# query doc
q = c("die", "dagger")
q_doc <-  colSums(U[rownames(U) %in% q, ]) / 2
q_doc

# join
all_df <- rbind(U, t(Vt), q_doc)
all_df <- all_df[,1:2]
all_df <- as_tibble(all_df, rownames = "names") 
 
type <- c(rep("word", 8), rep("doc", 6))
all_df <- all_df %>% bind_cols(type)

colnames(all_df) <- c("x", "dim1", "dim2", "type")

# ablines
zero = tibble(dim1 = 0, dim2 = 0)
line1 = all_df %>% filter(x == "d1") %>% 
  select(-x, -type) %>% bind_rows(zero)
line2 = all_df %>% filter(x == "d5") %>% 
  select(-x, -type) %>% bind_rows(zero)
lm1 = summary(lm(data=line1, dim2 ~ dim1))
lm2 = summary(lm(data=line2, dim2 ~ dim1))

# plot
all_df %>% 
  ggplot(aes(dim1, dim2, 
             color = as.factor(type), label = x)) +
  geom_point(show.legend = F) + 
  geom_text_repel(aes(fontface = "bold"), show.legend = F) +
  theme_bw() + 
  xlab(NULL) + 
  ylab(NULL) +
  geom_abline(slope = 0, intercept = 0, linetype = "dotted") +
  geom_vline(xintercept = -1.10, linetype = "dotted") +
  coord_cartesian(xlim = c(-1.7, 0.2), ylim = c(-1.5, 1.5)) +
  geom_abline(slope = lm1$coefficients[2], 
              intercept = lm1$coefficients[1], linetype = "dotted") + 
  geom_abline(slope = lm2$coefficients[2], 
              intercept = lm2$coefficients[1], linetype = "dotted")
  

# distance 
dist_mx <- all_df %>% filter(type == "doc") %>% select(-x, -type) %>% 
  philentropy::distance(method = "cosine") 
rownames(dist_mx) <- c("d1", "d2", "d3", "d4", "d5", "q")
colnames(dist_mx) <- c("d1", "d2", "d3", "d4", "d5", "q")
dist_mx


