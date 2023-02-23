# семейство кривых 
data.frame(chisq = 0:7000 / 100) %>% 
  mutate(df_05 = dchisq(x = chisq, df = 5),
         df_15 = dchisq(x = chisq, df = 15),
         df_30 = dchisq(x = chisq, df = 30)) %>%
  gather(key = "df", value = "density", -chisq) %>%
  ggplot() +
  geom_line(aes(x = chisq, y = density, color = df)) +
  labs(title = "Chi-Square at Various Degrees of Freedom",
       x = "Chi-square",
       y = "Density") 
 ## уровень значимости
library(sjPlot)
dist_chisq(deg.f = 4, p = 0.05)

## p- & q-
pchisq(9.49, 4)
qchisq(0.95, 4)

# критерий согласия
load("/Users/olga/R_Workflow/Elements_Stylometry/data/PlatoWords.Rdata")
library(dplyr)
my_words = c("γε", "μέν", "δέ",	"γάρ", "οὖν")
section.words.pl <- words.tbl %>% mutate(section = (row_number() %/% 5000)+1) %>% 
  filter(word %in% my_words) %>% mutate(word = factor(word)) %>% 
  group_by(section, word, .drop = F) %>% count()  %>% ungroup()

pl_sample <- section.words.pl %>% filter(section == 50) %>% select(-section)
chisq.test(pl_sample$n)
# под капотом критерия согласия
mu <- mean(pl_sample$n) 

chis <- c()
for (i in 1:length(pl_sample$n)) {
  chi = (pl_sample$n[i] - mu)^2 / mu
  chis <- c(chis, chi)
}
sum(chis)
1 - pchisq(341.2468, df = 4) 

## критерий равенства пропорций
load("/Users/olga/R_Workflow/Elements_Stylometry/data/AristotleTidy.Rdata")
section.words.ar <- Aristotle.tidy %>% mutate(section = (row_number() %/% 5000)+1) %>% 
  filter(word %in% my_words) %>% mutate(word = factor(word)) %>% 
  group_by(section, word, .drop = F) %>% count()  %>% ungroup()
ar_sample <- section.words.ar %>% filter(section == 10) %>% select(-section)
# таблица сопряженности
library(janitor)
pl_sample <- pl_sample %>% mutate(who = "Plato")
ar_sample <- ar_sample %>% mutate(who = "Aristotle")
observed <- bind_rows(pl_sample, ar_sample) %>% spread(word, n) 
# преобразовать
observed <-  as.data.frame(observed)
rownames(observed) <- observed$who
observed <- observed[, -1] 
# проверить 
chisq.test(observed)
# повторить вручную
library(janitor)
observed.totals <- bind_rows(pl_sample, ar_sample) %>% spread(word, n) %>% 
  adorn_totals("row") %>% adorn_totals("col")

ar.expected <- (415/731) * (observed.totals[3, 2:6]/731) * 731
pl.expected <- (316/731) * (observed.totals[3, 2:6]/731) * 731

ar.observed <- observed[1, ]
pl.observed <- observed[2, ]

ar.chi <- ((ar.observed - ar.expected)^2) / ar.expected
pl.chi <- ((pl.observed - pl.expected)^2) / pl.expected

sum(ar.chi) + sum(pl.chi)  # 20.34052

# критерий независимости
load("/Users/olga/R_Workflow/Elements_Stylometry/data/DialogueType.Rdata")
dialogues <- dialogues[,-1 ]
table(dialogues)

chisq.test(table(dialogues), correct = F)

num <- 25*((5*8 - 11)^2)
den <-  (5+11)*(1+8)*(5+1)*(11+8)
num/den ## 

# с поправкой на непрерывность
chisq.test(table(dialogues))
num <- 25*((abs(5*8 - 11) - 25/2)^2)
den <-  (5+11)*(1+8)*(5+1)*(11+8)
num/den

# фишер
fisher.test(table(dialogues))

## https://vulstats.ucsd.edu/chi-squared.html (подробнее)

