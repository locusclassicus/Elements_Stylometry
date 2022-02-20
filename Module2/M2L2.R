## M2L2 Zipf, Herder, Yule
## before this lesson, one needs to know lm, residuals, correlation, Z-statistic, power 

## herdan's C
par(mfrow = c(2, 2))
plot(log(laws.g$Tokens), log(laws.g$Types))
laws.g.lm <- lm(log(laws.g$Types) ~ log(laws.g$Tokens))
abline(alice.g.lm, col = "darkgrey")
summary(laws.g.lm)
## plot residuals
plot(log(laws.g$Tokens), resid(laws.g.lm))
abline(h = 0)

## zipf's K
z = zipf.fnc(Laws, plot = T)
head(z)
plot(log(z$rank), log(z$frequency), type = "s") ## s for "step"
z.lm <- lm(log(z$frequency) ~ log(z$rank))
abline(z.lm, col = "darkgrey")
## plot residuals
plot(log(z$rank), resid(z.lm))
abline(h = 0)

## but both C and K change as size increases! see previous plot
## normally, greater sample size leads to more precise estimates
## and this is known as the LAW OF LARGE NUMBERS
## but this is not so with lexical measures

## probably an example plot (check if slopes are different with diff authors)

## LNRE distributions

## probably introduce lnre() Baayen p. 232

## and something on Yule's K

