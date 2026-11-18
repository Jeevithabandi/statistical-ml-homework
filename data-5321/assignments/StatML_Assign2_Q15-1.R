library(ggplot2)
library(ISLR2)
data("Boston")
pred <- subset(Boston, select = -crim)
#a 
model_1 <- lapply(pred, function(x) lm(Boston$crim ~ x))
printCoefmat(do.call(rbind, lapply(model_1, function(x) coef(summary(x))[2, ])))
plot(Boston$rm, Boston$crim)
abline(model_1[[5]])

#b
multiple_model <- lm(crim ~ ., data = Boston)
summary(multiple_model)
#c
plot(sapply(model_1, function(x) coef(x)[2]), coef(multiple_model)[-1],
     xlab = "Univariate regression",
     ylab = "multiple regression"
)
#d
pred <- subset(pred, select = -chas)
fits <- lapply(names(pred), function(p) {
  f <- paste0("crim ~ poly(", p, ", 3)")
  lm(as.formula(f), data = Boston)
})
for (fit in fits) printCoefmat(coef(summary(fit)))
