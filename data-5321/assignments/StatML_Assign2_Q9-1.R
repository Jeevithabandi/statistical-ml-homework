library(ISLR2)
data("Auto")
pairs(Auto, cex = 0.2)
#b
cor_matrix <- subset(Auto, select = -name)
cor(cor_matrix)
#c
fit <- lm(mpg ~ ., data = cor_matrix)
summary(fit)
#d
par(mfrow = c(2, 2))
plot(fit)
#e
interaction_model <- lm(mpg ~ .^2, data = Auto[, -which(names(Auto) == "name")])
summary(interaction_model)
#f
log_model <- lm(log(mpg) ~ ., data = Auto[, -which(names(Auto) == "name")])
summary(log_model)

sqrt_model <- lm(sqrt(mpg) ~ ., data = Auto[, -which(names(Auto) == "name")])
summary(sqrt_model)

square_model <- lm(I(mpg^2) ~ ., data = Auto[, -which(names(Auto) == "name")])
summary(square_model)
