set.seed(1)
x <- rnorm(100, mean = 0, sd = 1)  # x from N(0, 1)
x
#b
set.seed(1)
e <- rnorm(100, mean = 0, sd = sqrt(0.25)) 
e
#c
y <- -1 + 0.5 * x + e
y
#d
plot(x, y, main = "Scatterplot with Regression Lines", xlab = "X", ylab = "Y", pch = 19, col = "blue")
#e
model <- lm(y ~ x)  # Fit regression model
beta_hat_0 <- coef(model)[1]  # Intercept
beta_hat_1 <- coef(model)[2]  # Slope
beta_hat_0
beta_hat_1
#f
abline(model, col = "red", lwd = 2)
abline(a = -1, b = 0.5, col = "green", lwd = 2, lty = 2)  # Population regression line
legend("topleft", legend = c("Data Points", "Fitted Regression Line", "Population Regression Line"), col = c("blue", "red", "green"), lty = c(NA, 1, 2), pch = c(19, NA, NA), lwd = c(NA, 2, 2))
#g
x2 <- x^2
poly <- lm(y ~ x + I(x2))
summary(poly)
#h
set.seed(1)
x <- rnorm(100, mean = 0, sd = 1)
epsilon_less_noise <- rnorm(100, mean = 0, sd = sqrt(0.1))
y_less_noise <- -1 + 0.5 * x + epsilon_less_noise

# Fit the linear model
model_less_noise <- lm(y_less_noise ~ x)

# Scatterplot with regression lines
plot(x, y_less_noise, main = "Less Noise: Scatterplot with Regression Line",
     xlab = "X", ylab = "Y", pch = 19, col = "blue")
abline(model_less_noise, col = "red", lwd = 2)
abline(a = -1, b = 0.5, col = "green", lwd = 2, lty = 2)
legend("topleft", legend = c("Data Points", "Fitted Regression Line", "Population Regression Line"), col = c("blue", "red", "green"), lty = c(NA, 1, 2), pch = c(19, NA, NA), lwd = c(NA, 2, 2))
#i
set.seed(1)
x <- rnorm(100, mean = 0, sd = 1)
epsilon_more_noise <- rnorm(100, mean = 0, sd = 1)
y_more_noise <- -1 + 0.5 * x + epsilon_more_noise

model_more_noise <- lm(y_more_noise ~ x)

plot(x, y_more_noise, main = "More Noise: Scatterplot with Regression Line",xlab = "X", ylab = "Y", pch = 19, col = "blue")
abline(model_more_noise, col = "red", lwd = 2)
abline(a = -1, b = 0.5, col = "green", lwd = 2, lty = 2)
legend("topleft", legend = c("Data Points", "Fitted Regression Line", "Population Regression Line"),col = c("blue", "red", "green"), lty = c(NA, 1, 2), pch = c(19, NA, NA), lwd = c(NA, 2, 2))
#j
# Confidence intervals for original dataset
confint(lm(y ~ x))

# Confidence intervals for less noisy dataset
confint(model_less_noise)

# Confidence intervals for more noisy dataset
confint(model_more_noise)
