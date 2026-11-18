library(boot)
#a
set.seed(1)
x <- rnorm(100)
y <- x - 2 * x^2 + rnorm(100)
n <- length(y)
p <- 2
# b
plot(x, y, main = "Scatterplot reflecting noise", xlab = "X", ylab = "Y")
#c
data <- data.frame(x = x, y = y)
loocv_mse <- function(model, data) {
  return(cv.glm(data, model)$delta[1])
}
set.seed(42)
model1 <- glm(y ~ x, data = data)  # Linear model
model2 <- glm(y ~ poly(x, 2), data = data)  # Quadratic model
model3 <- glm(y ~ poly(x, 3), data = data)  # Cubic model
model4 <- glm(y ~ poly(x, 4), data = data)  # Quartic model
loocv1 <- loocv_mse(model1, data)
loocv2 <- loocv_mse(model2, data)
loocv3 <- loocv_mse(model3, data)
loocv4 <- loocv_mse(model4, data)
cat("LOOCV Errors:\n")
cat("Model 1 (Linear):", round(loocv1, 4), "\n")
cat("Model 2 (Quadratic):", round(loocv2, 4), "\n")
cat("Model 3 (Cubic):", round(loocv3, 4), "\n")
cat("Model 4 (Quartic):", round(loocv4, 4), "\n")

#d
set.seed(99)

model1rep <- glm(y ~ x, data = data)
model2rep <- glm(y ~ poly(x, 2), data = data)
model3rep <- glm(y ~ poly(x, 3), data = data)
model4rep <- glm(y ~ poly(x, 4), data = data)
loocv1rep <- loocv_mse(model1rep, data)
loocv2rep <- loocv_mse(model2rep, data)
loocv3rep<- loocv_mse(model3rep, data)
loocv4rep<- loocv_mse(model4rep, data)
cat("New LOOCV Errors with Different Seed:\n")
cat("Model 1 (Linear):", round(loocv1rep, 4), "\n")
cat("Model 2 (Quadratic):", round(loocv2rep, 4), "\n")
cat("Model 3 (Cubic):", round(loocv3rep, 4), "\n")
cat("Model 4 (Quartic):", round(loocv4rep, 4), "\n")

#f
summary(model1)
summary(model2)
summary(model3)
summary(model4)
