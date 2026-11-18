library(ISLR2)
library(pls) 

attach(College)
set.seed(1)

# Split the data into training and test sets
train <- sample(1:nrow(College), nrow(College) / 2)
test <- -train

# Define response and predictors
x <- model.matrix(Apps ~ ., College)[, -1]
y <- College$Apps

x_train <- x[train, ]
y_train <- y[train]
x_test <- x[test, ]
y_test <- y[test]

# (e)
pcr_model <- pcr(Apps ~ ., data = College, subset = train, scale = TRUE, validation = "CV")
summary(pcr_model)

pcr_pred <- predict(pcr_model, x_test, ncomp = 10)  # Assume M=10 from cross-validation
pcr_mse <- mean((pcr_pred - y_test)^2)
cat("PCR Test MSE:", pcr_mse, "\n")

# (f)
pls_model <- plsr(Apps ~ ., data = College, subset = train, scale = TRUE, validation = "CV")
summary(pls_model)

# Selecting the best number of components using cross-validation
pls_pred <- predict(pls_model, x_test, ncomp = 8)  # Assume M=8 from cross-validation
pls_mse <- mean((pls_pred - y_test)^2)
cat("PLS Test MSE:", pls_mse, "\n")

# (g)
cat("\nComparison of Test Errors:\n")
cat("PCR Test MSE:", pcr_mse, "\n")
cat("PLS Test MSE:", pls_mse, "\n")

# Comments on the results
if (pls_mse < pcr_mse) {
  cat("\nThe PLS model performs better with a lower test error.\n")
} else {
  cat("\nThe PCR model performs better with a lower test error.\n")
}
