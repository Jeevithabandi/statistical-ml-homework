library(ISLR2)      # For Boston dataset
library(leaps)     # For best subset selection
library(glmnet)    # For Lasso and Ridge regression
library(pls)       # For PCR
library(caret)     # For cross-validation

data("Boston")
head(Boston)
# Best subset selection
best_subset <- regsubsets(crim ~ ., data = Boston, method = "exhaustive")
summary(best_subset)
plot(best_subset, scale = "bic")# Plot the results to see the performance of different model sizes

#Lasso Regression
x <- model.matrix(crim ~ ., Boston)[, -1]
y <- Boston$crim
set.seed(123)
lasso_model <- cv.glmnet(x, y, alpha = 1)
plot(lasso_model)

# Best lambda
best_lambda_lasso <- lasso_model$lambda.min
lasso_coeffs <- coef(lasso_model, s = "lambda.min")
print(lasso_coeffs)

#Ridge Regression
ridge_model <- cv.glmnet(x, y, alpha = 0)
plot(ridge_model)

# Best lambda
best_lambda_ridge <- ridge_model$lambda.min
ridge_coeffs <- coef(ridge_model, s = "lambda.min")
print(ridge_coeffs)

# PCR
pcr_model <- pcr(crim ~ ., data = Boston, validation = "CV")
summary(pcr_model)

# Plot the validation results
validationplot(pcr_model, val.type = "MSEP")


#(b)
# Compare model performance using RMSE or MAE
set.seed(123)
train_index <- createDataPartition(Boston$crim, p = 0.8, list = FALSE)
train_data <- Boston[train_index, ]
test_data <- Boston[-train_index, ]

# Train Lasso
lasso_final <- glmnet(model.matrix(crim ~ ., train_data)[, -1], train_data$crim, alpha = 1, lambda = best_lambda_lasso)
predictions_lasso <- predict(lasso_final, s = best_lambda_lasso, newx = model.matrix(crim ~ ., test_data)[, -1])
rmse_lasso <- sqrt(mean((predictions_lasso - test_data$crim)^2))

# Train Ridge
ridge_final <- glmnet(model.matrix(crim ~ ., train_data)[, -1], train_data$crim, alpha = 0, lambda = best_lambda_ridge)
predictions_ridge <- predict(ridge_final, s = best_lambda_ridge, newx = model.matrix(crim ~ ., test_data)[, -1])
rmse_ridge <- sqrt(mean((predictions_ridge - test_data$crim)^2))

cat("Lasso RMSE:", rmse_lasso, "\n")
cat("Ridge RMSE:", rmse_ridge, "\n")

#(c)
# Extract Lasso coefficients and convert to a matrix
lasso_coeffs <- as.matrix(coef(lasso_model, s = "lambda.min"))


selected_features_lasso <- rownames(lasso_coeffs)[lasso_coeffs != 0]
cat("Selected features in Lasso:", selected_features_lasso, "\n")
cat("Ridge coefficients:")
print(ridge_coeffs)

