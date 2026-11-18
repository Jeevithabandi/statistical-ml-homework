library(ISLR2)

data("College")

#a
set.seed(123)  # For reproducibility
train_index <- sample(1:nrow(College), 0.7 * nrow(College))
train_set <- College[train_index, ]
test_set <- College[-train_index, ]

#b
lm_model <- lm(Apps ~ ., data = train_set)
lm_predictions <- predict(lm_model, newdata = test_set)

#test error (Mean Squared Error)
lm_test_error <- mean((lm_predictions - test_set$Apps)^2)
lm_test_error

#c
library(glmnet)

X_train <- model.matrix(Apps ~ ., train_set)[, -1]
y_train <- train_set$Apps
X_test <- model.matrix(Apps ~ ., test_set)[, -1]
y_test <- test_set$Apps

#ridge regression with cross-validation
ridge_model <- cv.glmnet(X_train, y_train, alpha = 0)
ridge_predictions <- predict(ridge_model, newx = X_test, s = "lambda.min")
ridge_test_error <- mean((ridge_predictions - y_test)^2)
ridge_test_error

#d
# lasso regression with cross-validation
lasso_model <- cv.glmnet(X_train, y_train, alpha = 1)
lasso_predictions <- predict(lasso_model, newx = X_test, s = "lambda.min")
lasso_test_error <- mean((lasso_predictions - y_test)^2)
num_non_zero_coefficients <- sum(coef(lasso_model, s = "lambda.min") != 0) - 1  # Subtract 1 for intercept

lasso_test_error
num_non_zero_coefficients