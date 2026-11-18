#a
set.seed(123)
n <- 100
X <- rnorm(n)
epsilon <- rnorm(n)

#b
beta_0 <- 2
beta_1 <- 3
beta_2 <- -1
beta_3 <- 0.5

Y <- beta_0 + beta_1 * X + beta_2 * X^2 + beta_3 * X^3 + epsilon

#c
library(leaps)

# Create a data frame
data <- data.frame(Y, X)

# Best subset selection
model_selection <- regsubsets(Y ~ poly(X, 10, raw = TRUE), data = data)
summary_selection <- summary(model_selection)

# Extracting best models based on Cp, BIC, and adjusted R2
best_cp <- which.min(summary_selection$cp)
best_bic <- which.min(summary_selection$bic)
best_adj_r2 <- which.max(summary_selection$adjr2)

coef_cp <- coef(model_selection, best_cp)
coef_bic <- coef(model_selection, best_bic)
coef_adj_r2 <- coef(model_selection, best_adj_r2)

list(cp_model = coef_cp, bic_model = coef_bic, adj_r2_model = coef_adj_r2)

par(mfrow = c(2, 2))
plot(summary_selection$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
points(best_cp, summary_selection$rss[best_cp], col = "red", cex = 2, pch = 20)
title(main = "RSS vs Number of Variables")

plot(summary_selection$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "l")
points(best_adj_r2, summary_selection$adjr2[best_adj_r2], col = "red", cex = 2, pch = 20)
title(main = "Adjusted R^2 vs Number of Variables")

plot(summary_selection$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(best_bic, summary_selection$bic[best_bic], col = "red", cex = 2, pch = 20)
title(main = "BIC vs Number of Variables")

#d
# Full model
full_model <- lm(Y ~ poly(X, 10, raw = TRUE), data = data)

# Backward selection
backward_model <- step(full_model, direction = "backward")

# Forward selection
null_model <- lm(Y ~ 1, data = data)
forward_model <- step(null_model, scope = formula(full_model), direction = "forward")

summary(backward_model)
summary(forward_model)

#e
library(glmnet)

X_matrix <- model.matrix(Y ~ poly(X, 10, raw = TRUE), data)[, -1]
lasso_model <- cv.glmnet(X_matrix, Y, alpha = 1)

# cross-validation error
plot(lasso_model)

# Coefficient estimates
best_lambda <- lasso_model$lambda.min
coef(lasso_model, s = best_lambda)

#f
# Generate new Y
Y_new <- beta_0 + beta_1 * X^7 + epsilon

data_new <- data.frame(Y_new, X)

# Best subset selection
model_selection_new <- regsubsets(Y_new ~ poly(X, 10, raw = TRUE), data = data_new)
summary_selection_new <- summary(model_selection_new)

# Lasso model
X_new_matrix <- model.matrix(Y_new ~ poly(X, 10, raw = TRUE), data_new)[, -1]
lasso_model_new <- cv.glmnet(X_new_matrix, Y_new, alpha = 1)

# Compare results
best_lambda_new <- lasso_model_new$lambda.min
coef(lasso_model_new, s = best_lambda_new)
