library(ISLR2)
data(Weekly)
Weekly$Direction <- as.factor(Weekly$Direction)
#a
set.seed(42)
log_model <- glm(Direction ~ Lag1 + Lag2, data = Weekly, family = binomial)
summary(log_model)
#b
fit.log_model = glm(Direction ~ Lag1 + Lag2, data= Weekly[-1,], family = binomial)
summary(fit.log_model)
#c
prob <- predict(fit.log_model, newdata = Weekly[1, ], type = "response")
pred <- ifelse(prob > 0.5, "Up", "Down")
actual_direction <- Weekly$Direction[1]
correctly_classified <- pred == actual_direction
pred
actual_direction
correctly_classified
#d
# Initialize vector to store errors
n <- nrow(Weekly)  # Total number of observations
errors <- rep(0, n)  # Vector to track misclassifications

# LOOCV Loop
for (i in 1:n) {
  # Fit model excluding the ith observation
  loo_model <- glm(Direction ~ Lag1 + Lag2, data = Weekly[-i, ], family = binomial)
  
  # Predict probability for ith observation
  loo_prob <- predict(loo_model, newdata = Weekly[i, ], type = "response")
  
  # Convert probability to class prediction
  loo_pred <- ifelse(loo_prob > 0.5, "Up", "Down")
  
  # Check if prediction matches actual direction
  if (loo_pred != Weekly$Direction[i]) {
    errors[i] <- 1  # Misclassified observation
  }
}
# (e)
loocv_error <- mean(errors)
cat("LOOCV Estimate of Test Error:", loocv_error, "\n")

