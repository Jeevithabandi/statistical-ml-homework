library(ISLR2)
library(caTools)
set.seed(42)


data(Default)


# (a) Fit a logistic regression model using income and balance to predict default
logistic_reg_model <- glm(default ~ income + balance, data = Default, family = binomial)
summary(logistic_reg_model)

# (b) Validation set approach

# data split into training and test sets (70% train, 30% test)
split_data <- sample.split(Default$default, SplitRatio = 0.7)
train_data <- subset(Default, split_data == TRUE)
test_data <- subset(Default, split_data == FALSE)
  
# Fit logistic regression model on training data
train_model <- glm(default ~ income + balance, data = train_data, family = binomial)
summary(train_model)
  


# Predict probabilities on the test set
test_prob <- predict(train_model, newdata = test_data, type = "response")

# probability threshold to convert into classes
test_pred <- ifelse(test_prob > 0.5, "Yes", "No")

# Compute validation set error
validation_err <- mean(test_pred != test_data$default)

# Print validation set error
cat("Validation Set Error Rate:", validation_err, "\n")

# (c) 
set.seed(123)
split_data_1 <- sample.split(Default$default, SplitRatio = 0.7)
train_data_1 <- subset(Default, split_data_1 == TRUE)
test_data_1 <- subset(Default, split_data_1 == FALSE)

train_model_1 <- glm(default ~ income + balance, data = train_data_1, family = binomial)
test_prob_1 <- predict(train_model_1, newdata = test_data_1, type = "response")
test_pred_1 <- ifelse(test_prob_1 > 0.5, "Yes", "No")
validation_err_1 <- mean(test_pred_1 != test_data_1$default)

set.seed(999)
split_data_2 <- sample.split(Default$default, SplitRatio = 0.7)
train_data_2 <- subset(Default, split_data_2 == TRUE)
test_data_2 <- subset(Default, split_data_2 == FALSE)

train_model_2 <- glm(default ~ income + balance, data = train_data_2, family = binomial)
test_prob_2 <- predict(train_model_2, newdata = test_data_2, type = "response")
test_pred_2 <- ifelse(test_prob_2 > 0.5, "Yes", "No")
validation_err_2 <- mean(test_pred_2 != test_data_2$default)

set.seed(555)
split_data_3 <- sample.split(Default$default, SplitRatio = 0.7)
train_data_3 <- subset(Default, split_data_3 == TRUE)
test_data_3 <- subset(Default, split_data_3 == FALSE)

train_model_3 <- glm(default ~ income + balance, data = train_data_3, family = binomial)
test_prob_3 <- predict(train_model_3, newdata = test_data_3, type = "response")
test_pred_3 <- ifelse(test_prob_3 > 0.5, "Yes", "No")
validation_err_3 <- mean(test_pred_3 != test_data_3$default)

cat("Validation Errors:\n")
cat("Split 1:", validation_err_1, "\n")
cat("Split 2:", validation_err_2, "\n")
cat("Split 3:", validation_err_3, "\n")

# (d)
set.seed(42)
split_data_student <- sample.split(Default$default, SplitRatio = 0.7)
train_data_student <- subset(Default, split_data_student == TRUE)
test_data_student <- subset(Default, split_data_student == FALSE)

train_model_student <- glm(default ~ income + balance + student, data = train_data_student, family = binomial)
test_prob_student <- predict(train_model_student, newdata = test_data_student, type = "response")
test_pred_student <- ifelse(test_prob_student > 0.5, "Yes", "No")
verror_student <- mean(test_pred_student != test_data_student$default)
cat("Validation Set (With Student Variable):", verror_student, "\n")
if (verror_student < mean(c(validation_err_1, validation_err_2, validation_err_3))) {
  cat("Including the student variable improves test accuracy.\n")
} else {
  cat("Including the student variable does not significantly improve test accuracy.\n")
}

