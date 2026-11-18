library(ISLR2)
data("Auto")
mpg_median <- median(Auto$mpg)
Auto$mpg01 <- ifelse(Auto$mpg > mpg_median, 1, 0)
Auto <- Auto[, c("mpg01", colnames(Auto)[-which(colnames(Auto) == "mpg")])]
head(Auto)
#b
boxplot(Auto$displacement ~ Auto$mpg01, main = "Displacement vs mpg01")
boxplot(Auto$horsepower ~ Auto$mpg01, main = "Horsepower vs mpg01")
boxplot(Auto$weight ~ Auto$mpg01, main = "Weight vs mpg01")
boxplot(Auto$acceleration ~ Auto$mpg01, main = "Acceleration vs mpg01")
pairs(Auto[, c("displacement", "horsepower", "weight", "acceleration", "mpg01")])
#c
set.seed(1)
train_index <- sample(1:nrow(Auto), size = 0.7 * nrow(Auto))
train_data <- Auto[train_index, ]
test_data <- Auto[-train_index, ]
#d
library(MASS)
lda_fit <- lda(mpg01 ~ displacement + horsepower + weight, data = train_data)
lda_pred <- predict(lda_fit, newdata = test_data)
lda_class <- lda_pred$class
lda_conf_matrix <- table(Predicted = lda_class, Actual = test_data$mpg01)
lda_test_error <- 1 - mean(lda_class == test_data$mpg01)

print("LDA Confusion Matrix:")
print(lda_conf_matrix)
print(paste("LDA Test Error:", lda_test_error))
#e
qda_fit <- qda(mpg01 ~ displacement + horsepower + weight, data = train_data)
qda_pred <- predict(qda_fit, newdata = test_data)$class
qda_conf_matrix <- table(Predicted = qda_pred, Actual = test_data$mpg01)
qda_test_error <- 1 - mean(qda_pred == test_data$mpg01)

print("QDA Confusion Matrix:")
print(qda_conf_matrix)
print(paste("QDA Test Error:", qda_test_error))
#f
log_reg <- glm(mpg01 ~ displacement + horsepower + weight, data = train_data, family = binomial)
log_probs <- predict(log_reg, newdata = test_data, type = "response")
log_class <- ifelse(log_probs > 0.5, 1, 0)
log_conf_matrix <- table(Predicted = log_class, Actual = test_data$mpg01)
log_test_error <- 1 - mean(log_class == test_data$mpg01)

print("Logistic Regression Confusion Matrix:")
print(log_conf_matrix)
print(paste("Logistic Regression Test Error:", log_test_error))
#g
library(e1071)
nb_fit <- naiveBayes(mpg01 ~ displacement + horsepower + weight, data = train_data)
nb_pred <- predict(nb_fit, newdata = test_data)
nb_conf_matrix <- table(Predicted = nb_pred, Actual = test_data$mpg01)
nb_test_error <- 1 - mean(nb_pred == test_data$mpg01)

print("Naive Bayes Confusion Matrix:")
print(nb_conf_matrix)
print(paste("Naive Bayes Test Error:", nb_test_error))
#h
library(class)
train_X <- as.matrix(train_data[, c("displacement", "horsepower", "weight")])
test_X <- as.matrix(test_data[, c("displacement", "horsepower", "weight")])
train_Y <- train_data$mpg01
k_values <- c(1, 3, 5, 7, 10)
knn_accuracies <- sapply(k_values, function(k) {
  knn_pred <- knn(train_X, test_X, train_Y, k = k)
  1 - mean(knn_pred == test_data$mpg01)
})
knn_results <- data.frame(K = k_values, TestError = knn_accuracies)
print("KNN Test Errors for Different K Values:")
print(knn_results)
best_k <- k_values[which.min(knn_accuracies)]
print(paste("Best K:", best_k))
