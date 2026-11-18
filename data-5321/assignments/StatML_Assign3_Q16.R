library(ISLR2)
library(MASS)
library(e1071)
library(class)
data("Boston")
crime_above_median <- ifelse(Boston$crim > median(Boston$crim), 1, 0)
Boston$crime_above_median <- crime_above_median
log_reg <- glm(crime_above_median ~ ., data = Boston, family = "binomial")
summary(log_reg)
# LDA
lda_reg<- lda(crime_above_median ~ ., data = Boston)
lda_reg
# Naive Bayes
naive_bayes_model <- naiveBayes(as.factor(crime_above_median) ~ ., data = Boston)
naive_bayes_model
# KNN
set.seed(123) # For reproducibility
train <- sample(1:nrow(Boston), 0.7*nrow(Boston)) # 70% for training
train_data <- Boston[train, ]
test_data <- Boston[-train, ]
knn_model <- knn(train_data[, -1], test_data[, -1], train_data$crime_above_median, k = 5)
table(knn_model, test_data$crime_above_median)
