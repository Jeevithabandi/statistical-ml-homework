library(ISLR2)
attach(Weekly)
#a
pairs(Weekly)
cor(Weekly[,-9])
#(b)
log_reg <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = binomial)
summary(log_reg)
#c
probs_log <- predict(log_reg, type = "response")
pred_log <- rep("Down", length(probs_log))
pred_log[probs_log >.5]= "Up"
table(pred_log, Weekly$Direction)
#d
train = (Year<2008)
W_2008 <-Weekly[!train,]
log_reg<-glm(Direction~Lag2, data=Weekly,family=binomial, subset=train)
probs_log= predict(log_reg, W_2008, type = "response")
pred_log <- rep("Down", length(probs_log))
pred_log[probs_log > 0.5] = "Up"
Direction_2008 = Direction[!train]
table(pred_log, Direction_2008)
mean(pred_log == Direction_2008)
#e
library(MASS)
lda_fit<-lda(Direction~Lag2, data=Weekly,family=binomial, subset=train)
lda_pred<-predict(lda_fit, W_2008)
table(lda_pred$class, Direction_2008)
mean(lda_pred$class==Direction_2008)
#f
qda_fit = qda(Direction ~ Lag2, data = Weekly, subset = train)
qda_pred = predict(qda_fit, W_2008)$class
table(qda_pred, Direction_2008)
mean(qda_pred==Direction_2008)
#g
library(class)
Week.train=as.matrix(Lag2[train])
Week.test=as.matrix(Lag2[!train])
train.Direction =Direction[train]
set.seed(1)
Weekknn.pred=knn(Week.train,Week.test,train.Direction,k=1)
table(Weekknn.pred,Direction_2008)
mean(Weekknn.pred == Direction_2008)
#h
library(e1071)
nb_fit <- naiveBayes(Direction ~ Lag2, data = Weekly, subset = train)
nb_pred <- predict(nb_fit, W_2008)
table(nb_pred, Direction_2008)
mean(nb_pred == Direction_2008)
#i
accuracy_log <- mean(pred_log == Direction_2008)         # Logistic Regression
accuracy_lda <- mean(lda_pred$class == Direction_2008)   # LDA
accuracy_qda <- mean(qda_pred == Direction_2008)         # QDA
accuracy_knn <- mean(Weekknn.pred == Direction_2008)     # KNN
accuracy_nb <- mean(nb_pred == Direction_2008)           # Naive Bayes

accuracy_results <- data.frame(
  Method = c("Logistic Regression", "LDA", "QDA", "KNN (K=1)", "Naive Bayes"),
  Accuracy = c(accuracy_log, accuracy_lda, accuracy_qda, accuracy_knn, accuracy_nb)
)
print("Accuracy Comparison of Methods:")
print(accuracy_results)
best_method <- accuracy_results[which.max(accuracy_results$Accuracy), ]
print("Best Performing Method:")
print(best_method)

#j
log_reg_exp <- glm(Direction ~ Lag1 * Lag2 + Lag3 + I(Lag2^2) + Volume, 
                   data = Weekly, family = binomial, subset = train)
probs_log_exp <- predict(log_reg_exp, W_2008, type = "response")
pred_log_exp <- rep("Down", length(probs_log_exp))
pred_log_exp[probs_log_exp > 0.5] <- "Up"
conf_matrix_log_exp <- table(pred_log_exp, Direction_2008)
accuracy_log_exp <- mean(pred_log_exp == Direction_2008)
set.seed(1)
k_values <- c(1, 3, 5, 7, 10)
knn_accuracies <- sapply(k_values, function(k) {
  knn_pred <- knn(Week.train, Week.test, train.Direction, k = k)
  mean(knn_pred == Direction_2008)
})
knn_results <- data.frame(K = k_values, Accuracy = knn_accuracies)
print("KNN Results for Different K Values:")
print(knn_results)
all_results <- rbind(
  accuracy_results,
  data.frame(Method = "Logistic Regression (Expanded)", Accuracy = accuracy_log_exp),
  data.frame(Method = paste("KNN (K =", knn_results$K[which.max(knn_results$Accuracy)], ")"),
             Accuracy = max(knn_results$Accuracy))
)

print("Combined Results of All Methods and Transformations:")
print(all_results)
best_method_exp <- all_results[which.max(all_results$Accuracy), ]
print("Best Method with Experiments:")
print(best_method_exp)
