library(gam)
library(ISLR2)
library(leaps)
library(caret)

#a
set.seed(123)
data(College)

train_idx <- createDataPartition(College$Outstate, p = 0.7, list = FALSE)
train <- College[train_idx, ]
test <- College[-train_idx, ]

regfit_fwd <- regsubsets(Outstate ~ ., data = train, nvmax = 10, method = "forward")
summary_fwd <- summary(regfit_fwd)

model_size <- which.max(summary_fwd$adjr2)
best_pred <- names(coef(regfit_fwd, model_size))[-1]

print(best_pred) # Selected features

#b
gam_model <- gam(Outstate ~ s(Room.Board, df=4) + s(PhD, df=4) + s(Enroll, df=4) + Private, data = train)
par(mfrow = c(2,2))
plot(gam_model, se = TRUE, col = "blue")

#c
pred_gam <- predict(gam_model, newdata = test)

mse <- mean((pred_gam - test$Outstate)^2)
print(mse) # Model error on test data
