library(ISLR2)
library(boot)
data(Default)
#a
log_model <- glm(default ~ income + balance, data = Default, family = binomial)
summary(log_model)
summary(log_model)$coefficients[, "Std. Error"]
#b
boot.fn <- function(data, index) {
  fit <- glm(default ~ income + balance, data = data, family = binomial, subset = index)
  return (coef(fit))
}
#c
set.seed(42)
boot_results <- boot(Default, boot.fn, R = 1000)
boot_results$t0 
apply(boot_results$t, 2, sd)
