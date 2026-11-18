#6th QUESTION
#a)
library(ISLR2)
library(boot)
library(ggplot2)

#Load Wage Dataset
data(Wage)
max_deg<-10
cv_err<-rep(NA, max_deg)
#perform cross-validation to select the optimal polynomial degree
set.seed(123)
for (d in 1:max_deg) {
  fit <- glm(wage ~ poly(age, d), data = Wage)
  cv_err[d] <- cv.glm(Wage, fit, K = 10)$delta[1]
}
# find optimal degree
opt_deg <- which.min(cv_err)
cat("Optimal polynomial degree selected by cross-validation:", opt_deg, "\n")
# Fit the polynomial regression model using the optimal degree
final_mod <- lm(wage ~ poly(age, opt_deg), data = Wage)
# Perform ANOVA to compare polynomial models
anova_mods <- list()
for (d in 1:max_deg) {
  anova_mods[[d]] <- lm(wage ~ poly(age, d), data = Wage)
}
anova_results <- do.call(anova, anova_mods)
print(anova_results)

# Plot the polynomial regression fit
age_grid <- seq(min(Wage$age), max(Wage$age), length.out = 100)
preds <- predict(final_mod, newdata = data.frame(age = age_grid))
# Create a dataframe for predictions
pred_df <- data.frame(age = age_grid, wage = preds)
ggplot(Wage, aes(x = age, y = wage)) +
  geom_point(alpha = 0.5) +  # Scatter plot of data
  geom_line(data = pred_df, aes(x = age_grid, y = preds), color = "blue", size = 1) +  
  # Polynomial fit
  labs(title = paste("Polynomial Regression Fit (Degree", opt_deg, ")"),
       x = "Age", y = "Wage") +
  theme_minimal()


#b)Fit a step function to predict wage using age, and perform cross-
#validation to choose the optimal number of cuts. Make a plot of
#the fit obtained.
# Defining maximum number of cuts to test
max_cuts <- 10
cv_err_step <- rep(NA, max_cuts)

set.seed(123)  
for (k in 2:max_cuts) {  # Start from 2 bins (at least one cut)
  Wage$age_cut <- cut(Wage$age, breaks = k)  # Create step function 
  fit <- glm(wage ~ age_cut, data = Wage)  # Fit model using step function
  cv_err_step[k] <- cv.glm(Wage, fit, K = 10)$delta[1]  # 10-fold CV error
}

# Finding the optimal number of cuts
opt_cuts <- which.min(cv_err_step)
cat("Optimal number of cuts selected by cross-validation:", opt_cuts, "\n")

# Fitting final model with the optimal number of cuts
Wage$age_cut <- cut(Wage$age, breaks = opt_cuts)
final_step_model <- lm(wage ~ age_cut, data = Wage)

# Generation of preds for plotting
age_grid <- seq(min(Wage$age), max(Wage$age), length.out = 100)
pred_grid <- cut(age_grid, breaks = opt_cuts)
step_preds <- predict(final_step_model, newdata = data.frame(age_cut = pred_grid))

# Creating dataframe for step fn preds
step_df <- data.frame(age = age_grid, wage = step_preds)

# Plotting step function fit
ggplot(Wage, aes(x = age, y = wage)) +
  geom_point(alpha = 0.5) +  # Scatter plot of data
  geom_step(data = step_df, aes(x = age, y = wage), color = "red", size = 1.2) +  # Step function fit
  labs(title = paste("Step Function Fit (", opt_cuts, " Cuts)"),
       x = "Age", y = "Wage") +
  theme_minimal()

