#9)
library(ISLR2)
library(MASS)
library(boot)
library(ggplot2)
library(splines)

#Boston dataset
data(Boston)

# (a) Fit a cubic polynomial regression and report the output
cub_modl <- lm(nox ~ poly(dis, 3), data = Boston)
summary(cub_modl)

# Generate prediction grid
dis_grid <- seq(min(Boston$dis), max(Boston$dis), length.out = 100)
cub_preds <- predict(cub_modl, newdata = data.frame(dis = dis_grid))

cub_df <- data.frame(dis = dis_grid, nox = cub_preds)

# Plot the cubic polynomial fit
ggplot(Boston, aes(x = dis, y = nox)) +
  geom_point(alpha = 0.5) +
  geom_line(data=cub_df, aes(x = dis_grid, y = cub_preds), color = "blue", size = 1.2) +
  labs(title = "Cubic Polynomial Regression Fit", x = "dis", y = "nox") +
  theme_minimal()

# (b) Fit polynomial regressions for degrees 1 to 10 and calculate RSS
rss_values <- numeric(10)
for (d in 1:10) {
  model <- lm(nox ~ poly(dis, d), data = Boston)
  rss_values[d] <- sum(residuals(model)^2)
}
print(data.frame(Degree = 1:10, RSS = rss_values))

# (c) Perform cross-validation to select the optimal polynomial degree
cv_errs <- numeric(10)
set.seed(123)  # For reproducibility
for (d in 1:10) {
  model <- glm(nox ~ poly(dis, d), data = Boston)
  cv_errs[d] <- cv.glm(Boston, model, K = 10)$delta[1]
}
opt_deg <- which.min(cv_errs)
cat("Optimal polynomial degree:", opt_deg, "\n")

# (d) Fit a regression spline with 4 degrees of freedom and plot the fit
spln_mod <- lm(nox ~ bs(dis, df = 4), data = Boston)
spln_preds <- predict(spln_mod, newdata = data.frame(dis = dis_grid))

spln_df <- data.frame(dis = dis_grid, nox = spln_preds)

ggplot(Boston, aes(x = dis, y = nox)) +
  geom_point(alpha = 0.5) +
  geom_line(data = spln_df, aes(x = dis_grid, y = spln_preds), color = "red", size = 1.2) +
  labs(title = "Regression Spline Fit (4 df)", x = "dis", y = "nox") +
  theme_minimal()

# (e) Fit regression splines for different degrees of freedom and report RSS
rss_spln_val <- numeric(10)
for (df in 3:10) {
  model <- lm(nox ~ bs(dis, df = df), data = Boston)
  rss_spln_val[df] <- sum(residuals(model)^2)
}
print(data.frame(Degrees_Freedom = 3:10, RSS = rss_spln_val[3:10]))

# (f) Perform cross-validation to select the best degrees of freedom for splines
cv_spln_errs <- numeric(10)
set.seed(123)
for (df in 3:10) {
  model <- glm(nox ~ bs(dis, df = df), data = Boston)
  cv_spln_errs[df] <- cv.glm(Boston, model, K = 10)$delta[1]
}
opt_df <- which.min(cv_spln_errs[3:10]) + 2
cat("Optimal degrees of freedom for spline:", opt_df, "\n")

