#8)
library(ISLR2)
library(splines)
library(ggplot2)
# Load dataset
data(Auto)
# Exploratory analysis
# plotting mpg vs horsepower
ggplot(Auto, aes(x = horsepower, y = mpg)) +
  geom_point(alpha = 0.5) +
  labs(title = "MPG vs Horsepower", x = "Horsepower", y = "MPG") +
  theme_minimal()

# Polynomial regression: Fit quadratic and cubic models
poly_quad_model <- lm(mpg ~ poly(horsepower, 2), data = Auto)
poly_cubic_model <- lm(mpg ~ poly(horsepower, 3), data = Auto)

# Fitting smoothing spline model
spln_model <- smooth.spline(Auto$horsepower, Auto$mpg, df = 5)

# Generating prediction grid
hp_grid <- seq(min(Auto$horsepower), max(Auto$horsepower), length.out = 100)

# Get predictions
poly_quad_preds <- predict(poly_quad_model, newdata = data.frame(horsepower = hp_grid))
poly_cubic_preds <- predict(poly_cubic_model, newdata = data.frame(horsepower = hp_grid))
spln_preds <- predict(spln_model, x = hp_grid)$y

# Creating dataframes for predictions
poly_quad_df <- data.frame(horsepower = hp_grid, mpg = poly_quad_preds)
poly_cubic_df <- data.frame(horsepower = hp_grid, mpg = poly_cubic_preds)
spln_df <- data.frame(horsepower = hp_grid, mpg = spln_preds)

# Plotting polynomial and spline fits
ggplot(Auto, aes(x = horsepower, y = mpg)) +
  geom_point(alpha = 0.5) +  # Scatter plot of data
  geom_line(data = poly_quad_df, aes(x = horsepower, y = mpg), 
            color = "blue", size = 1, linetype = "dashed") +  # Quadratic fit
  geom_line(data = poly_cubic_df, aes(x = horsepower, y = mpg), 
            color = "red", size = 1) +  # Cubic fit
  geom_line(data = spln_df, aes(x = horsepower, y = mpg), 
            color = "green", size = 1.2, linetype = "dotted") +  # Spline fit
  labs(title = "Polynomial and Spline Regression Fits",
       x = "Horsepower", y = "MPG",
       caption = "Blue: Quadratic, Red: Cubic, Green: Spline") +
  theme_minimal()

