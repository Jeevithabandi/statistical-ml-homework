library(ISLR2)
data("Carseats")
model_a <- lm(Sales ~ Price + Urban + US, data = Carseats)
summary(model_a)
#e
model_b <- lm(Sales ~ Price + US, data = Carseats)
summary(model_b)
#f
summary(model_a)$r.squared
summary(model_b)$r.squared 
#g
confint(model_b, level = 0.95)
#h
par(mfrow = c(2, 2))
plot(model_a) 
