# quiz GAMs and GLMs
set.seed(123)
x = seq(-1, 1, length=100)
beta0 = -1
beta1 = 2
logitpi = beta0+beta1*x
p_i = plogis(logitpi)
y_i = rbinom(n=length(p_i),size=1,prob = p_i)
sum(y_i)

# Create a data frame with the variables
data <- data.frame(x = x, y = y_i)
data$y_i <- as.factor(data$y_i)

# Fit logistic regression model
glm_model <- glm(y ~ x, data = data, family = "binomial")
summary(glm_model)


#### Now fit a GAM
library(mgcv)
library(itsadug)

# Fit a GAM
gam_model <- gam(y ~ s(x), data = data, family = "binomial")
summary(gam_model)

# Plot of smoother
itsadug::plot_smooth(gam_model, view = "x", col = "blue", se = TRUE)


# Create a plot of the smooth term for x with seWithMean=TRUE. How does it compare to the default plot? 
plot_smooth(gam_model, view = "x", col = "blue", seWithMean = TRUE)
