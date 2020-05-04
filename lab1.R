##### Lab 1 #####

rm(list = ls())
load("Data/Pb_mossa.rda")
#summary(Pb_mossa)
#head(Pb_mossa)

##### 1 (a) #####
x <- Pb_mossa$year # year
Y <- Pb_mossa$Pb # Pb concentration
#plot(x, Y, main = "Pb vs year", xlab = "Year", ylab = "Pb concentration (mg/kg)")

# Does the relationship look linear? - No, more like an exponential decay.
# Should it be linear? - Probably not, more likely it will decline faster
# in the beginning when the concentration is larger.

##### 1 (b) #####
x = I(x - 1975) # year starts at zero (1975)
#plot(x, Y, main = "Plot of Pb against time", xlab ="time (year)", ylab = "Pb (mg/kg)")

##### 1 (c) #####
# Fit linear regression model
model <- lm(Y ~ x, data = Pb_mossa)
model_summary <- summary(model)

# Beta (B0 = 16.962, B1 = -0.497)
beta_estimates <- model$coefficients
B0 <- beta_estimates[1]
B1 <- beta_estimates[2]

# Standard errors - estimation of standard deviation
se_B0 <- model_summary$coefficients[1,2]
se_B1 <- model_summary$coefficients[2,2]


# hur räknar vi fram standars errors? vi har ju endast plockat ut dem här...


# 95 % confidence intervals
confInt <- confint(model)         # always confidence interval for the model!

##### 1 (d) #####
average_1975 <- B0 + B1*0 # Average 1975 = 17 mg/kg

##### 1 (e) #####
# The concentration changes according to B1 = -0.497 mg/kg/year (i.e. B1 is the slope)

##### 1 (f) #####
library(ggplot2)

  plot_data <- ggplot(data = Pb_mossa, 
    aes(x = x, y = Y)) + 
    geom_point(size = 1) +
    xlab("Year (0 = 1975)") +
    ylab("Pb concentration") +
    labs(title = "Pb concentration vs year") +
    theme(text = element_text(size = 16))

### Add the fitted line to the data plot####
# without confidence interval: se = FALSE:
  # plot_data + 
  #   geom_smooth(method = lm, se = FALSE) +
  #   labs(caption = "Pb data and fitted line")

### Add fitted line WITH confidence interval####
  plot_conf <- plot_data + 
    geom_smooth(method = lm) +
    labs(caption = "Pb data, fitted line and 95% confidence interval")

### Calculate and add the prediction interval####
Pb_pred <- 
  cbind(Pb_mossa, 
        pred = predict(model, interval = "prediction"))
head(Pb_pred)

# Add the prediction interval to the confidence 
# interval plot,
(
  plot_conf +
    geom_line(data = Pb_pred, aes(y = pred.lwr),
              color = "red", linetype = "dashed", size = 1) +
    geom_line(data = Pb_pred, aes(y = pred.upr),
              color = "red", linetype = "dashed", size = 1) +
    labs(caption = "Pb data, fitted line, 95% confidence and prediction intervals")
)

# Does it look reasonable? - Yes, according to the model! But it is obviously wrong since level of Pb is below zero in the end.

##### 1 (g) #####
### Basic residual analysis####

            # jag l?gger till residuals fr?n den anpassade modellen till pred???

### Add the residuals to the predicted data
Pb_pred$e <- model$residuals
head(Pb_pred)

# Save the max-value in order to make the y-axis symmetrical in the plots.
max.e <- max(abs(Pb_pred$e))
Pb_elims <- c(-max.e, max.e)

# Plot residuals against yhat, add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.
# ggplot(data = Pb_pred, 
#        aes(x = pred.fit, y = e)) +
#   geom_point(size = 3) +
#   geom_hline(yintercept = 0) +
#   expand_limits(y = Pb_elims) +
#   xlab("Predicted Pb concentration") +
#   ylab("Residual") +
#   labs(tag = "B") +
#   labs(title = "Residuals vs predicted values Y-hat") +
#   theme(text = element_text(size = 18))

##### 1 (h) #####
# Make a normal qq-plot of the residuals.
# ggplot(data = Pb_pred, 
#        aes(sample = e)) +
#   geom_qq(size = 3) +
#   geom_qq_line() +
#   labs(tag = "C") +
#   labs(title = "Normal Q-Q-plot of the residuals") +
#   theme(text = element_text(size = 18))

# Hisogram of the residuals:
# ggplot(data = Pb_pred,
#        aes(x = e)) +
#   geom_histogram(bins = 50) +
#   xlab("Residuals") +
#   labs(title = "Histogram of residuals") +
#   theme(text = element_text(size = 18))

# Obviously not a good model...

##### 1 (i) #####
# Get confidence and prediction interval for x0 = 40 and 45
Pb_x0 <- data.frame(x = c(40, 45))
Pb_y0_pred <- cbind(Pb_x0, pred = predict(model, Pb_x0, interval = "prediction"))

##### 2 (a) #####
# The concentration of lead, when there are no new additions, should decrease at a constant
# rate, e.g., 10% per year.

# What type of model would this indicate and what type of
# transformations would be natural for Y and/or x? - A decaying exponential model, use log(Y).

##### 2 (b) #####
x <- Pb_mossa$year
x = I(x - 1975)
Y <- Pb_mossa$Pb # Pb concentration
y = log(Y)
#plot(x, y, main = "log(Pb) vs year", xlab = "Year", ylab = "Pb concentration in log-scale (log(mg/kg))")

# Does this seem like a linear relationship? Yes kind of.

##### 2 (c) #####
# Fit log-linear regression model
loglin_model <- lm(y ~ x, data = Pb_mossa)
model_summary <- summary(loglin_model)

  plot_log_data <- ggplot(data = Pb_mossa, 
                      aes(x = x, y = y)) + 
    geom_point(size = 1) +
    xlab("Year (0 = 1975)") +
    ylab("log(Pb) concentration") +
    labs(title = "log(Pb) concentration vs year") +
    theme(text = element_text(size = 16))


# Beta (B0 = 3.06 , B1 = -0.0799)
beta_estimates <- loglin_model$coefficients
B0 <- beta_estimates[1]
B1 <- beta_estimates[2]

# Standard errors - estimation of standard deviation
se_B0 <- model_summary$coefficients[1,2]
se_B1 <- model_summary$coefficients[2,2]

# 95 % confidence intervals
confInt <- confint(loglin_model)

##### 2 (d) #####
### Add fitted line WITH confidence interval####
  plot_conf <- plot_log_data + 
    geom_smooth(method = lm) +
    labs(caption = "log(Pb) data, fitted line and 95% confidence interval")

### Calculate and add the prediction interval####
  Pb_pred <- 
  cbind(Pb_mossa, 
        logfit = predict(loglin_model), 
        logconf = predict(loglin_model, interval = "confidence"),
        logpred = predict(loglin_model, interval = "prediction"))

head(Pb_pred)

###   om man tar bort logfit = predict(loglin_model) s? h?nder inget, den datan finns i logpred.fit,
###   s? varf?r ska man ens ha logfit??

# Plot data, fitted line, conf and pred interval (only adding pred interval here)
(
  plot_conf +
    geom_line(data = Pb_pred, aes(y = logpred.lwr),
              color = "red", linetype = "dashed", size = 1) +
    geom_line(data = Pb_pred, aes(y = logpred.upr),
              color = "red", linetype = "dashed", size = 1) +
    labs(caption = "log(Pb) data, fitted line, 95% confidence and prediction intervals")
)

# How does this look compared to the original linear model? Better but not great.

##### 2 (e) #####
### Basic residual analysis####

### Add the residuals to the predicted data
Pb_pred$e <- loglin_model$residuals
head(Pb_pred)

# Save the max-value in order to make the y-axis symmetrical in the plots.
(max.e <- max(abs(Pb_pred$e)))
(Pb_elims <- c(-max.e, max.e))

# Plot residuals against yhat, add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.
# ggplot(data = Pb_pred, 
#        aes(x = logpred.fit, y = e)) +
#   geom_point(size = 3) +
#   geom_hline(yintercept = 0) +
#   expand_limits(y = Pb_elims) +
#   xlab("Predicted log(Pb) concentration") +
#   ylab("Residual") +
#   labs(tag = "B") +
#   labs(title = "Residuals vs predicted values Y-hat") +
#   theme(text = element_text(size = 18))

# Make a normal qq-plot of the residuals.
# ggplot(data = Pb_pred, 
#        aes(sample = e)) +
#   geom_qq(size = 3) +
#   geom_qq_line() +
#   labs(tag = "C") +
#   labs(title = "Normal Q-Q-plot of the residuals") +
#   theme(text = element_text(size = 18))

# Hisogram of the residuals:
# ggplot(data = Pb_pred,
#        aes(x = e)) +
#   geom_histogram(bins = 20) +
#   xlab("Residuals") +
#   labs(title = "Histogram of residuals") +
#   theme(text = element_text(size = 18))

# Both Q-Q-plot and histogram looks fairly normal distributed, some outlier in the histogram

#Note: the residual analysis should always be done on the fitted linear model with the
#additive normally distributed errors.  Vad exakt menar de? vad skulle man annars g?ra av misstag??

##### 2 (f) #####
# Prediction interval for year 2015 and 2020:
(Pb_y0_log_pred <- cbind(Pb_x0,
                         predlog = predict(loglin_model, Pb_x0,
                                           interval = "prediction")))
# 2015 Prediction interval: (-1.25, 0.98)
# 2020 Prediction interval: (-1.65, 0.58)

### TODO ###


##### 3 (a) #####
# Concentration of Pb over time
# y = B0 + B1 * x + epsilon -> log(Y) = B0 + B1 * x + epsilon
# Transformation -> Y = exp(B0) * exp(B1)^x * exp(epsilon) = a * b^x * exp(epsilon),
# where a = exp(B0) and b = exp(B1)

##### 3 (b) #####
# Estimates of a and b
a <- exp(beta_estimates[1]) # a = 21.3
b <- exp(beta_estimates[2]) #b = 0.923

# 95% confidence intervals
log_confInt <- exp(confInt)

##### 3 (c) #####
# Average Pb concentration in 1975 according to the model
a * b^0
# 21.3 +- 1.77 mg/kg (95 % confidence interval)

##### 3 (d) #####
#  The rate of decrease in lead concentration (%/year) according to the estimated model:
1 - b # Rate of decrease: 7.7 %/year

##### 3 (e) #####
(
  plot_pred <- plot_data +      # obs do NOT use plot_log_data since this is the logdata, 
    geom_line(data = Pb_pred,   # we want to use the original data i.e. plot_data
              aes(y = exp(logfit)),
              color = "blue", size = 1) +
    geom_ribbon(data = Pb_pred,
                aes(ymin = exp(logconf.lwr), 
                    ymax = exp(logconf.upr)),
                alpha = 0.2) +
    geom_line(data = Pb_pred, 
              aes(y = exp(logpred.lwr)),
              color = "red", linetype = "dashed",
              size = 1) +
    geom_line(data = Pb_pred, 
              aes(y = exp(logpred.upr)),
              color = "red", linetype = "dashed",
              size = 1) +
    labs(caption = "fitted line and 95% conf. and pred. intervals")
)

# How does this look compared to the first model? - Much better!

##### 3 (f) #####


x <- Pb_mossa$year

  plot_data <- ggplot(data = Pb_mossa, 
                      aes(x = x, y = Y)) + 
    geom_point(size = 1) +
    xlab("Year (0 = 1975)") +
    ylab("Pb concentration") +
    labs(title = "Pb concentration vs year") +
    theme(text = element_text(size = 16))

(
  plot_pred <- plot_data +      # obs do NOT use plot_log_data since this is the logdata, 
    geom_line(data = Pb_pred,   # we want to use the original data i.e. plot_data
              aes(y = exp(logfit)),
              color = "blue", size = 1) +
    geom_ribbon(data = Pb_pred,
                aes(ymin = exp(logconf.lwr), 
                    ymax = exp(logconf.upr)),
                alpha = 0.2) +
    geom_line(data = Pb_pred, 
              aes(y = exp(logpred.lwr)),
              color = "red", linetype = "dashed",
              size = 1) +
    geom_line(data = Pb_pred, 
              aes(y = exp(logpred.upr)),
              color = "red", linetype = "dashed",
              size = 1) +
    labs(caption = "fitted line and 95% conf. and pred. intervals")
)

##### 3 (g) #####
(
  Pb_y0_trans_pred <- cbind(Pb_x0,
                  predtrans = exp(predict(loglin_model, Pb_x0,
                  interval = "prediction")))
  )

