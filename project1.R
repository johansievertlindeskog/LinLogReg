##### Project 1 #####

rm(list = ls())
load("Project_1/weather.rda")

##### 1 (a) #####
x <- weather$temp # average monthly temperature (C)
Y <- weather$rain # total monthly precipitation (mm)

### Fit linear regression model####

(model <- lm(Y ~ x, data = weather))
(modelSummary <- summary(model))

library(ggplot2) # activate ggplot2 commands

(
  plotData <- ggplot(data = weather, 
                      aes(x = x, y = Y)) + 
    geom_point(size = 1) +
    xlab("Temperature (°C)") +
    ylab("Precipitation of rain (mm)") +
    labs(title = "Rain vs temperature") +
    theme(text = element_text(size = 16))
)

### Add the fitted line to the data plot####
# without confidence interval: se = FALSE:
(
  plotData + 
    geom_smooth(method = lm, se = FALSE) +
    labs(caption = "Rain vs temperature, with fitted line")
)

### Basic residual analysis####

### Calculate the prediction fit, pred and conf interval####
rainPred <- 
  cbind(weather,
        linconf = predict(model, interval = "confidence"),
        linpred = predict(model, interval = "prediction"))

### Add the residuals to the predicted data
rainPred$e <- model$residuals
head(rainPred)

# Save the max-value in order to make the y-axis symmetrical in the plots.
(eMax <- max(abs(rainPred$e)))
(eLims <- c(-eMax, eMax))

# Plot residuals against yhat, add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.
ggplot(data = rainPred, 
       aes(x = linpred.fit, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = eLims) +
  xlab("Predicted precipitation") +
  ylab("Residual") +
  labs(tag = "B") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 18))

# Make a normal qq-plot of the residuals.
ggplot(data = rainPred, 
       aes(sample = e)) +
  geom_qq(size = 3) +
  geom_qq_line() +
  labs(tag = "C") +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 18))

# Hisogram of the residuals:
ggplot(data = rainPred,
       aes(x = e)) +
  geom_histogram(bins = 100) +
  xlab("Residuals") +
  labs(title = "Histogram of residuals") +
  theme(text = element_text(size = 18))

### Conclusion: It is not a good model! Residuals dont follow Q-Q plot and
### histogram shows not normal distributed variables (very expected)


##### 1 (b) #####
# try model with log(rain) instead i.e.y = B0 + B1 * x + epsilon -> 
# log(Y) = B0 + B1 * x + epsilon
y = sqrt(Y)
plot(x, y, main = "log(rain) vs temperature", xlab = "Temperature (°C)", ylab = "Monthly precipitation of rain in log-scale (log(mm))")

# Fit log-linear regression model
(loglinModel <- lm(y ~ x, data = weather))
modelSummary <- summary(loglinModel)

(
  plotLogData <- ggplot(data = weather, 
                          aes(x = x, y = y)) + 
    geom_point(size = 1) +
    xlab("Temperature (°C)") +
    ylab("Monthly precipitation of rain in log-scale (log(mm))") +
    labs(title = "log(rain precipitation) vs temperature") +
    theme(text = element_text(size = 16))
)

### Add the fitted line do the data plot####
# without confidence interval: se = FALSE:
(
  plotLogData + 
    geom_smooth(method = lm, se = FALSE) +
    labs(caption = "log(rain) data and fitted line")
)

### Basic residual analysis####

rainPred <- 
  cbind(weather,
        linconf = predict(loglinModel, interval = "confidence"),
        linpred = predict(loglinModel, interval = "prediction"))

rainPred$e <- loglinModel$residuals
head(rainPred)
(eMax <- max(abs(rainPred$e)))
(eLims <- c(-eMax, eMax))

ggplot(data = rainPred, 
       aes(x = linpred.fit, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = eLims) +
  xlab("Predicted precipitation") +
  ylab("Residual") +
  labs(tag = "B") +
  labs(title = "Residuals from loglinModel vs predicted values Y-hat") +
  theme(text = element_text(size = 18))

# Make a normal qq-plot of the residuals.
ggplot(data = rainPred, 
       aes(sample = e)) +
  geom_qq(size = 3) +
  geom_qq_line() +
  labs(tag = "C") +
  labs(title = "Normal Q-Q-plot of the residualss") +
  theme(text = element_text(size = 18))

# Hisogram of the residuals:
ggplot(data = rainPred,
       aes(x = e)) +
  geom_histogram(bins = 20) +
  xlab("Residuals") +
  labs(title = "Histogram of residuals") +
  theme(text = element_text(size = 18))

### Conclusion: Still not a good model! Dont follow Q-Q plot and histogram
### shows not normal distributed variables. Should it be much better though???

##### 1 (c) #####
# Compare the results and plots for the two models and explain what features in the plots
# caused you to decide which model is the better (less bad) one. - both look shit

# Unreasonable to transform the temperature right? ask if log-lin model is good, also ask
# if the QQ-plots are suppose to look lite shit (not following the line)


##### 1 (d) #####



##### 1 (e) #####



##### 1 (f) #####




