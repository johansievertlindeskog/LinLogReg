##### Project 1 - part 1  #####

rm(list = ls())
load("Data/weather.rda")

##### 1 Precipitation as a function of temperature####

##### 1 (a) #####
# Fit linear regression model
(model <- lm(rain ~ temp, data = weather))
(modelSummary <- summary(model))

library(ggplot2) # activate ggplot2 commands

# Plotting rain vs temp
plotData <- ggplot(data = weather, 
                   aes(x = temp, y = rain)) + 
  geom_point(size = 1) +
  xlab("Temperature (°C)") +
  ylab("Precipitation of rain (mm)") +
  labs(title = "Rain vs temperature") +
  theme(text = element_text(size = 18))

# Add the fitted line to the data plot
# without confidence interval: se = FALSE:
plotData + 
  geom_smooth(method = lm, se = FALSE) #+ labs(caption = "Rain vs temperature, fitted line")

# Basic residual analysis
# Calculate the prediction fit, pred and conf interval
rainPred <- 
  cbind(weather,
        conf = predict(model, interval = "confidence"),
        pred = predict(model, interval = "prediction"))

### Add the residuals to the predicted data
rainPred$e <- model$residuals
head(rainPred)

# Save the max-value in order to make the y-axis symmetrical in the plots.
eMax <- max(abs(rainPred$e))
eLims <- c(-eMax, eMax)

# Plot residuals against yhat, add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.
residYhat <- ggplot(data = rainPred, 
                    aes(x = pred.fit, y = e)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0) +
  expand_limits(y = eLims) +
  xlab("Predicted precipitation of rain (mm)") +
  ylab("Residual") +
  labs(title = "Residuals vs predicted values") +
  theme(text = element_text(size = 12))

# Make a normal qq-plot of the residuals.
residQQ <- ggplot(data = rainPred, 
                  aes(sample = e)) +
  geom_qq(size = 1) +
  geom_qq_line() +
  xlab("Theoretical") +
  ylab("Sample") +
  labs(title = "Normal QQ-plot of the residuals") +
  theme(text = element_text(size = 12))

# Hisogram of the residuals:
residHisto <- ggplot(data = rainPred,
                     aes(x = e)) +
  geom_histogram(bins = 50) +
  xlab("Residuals") +
  ylab("Count") + 
  labs(title = "Histogram of residuals") +
  theme(text = element_text(size = 12))

### Conclusion: Not a good model! Residuals dont follow QQ-plot and
### histogram shows not normal distributed variables (very expected).

##### 1 (b) #####
# Model with log(rain) instead i.e. log(Y) = B0 + B1 * x + epsilon
# Fit log-linear regression model
(loglinModel <- lm(log(rain) ~ temp, data = weather))
loglinModelSummary <- summary(loglinModel)

plotlogData <- ggplot(data = weather, 
                      aes(x = temp, y = log(rain))) + 
  geom_point(size = 1) +
  xlab("Temperature (°C)") +
  ylab("Precipitation of rain (ln(mm))") +
  labs(title = "Rain vs temperature") +
  theme(text = element_text(size = 18))

# Add the fitted line do the data plot
# without confidence interval: se = FALSE:
plotlogData + 
  geom_smooth(method = lm, se = FALSE)

# Basic residual analysis
logRainPred <- 
  cbind(weather,
        conf = predict(loglinModel, interval = "confidence"),
        pred = predict(loglinModel, interval = "prediction"))

logRainPred$e <- loglinModel$residuals
head(logRainPred)

# Save the max-value in order to make the y-axis symmetrical in the plots.
eMax <- max(abs(logRainPred$e))
eLims <- c(-eMax, eMax)

# Plot residuals against yhat, add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.
residYhatTrans <- ggplot(data = logRainPred, 
                         aes(x = pred.fit, y = e)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0) +
  expand_limits(y = eLims) +
  xlab("Predicted precipitation of rain (ln(mm))") +
  ylab("Residual") +
  labs(title = "Residuals vs predicted values") +
  theme(text = element_text(size = 12))

# Make a normal qq-plot of the residuals.
residQQTrans <- ggplot(data = logRainPred, 
                       aes(sample = e)) +
  geom_qq(size = 1) +
  geom_qq_line() +
  xlab("Theoretical") +
  ylab("Sample") +
  labs(title = "Normal QQ-plot of the residuals") +
  theme(text = element_text(size = 12))

# Hisogram of the residuals:
residHistoTrans <- ggplot(data = logRainPred,
                          aes(x = e)) +
  geom_histogram(bins = 50) +
  xlab("Residuals") +
  ylab("Count") +
  labs(title = "Histogram of residuals") +
  theme(text = element_text(size = 12))

#install.packages("gridExtra")
library("gridExtra")

# 2x3 plot for the report
grid.arrange(residYhat, residYhatTrans, residQQ, residQQTrans,
             residHisto, residHistoTrans, nrow=3, ncol=2)

##### 1 (c) #####
# Conclusion: Better QQ and histogram, also better residuals vs Yhat!

##### 1 (d) #####
# Transformed model: Log-lin regression model
# y = B0 + B1*x + epsilon --> log(Y) = B0 + B1*x + epsilon
# Transformed model in untransformed scale
# Y = exp(B0) * exp(B1)^x * exp(epsilon) = a * b^x * exp(epsilon)
# where a = exp(B0) and b = exp(B1)

B0 <- loglinModel$coefficients[1]
B1 <- loglinModel$coefficients[2]
confIntlog <- confint(loglinModel)

# Standard error for residual and beta estimates
sigma <- loglinModelSummary$sigma
seB0 <- loglinModelSummary$coefficients[1,2]
seB1 <- loglinModelSummary$coefficients[2,2]

a <- exp(B0)
b <- exp(B1)
confIntExp <- exp(confIntlog)

# Do NOT report standard error for a, b etc., only for the beta estimates!

# When the temperature changes with one degree Celcius the
# precipitation increases with about 3 % (variable b)

##### 1 (e) #####

# Precipitation against temperature (original data) together with loglinModel,
# its confidence interval and prediction interval
plotLogPred <- plotData +      # obs do NOT use plot_log_data since this is the logdata, 
  geom_line(data = logRainPred,   # we want to use the original data i.e. plot_data
            aes(y = exp(pred.fit)),
            color = "blue", size = 1) +
  geom_ribbon(data = logRainPred,
              aes(ymin = exp(conf.lwr), 
                  ymax = exp(conf.upr)),
              alpha = 0.2) +
  geom_line(data = logRainPred, 
            aes(y = exp(pred.lwr)),
            color = "red", linetype = "dashed",
            size = 1) +
  geom_line(data = logRainPred, 
            aes(y = exp(pred.upr)),
            color = "red", linetype = "dashed",
            size = 1)

##### 1 (f) #####
# Predict Y0
x0 <- data.frame(temp = c(5))
y0 <- cbind(x0, predict(loglinModel, x0, se.fit = TRUE),
            pred = exp(predict(loglinModel, x0,
                               interval = "prediction")),)
# See prediction interval, between 8.1 and 146.3

# OBS använd "predict(loglinModel, x0, se.fit = TRUE)" endast vid transformerad scale, inte
# vid e upphöjt till, precis på samma sätt som att vi ej har s.e. för a=exp(B0), b=exp(B1) osv

##### Project 1 - part 2  #####

# Initializing from part 1
rm(list = ls())
load("Data/weather.rda")

# Fit log-linear regression model
(loglinModel <- lm(log(rain) ~ temp, data = weather))
loglinModelSummary <- summary(loglinModel)

##### 2.1 Temperature again####

##### 2 (a) #####

# t-test for location

# Degrees of freedom is nbr of data points - model parameters (betas)

# The t-test for different location is found in:
loglinModelSummary$coefficients

# compare t-value with t-quantile:
# upper alpha/2-t-quantile with df = 1089:
(tvalue <- loglinModelSummary$coefficients[2, 3])
qt(1 - 0.05/2, 1089)

# abs of tvalue > qt => reject H_0!

# calculate P-value:
# 2 * P(|t| > |tvalue|) to cover both tails:
pValue <- 2*pt(abs(tvalue), 1089, lower.tail = FALSE)

# pValue = 1.351846e-29 < 0.05 => reject H_0!

# compare with summary
loglinModelSummary$coefficients[2, 4] # same p-value as calculated

##### 2.2 Temperature and pressure####

##### 2 (b) #####
library(ggplot2) # activate ggplot2 commands

# Plotting rain vs temp
(
  plotRainTemp <- ggplot(data = weather, 
                         aes(x = temp, y = log(rain) )) + 
    geom_point(size = 1) +
    xlab("Temperature (°C)") +
    ylab("Precipitation of rain (ln(mm))") +
    labs(title = "Rain vs temperature") +
    theme(text = element_text(size = 18))
)

# Plotting rain vs pressure
(
  plotRainPressure <- ggplot(data = weather, 
                             aes(x = pressure, y = log(rain) )) + 
    geom_point(size = 1) +
    xlab("Pressure (hPa)") +
    ylab("Precipitation of rain (ln(mm))") +
    labs(title = "Rain vs pressure") +
    theme(text = element_text(size = 18))
)

# Does is seem like there might be a linear relationship between precipitation
# and air pressure? - Yes.
# Does it improve if we use the same transformations as in 1.(b)? - Yes, log(rain).

# Plotting temp vs pressure
(
  plotTempPressure <- ggplot(data = weather, 
                             aes(x = pressure, y = temp)) + 
    geom_point(size = 1) +
    xlab("Pressure (hPa)") +
    ylab("Temperature (°C)") +
    labs(title = "Temperature vs pressure") +
    theme(text = element_text(size = 18))
)

# Does it look like there might be a strong linear relationship between temperature and
# pressure, that might cause problems? - No, small linear relationship.

##### 2 (c) #####
# Fit MULTIPLE linear regression model
(multiModel <- lm(log(rain) ~ temp + pressure, data = weather))
(multiModelSummary <- summary(multiModel)) # pressure significant

# Beta estimates and CI (better way for displaying estimates and CI)
cbind(multiModel$coefficients,
      confint(multiModel))

# Notera ett hogt B0 eftersom att modellen ej ar anpassad vid laga
# tryck (tryck = 0 vid intercept)

##### 2 (d) #####
# Basic residual analysis
multiModelPred <- cbind(weather,
                        conf = predict(multiModel, interval = "confidence"),
                        pred = predict(multiModel, interval = "prediction"))

### Add the residuals to the predicted data
multiModelPred$e <- multiModel$residuals
head(multiModelPred)

# Save the max-value in order to make the y-axis symmetrical in the plots.
max.e <- max(abs(multiModelPred$e))
rainElimits <- c(-max.e, max.e)

# Plot residuals vs yhat
residVsYhat <- ggplot(data = multiModelPred,
                      aes(x = pred.fit, y = e)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0) +
  expand_limits(y = rainElimits) +
  xlab("Predicted precipitation of rain (ln(mm))") +
  ylab("Residual") +
  labs(title = "Residuals vs predicted values") +
  theme(text = element_text(size = 12))

# Plot residuals vs pressure
residVsPressure <- ggplot(data = multiModelPred,
                          aes(x = pressure, y = e)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0) +
  expand_limits(y = rainElimits) +
  xlab("Pressure (hPa)") +
  ylab("Residual") +
  labs(title = "Residuals vs pressure") +
  theme(text = element_text(size = 12))

# Plot residuals vs temperature
residVsTemp <- ggplot(data = multiModelPred,
                      aes(x = temp, y = e)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0) +
  expand_limits(y = rainElimits) +
  xlab("Temperature (°C)") +
  ylab("Residual") +
  labs(title = "Residuals vs temperature") +
  theme(text = element_text(size = 12))

# Normal qq-plot of the residuals
QQ1 <- ggplot(data = multiModelPred, 
              aes(sample = e)) +
  geom_qq(size = 1) +
  geom_qq_line() +
  xlab("Theoretical") +
  ylab("Sample") +
  labs(title = "Normal QQ-plot of the residuals") +
  theme(text = element_text(size = 14))

# Make a visual comparison between these residuals and those from 1.(b), have they improved?
# Ans: Maybe slightly improved but QQ-plot still indicates not perfect normal distribution.

##### 2 (e) #####
# When the temperature changes with one degree Celcius the precipitation
# increases with about 4 % (i.e. e^B0 %) compared with 3 % as before from 1 (d)

##### 2 (f) #####
# When the pressure increases by 20 hPa, the precipitation decreases
# with about 69 % on average (i.e. c^20 = 0.944^20 = 0.31)

##### 2 (g) #####
x0 <- data.frame(temp = 5, pressure = c(1000, 1020))
y0 <- 
  cbind(x0,
        conf = exp(predict(multiModel, x0, interval = "confidence")))

# conf.fit for (5,1000) is 0.68.8 and conf.fit for (5,1020) is 21.6 
# and 68.8 * 0.3 = 21.6 (decrease with 70 % from 2 (f))

##### 2.3 Temperature and pressure with interaction####

##### 2 (h) #####
# Fit multiple linear regression model WITH INTERACTION TERM
# Investigate interaction term ONLY with ":" instead of "*"
(tpiModel <- lm(log(rain) ~ temp*pressure, data = weather))
tpiModelSummary <- summary(tpiModel) # interaction significant, see p-value

# Beta estimates and CI
cbind(tpiModel$coefficients,
      confint(tpiModel))

##### 2 (i) #####
# Same but with I(pressure - 1012) since 1012 hPa is the average pressure
tpiModel <- lm(log(rain) ~ temp*I(pressure - 1012), data = weather)
tpiModelSummary <- summary(tpiModel)

# Beta estimates and CI
cbind(tpiModel$coefficients,
      confint(tpiModel))

# Beta estimates and CI in ORIGINAL scale
cbind(exp(tpiModel$coefficients),
      exp(confint(tpiModel)))

# The parameter B1 changed substantially, from 0.04 to 3.27 when added interaction.
# When adding I(pressure - 1012), B0 changed from 61 to 3.35 B1 from 3.27 to 0.04 
# (same value as before interaction), B2 and B3 no change. 

##### 2 (j) #####
# Basic residual analysis
tpiModelPred <- cbind(weather,
                      conf = predict(tpiModel, interval = "confidence"),
                      pred = predict(tpiModel, interval = "prediction"))

# Add the residuals to the predicted data
tpiModelPred$e <- tpiModel$residuals
head(tpiModelPred)

# Save the max-value in order to make the y-axis symmetrical in the plots.
max.e <- max(abs(tpiModelPred$e))
tpiElimits <- c(-max.e, max.e)

# Plot residuals vs yhat
residVsYhatInt <- ggplot(data = tpiModelPred,
                         aes(x = pred.fit, y = e)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0) +
  expand_limits(y = tpiElimits) +
  xlab("Predicted precipitation of rain (ln(mm))") +
  ylab("Residual") +
  labs(title = "Residuals vs predicted values") +
  theme(text = element_text(size = 12))

# Plot residuals vs pressure
residVsPressureInt <- ggplot(data = tpiModelPred,
                             aes(x = pressure, y = e)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0) +
  expand_limits(y = tpiElimits) +
  xlab("Pressure (hPa)") +
  ylab("Residual") +
  labs(title = "Residuals vs pressure") +
  theme(text = element_text(size = 12))

# Plot residuals vs temperature
residVsTempInt <- ggplot(data = tpiModelPred,
                         aes(x = temp, y = e)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0) +
  expand_limits(y = tpiElimits) +
  xlab("Temperature (°C)") +
  ylab("Residual") +
  labs(title = "Residuals vs temperature") +
  theme(text = element_text(size = 12))

# Normal qq-plot of the residuals
QQ2 <- ggplot(data = tpiModelPred, 
              aes(sample = e)) +
  geom_qq(size = 1) +
  geom_qq_line() +
  xlab("Theoretical") +
  ylab("Sample") +
  labs(title = "Normal QQ-plot of the residuals") +
  theme(text = element_text(size = 14))

library("gridExtra")
# 2x3 plot of residuals vs y-hat, pressure and temp
grid.arrange(residVsYhat, residVsYhatInt, residVsTemp, residVsTempInt,
             residVsPressure, residVsPressureInt, nrow=3, ncol=2)

# 2x1 plot of QQ-plot
grid.arrange(QQ1, QQ2, nrow = 1, ncol = 2)

# Conclusion: No improvement with interaction term! (based on QQ of resid and resid vs yhat)

##### 2 (k) #####
# Explain how, according to this model, the precipitation changes when the temperature
# increases by 1 *C, on average, as a function of the air pressure.
# Ans: 1.04*(c*d)^(pressure-1012)

# Exemplify by estimating the change when the air pressure is 1000 Pa, and when it is 1020 Pa.
# Ans: (1.04*(c*d)^(1020-1012)) / (1.04*(c*d)^(1000-1012)) = (c*d)^8 / (c*d)^-12 = (c*d)^20 =
# = 0.299 i.e. decrease with 70 %, almost same as 2f.

# Compare with the results in 1.(d) and 2.(e). Comment on any interesting differences.
# Ans: Parameter b from 3 % (1d only temp) to 4 % (2e temp-pressure)
# to 4 % (2k with interaction).

##### 2 (l) #####
x0 <- data.frame(temp = c(-10, 10, -10, 10), pressure = c(1000, 1020, 1020, 1000))

# Confidence interval in log-scale 
(y0 <- cbind(x0,
             conf = exp(predict(tpiModel, x0,
                                interval = "confidence"))))

##### 2.4 Temperature, pressure and location####

##### 2 (m) #####
summary(weather)
# Conclusion: Use Uppsala as reference since most observations.

##### 2 (n) #####
# Change reference location to Uppsala
weather$location <- relevel(weather$location, "Uppsala")

# Fit same model as before but WITH LOCATION
tpiLocModel <- lm(log(rain) ~ temp*I(pressure-1012) + location, data = weather)
tpiLocModelSummary <- summary(tpiLocModel)

# Beta estimates and CI
cbind(tpiLocModel$coefficients,
      confint(tpiLocModel))

# Beta estimates and CI in ORIGINAL scale
cbind(exp(tpiLocModel$coefficients),
      exp(confint(tpiLocModel)))

# Partial F-test for the locations
# Fit the reduced model (original tpiModel) without locations:
modelReduced <- tpiModel

# Compare the models
(weatherAnova <- anova(modelReduced, tpiLocModel))
# -> p-value 2.2e-16

# Compare the F-value with upper F-quantile
(Fvalue <- weatherAnova$F[2]) # F-value: 73.5
qf(1 - 0.05, 2, 1085) # quantile value: 3.004. OBS use degrees of freedom for the "big" model!

# Fvalue > 3 (i.e. the quantile value) => add location in model = significant difference!

# Calculate P-value
pf(Fvalue, 2, 1085, lower.tail = FALSE)
weatherAnova$`Pr(>F)`[2]

# calculate P-value:
# 2 * P(|t| > |tvalue|) to cover both tails:
pValue <- 2*pt(abs(tvalue), 1089, lower.tail = FALSE)

# weatherAnova = pf so conclusion??????????????????????

# Conclusion: Using partil F-test, improves when adding location.

# Ska vi ha partial F-test?

##### 2 (o) #####
# Conslusion: All parameters changed slightly, not substantially.
# Intercept B0 was most effected, from 28.44 to 29.7 (original scale).

##### 2 (p) #####
# Basic residual analysis
tpiLocModelPred <- cbind(weather,
                         conf = (predict(tpiLocModel, interval = "confidence")),
                         pred = (predict(tpiLocModel, interval = "prediction")))

# Add residuals to predicted data
tpiLocModelPred$e <- tpiLocModel$residuals
head(tpiLocModelPred)

tpiLocMax.e <- max(abs(tpiLocModelPred$e))
tpiLocElimits <- c(-tpiLocMax.e, tpiLocMax.e)

# Plot residuals vs yhat
ggplot(data = tpiLocModelPred, 
       aes(x = pred.fit, y = e, color = location)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0) +
  expand_limits(y = tpiLocElimits) +
  xlab("Predicted precipitation of rain (ln(mm))") +
  ylab("Residual") +
  labs(title = "Residuals vs predicted values") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ location)

# Plot residuals vs temperature
ggplot(data = tpiLocModelPred, 
       aes(x = temp, y = e, color = location)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0) +
  expand_limits(y = tpiLocElimits) +
  xlab("Temperature (C)") +
  ylab("Residual") +
  labs(title = "Residuals vs temperature") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ location)

# Plot residuals vs pressure
ggplot(data = tpiLocModelPred, 
       aes(x = I(pressure - 1012), y = e, color = location)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0) +
  expand_limits(y = tpiLocElimits) +
  xlab("Pressure (0 = 1012 hPa)") +
  ylab("Residual") +
  labs(title = "Residuals vs pressure") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ location)

# Make a normal qq-plot of the residuals (all at once)
ggplot(data = tpiLocModelPred, 
       aes(sample = e)) +
  geom_qq(size = 1) +
  geom_qq_line() +
  xlab("Theoretical") +
  ylab("Sample") +
  labs(title = "Normal QQ-plot of the residuals") +
  theme(text = element_text(size = 18))

# Make a normal qq-plot of the residuals
ggplot(data = tpiLocModelPred, 
       aes(sample = e, color = location)) +
  facet_wrap(~ location) +
  geom_qq(size = 1) +
  geom_qq_line() +
  xlab("Theoretical") +
  ylab("Sample") +
  labs(title = "Normal QQ-plot of the residuals") +
  theme(text = element_text(size = 18))

# Conclusion: Much better when adding location, at least for Lund and Abisko.

##### 2 (q) #####
# Conclusion: Looking at the e^(beta estimates), Lund is expected to have the highest
# average precipitation. We also do a prediction to check.

##### Project 1 - part 3  #####

# Initializing from part 1 & 2
rm(list = ls())
load("Data/weather.rda")

weather$location <- relevel(weather$location, "Uppsala")

# Model from 2 (n)
model2n <- lm(log(rain) ~ temp*I(pressure-1012) + location, data = weather)
model2nSummary <- summary(model2n)

##### 3.1 Outliers and influential observations####

##### 3 (a) ####
pred2n <- cbind(weather, 
                fit = predict(model2n),
                e = residuals(model2n))

# Add leverage
pred2n$v <- influence(model2n)$hat
head(pred2n)

library(ggplot2) # activate ggplot2 commands

# Plot of leverage vs temp with 1/n and 2(p+1)/n horizontal lines
plotLevTemp <- ggplot(pred2n, aes(x = temp, y = v, color = location, ymin = 0)) +
  geom_point() + 
  facet_wrap(~ location) +
  geom_hline(yintercept = c(I(1/nrow(weather)))) +
  geom_hline(yintercept = I(2*12/nrow(weather)), color = "red") + # (2*6 = 12 parameters)
  xlab("Temperature (°C)") +
  ylab("Leverage") +
  labs(title = "Precipitation of rain: leverage vs temperature") +
  # labs(caption = "y = 1/n (black) and 2(p+1)/n (red)") +
  theme(text = element_text(size = 18))

# Plot of leverage vs pressure with 1/n and 2(p+1)/n horizontal lines:
plotLevPressure <- ggplot(pred2n, aes(x = I(pressure - 1012), y = v,
                                      color = location, ymin = 0)) +
  geom_point() + 
  facet_wrap(~ location) +
  geom_hline(yintercept = c(I(1/nrow(weather)))) +
  geom_hline(yintercept = I(2*12/nrow(weather)), color = "red") + # (2*6 = 12 parameters)
  xlab("Pressure (0 = 1012 hPa)") +
  ylab("Leverage") +
  labs(title = "Precipitation of rain: leverage vs pressure") +
  # labs(caption = "y = 1/n (black) and 2(p+1)/n (red)") +
  theme(text = element_text(size = 18))

# Conclusion: The smallest leverage in Lund and Abisko are larger than
# the smallest leverage in Uppsala since we used Uppsala as a reference
# and thereby the model will fit Uppsala better => less leverage. This
# is because leverage is measuring the distance from the center of gravity,
# which obviously lies in Uppsala since the most nbr of observations is
# collected there. 

##### 3 (b) #####
# Find all leverage above 0.026
outliersLeverage <- which(pred2n$v > 0.026)

# Plot temperature vs pressure and highlight the outliers
ggplot(weather, aes(x = I(pressure - 1012), y = temp, color = location, ymin = 0)) +
  geom_point() + 
  geom_point(data = weather[outliersLeverage, ], color = "red",
             size = 3, shape = 24) +
  facet_wrap(~ location) +
  xlab("Pressure (0 = 1012 hPa)") +
  ylab("Temperature (°C)") +
  labs(title = "Temperature vs pressure") +
  theme(text = element_text(size = 18))

##### 3 (c) #####
# Add studentized residuals
pred2n$r <- rstudent(model2n)

# Plot studentized residuals vs fitted values with horizontal lines for 0, +-2 and +-4.
studResid <- ggplot(pred2n, aes(x = fit, y = r, color = location)) +
  geom_point() +
  geom_point(data = pred2n[outliersLeverage, ], color = "red",
             size = 3, shape = 24) +
  facet_wrap(~ location) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), color = "red") +  # 95% of data should be inside +-2
  geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dashed") +
  xlab("Fitted value") +
  ylab("r*") +
  labs(title = "Precipitation of rain: studentized residuals vs fitted value") +
  # labs(caption = "y = +/- 2 and +/- 4") +
  theme(text = element_text(size = 18))

# Conclusion: No problematic studentized residuals for Abisko,
# but 1 for Uppsala and maybe 1 for Lund.

##### 3 (d) #####
# Sorting out Uppsala
index <- which(pred2n$loc == "Uppsala")
pred2n$rUppsala <- 0
pred2n$rUppsala[index] <- pred2n$r[index]
outliersStudResU <- which.max(abs(pred2n$rUppsala)) # U = Uppsala

# Sorting out Lund
index <- which(pred2n$loc == "Lund")
pred2n$rLund <- 0
pred2n$rLund[index] <- pred2n$r[index]
outliersStudResL <- which.max(abs(pred2n$rLund)) # L = Lund

# Sorting out Abisko
index <- which(pred2n$loc == "Abisko")
pred2n$rAbisko <- 0
pred2n$rAbisko[index] <- pred2n$r[index]
outliersStudResA <- which.max(abs(pred2n$rAbisko)) # A = Abisko

# Outliers from studentized residuals in one vector
outliersStudRes <- c(outliersStudResU, outliersStudResL, outliersStudResA)

# Outliers from studentized residuals and leverage in one vector
outliersTot <- c(outliersStudRes, outliersLeverage)

# Plot ln(rain) vs temp
ggplot(weather, aes(x = temp, y = log(rain), color = location, ymin = 0)) +
  geom_point() + 
  geom_point(data = weather[outliersTot, ], color = "red",
             size = 3, shape = 24) +
  geom_point(data = pred2n[outliersStudRes, ], color = "black",
             size = 3, shape = 24) +
  facet_wrap(~ location) +
  xlab("Temperature (°C)") +
  ylab("Precipitation of rain (ln(mm))") +
  labs(title = "Rain vs temperature") +
  # labs(caption = "Highlighted: leverage (black) and studentized residuals (red)") +
  theme(text = element_text(size = 18))

# Plot ln(rain) vs pressure
ggplot(weather, aes(x = I(pressure - 1012), y = log(rain), color = location, ymin = 0)) +
  geom_point() + 
  geom_point(data = weather[outliersTot, ], color = "red",
             size = 3, shape = 24) +
  geom_point(data = pred2n[outliersStudRes, ], color = "black",
             size = 3, shape = 24) +
  facet_wrap(~ location) +
  xlab("Pressure (0 = 1012 hPa)") +
  ylab("Precipitation of rain (ln(mm))") +
  labs(title = "Rain vs pressure") +
  # labs(caption = "Highlighted: leverage (black) and studentized residuals (red)") +
  theme(text = element_text(size = 18))

# Conclusion: For leverage (Abisko), low pressure is causing the outliers.

##### 3 (e) #####
# Cook's D (distance)
pred2n$D <- cooks.distance(model2n)
head(pred2n)

# Plot Cook's D vs pressure 
ggplot(pred2n, aes(x = I(pressure - 1012), y = D, color = location)) + 
  geom_point() +
  geom_point(data = pred2n[outliersTot, ], color = "red",
             size = 3, shape = 24) +
  geom_point(data = pred2n[outliersLeverage, ], color = "black",
             size = 3, shape = 24) +
  facet_wrap(~ location) +
  #geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  geom_hline(yintercept = I(4/nrow(weather)), color = "red") +
  xlab("Pressure (0 = 1012 hPa)") +
  ylab("Cook's D") +
  labs(title = "Precipitation of rain: Cook's D vs pressure") +
  # labs(caption = "y = 4/n") +
  theme(text = element_text(size = 18))

# Did any of the observations with high leverage have a large influence on the estimates?
# Ans: Only two of them (black).
# Did the observations with the largest residuals have a large influence?
# Ans: Yes all of them.
# Are there other observations that had a large influence?
# Ans: Yes, in Uppsala and some in Abisko. 

##### 3 (f) #####
# Exclude the outliers from the studentized residuals and the two highest
# values from leverage and re-fit the model

# Sorting out outliers with high leverage and high Cook's D from Abisko
# and keeping the ouliers with high leverage but low Cook's D
index <- which(pred2n$loc == "Abisko")
pred2n$DAbisko <- 0
pred2n$DAbisko[index] <- pred2n$D[index]
outliersDA <- which(pred2n$DAbisko > 0.022)

# Also removing the 2 extra outliers with high Cook's D in Uppsala
index <- which(pred2n$loc == "Uppsala")
pred2n$DUppsala <- 0
pred2n$DUppsala[index] <- pred2n$D[index]
outliersDUpp <- which(pred2n$DUppsala > 0.021)

# index 414 is the highest already identified, use the 2 others:
outliersDUpp <- c(outliersDUpp[1], outliersDUpp[3])

# The outliers that we should exclude
outliersExc <- c(outliersStudRes, outliersDA, outliersDUpp)

# Exclude outliers and refit the model
weatherExc <- weather[-outliersExc, ]
model2nExc <- lm(log(rain) ~ temp*I(pressure-1012) + location, data = weatherExc)

pred2nExc <- cbind(weatherExc, 
                   fit = predict(model2nExc),
                   e = residuals(model2nExc))

# Add studentized residuals
pred2nExc$r <- rstudent(model2nExc)

# Plot of studentized residuals vs fitted values with excluded data set
studResidExc <- ggplot(pred2nExc, aes(x = fit, y = r, color = location)) +
  geom_point() +
  # geom_point(data = pred2nExc[outliersLeverage, ], color = "red",
  #            size = 3, shape = 24) +
  facet_wrap(~ location) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), color = "red") +
  geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dashed") +
  xlab("Fitted value") +
  ylab("r*") +
  labs(title = "Precipitation of rain: studentized residuals vs fitted value") +
  # labs(caption = "y = +/- 2 and +/- 4 exc") +
  theme(text = element_text(size = 18))

# Add Cook's D
pred2nExc$D <- cooks.distance(model2nExc)

# Plot Cook's D vs pressure with excluded data set
ggplot(pred2nExc, aes(x = I(pressure - 1012), y = D, color = location)) + 
  geom_point() +
  facet_wrap(~ location) +
  #geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  geom_hline(yintercept = I(4/nrow(weather)), color = "red") +
  xlab("Preassure (0 = 1012 hPa)") +
  ylab("Cook's D") +
  labs(title = "Precipitation of rain: Cook's D vs pressure") +
  # labs(caption = "y = 4/n exc") +
  theme(text = element_text(size = 18))

# Conclusion: Much better studentized residuals and also better Cooks D(note scale difference)
# but still some more removal to do if only looking at Cooks D.

##### 3.2 Model comparisons####

##### 3 (g) #####
# Re-fit model 1.(b), 2.(d) and 2.(h)
(modelTemp <- lm(log(rain) ~ temp, data = weatherExc))
modelTempSummary <- summary(modelTemp)

(modelTp <- lm(log(rain) ~ temp + pressure, data = weatherExc))
modelTpSummary <- summary(modelTp)

(modelTpi <- lm(log(rain) ~ temp*pressure, data = weatherExc))
modelTpiSummary <- summary(modelTpi)

# Rename model2nExc to modelTpiLoc
modelTpiLoc <- model2nExc
modelTpiLocSummary <- summary(modelTpiLoc)

# Collect the R2-s for plotting
(collectR2s <- data.frame(
  nr = seq(1, 4),
  model = c("temp", "temp + pressure", "temp*pressure", "temp*pressure + loc"),
  R2 = c(modelTempSummary$r.squared,
         modelTpSummary$r.squared,
         modelTpiSummary$r.squared,
         modelTpiLocSummary$r.squared),
  R2.adj = c(modelTempSummary$adj.r.squared,
             modelTpSummary$adj.r.squared,
             modelTpiSummary$adj.r.squared,
             modelTpiLocSummary$adj.r.squared)))

# ... and plot them
ggplot(collectR2s, aes(model, R2)) +
  geom_point(size = 3) + 
  geom_point(aes(y = R2), color = "red", size = 3) + 
  geom_line(aes(x = nr), size = 1) +
  geom_line(aes(x = nr, y = R2.adj), 
            color = "red", size = 1, linetype = "dashed") +
  geom_hline(yintercept = 1) +
  # labs(caption = "R2 (black), R2-adj (red dashed)") +
  labs(title = "Precipitation of rain: R2 and R2-adjusted") +
  ylab("R2 and R2-adj") +
  xlab("Model") + 
  theme(text = element_text(size = 18))

# AIC and BIC
(collectAIC <- data.frame(
  nr = seq(1, 4),
  model = c("temp", "temp + pressure", "temp*pressure", "temp*pressure + loc"),
  AIC(modelTemp, modelTp, modelTpi, modelTpiLoc),
  BIC(modelTemp, modelTp, modelTpi, modelTpiLoc)))

# and plot:
ggplot(collectAIC, aes(model, AIC)) +
  geom_point(size = 3) + 
  geom_point(aes(y = BIC), color = "red", size = 3) + 
  geom_line(aes(x = nr), size = 1) +
  geom_line(aes(x = nr, y = BIC), 
            color = "red", size = 1, linetype = "dashed") +
  # labs(caption = "AIC (black), BIC (red dashed)") +
  labs(title = "Precipitation of rain: AIC and BIC") +
  ylab("AIC and BIC") +
  xlab("Model") + 
  theme(text = element_text(size = 18))

# Conclusion: R/Radj and AIC/BIC suggest model temp*pressure + location as the best.
# About 40 % of the variability in Y can be explained by our model.

##### 3 (h) #####
# Fit model with three-way interaction temp*pressure*location
modelTpLocI <- lm(log(rain) ~ temp*I(pressure-1012)*location, data = weatherExc)
modelTpLocISummary <- summary(modelTpLocI)

# Conclusion: A LOT of insignificant parameters since p-value > 0.05

# Partial F-test for the locations
# Compare the models:
(weatherAnova <- anova(modelTpiLoc, modelTpLocI))

# Compare the F-value with upper F-quantile:
(Fvalue <- weatherAnova$F[2]) # F-value: 1.21
qf(1 - 0.05, 2, 1074) # Degrees of freedom for the full model, not the reduced one
# 3.004

# calculate P-value:
pf(Fvalue, 2, 1074, lower.tail = FALSE)
weatherAnova$`Pr(>F)`[2]

# Not significant difference since F-value is smaller than the quantile
# and that the p-value is larger than 0.05.

##### 3 (i) #####
# Backward elimination using BIC 
step(modelTpLocI, k = log(nrow(weatherExc))) # default AIC (k = 2):

# first step:
# best = remove tripple interaction
# second best = do nothing

# second step (after removing tripple interaction):
# best = remove temp:location
# second best = I(pressure - 1012):location
# third best = do nothing

# third step (after removing temp:location):
# best = remove I(pressure - 1012):location
# second best = do nothing

# 4th step (after removing I(pressure - 1012):location):
# best = do nothing
# second best = remove temp:I(pressure - 1012)

# Model suggestion from automatic backward elimination
# modelAutoBackward <- lm(formula = log(rain) ~ temp + I(pressure - 1012) + location + 
#                        temp:I(pressure - 1012), data = weatherExc)

modelBackward <- lm(formula = log(rain) ~ temp*I(pressure - 1012)
                    + location, data = weatherExc)
modelBackwardSummary <- summary(modelBackward)

# Conclusion: All parameters significant.

##### 3 (j) #####
# Fit a model with only intercept (initializing forward selection)
modelIntercept <- lm(formula = log(rain) ~ 1, data = weatherExc)

# Forward selection
step(modelIntercept, 
     scope = list(upper = modelTpLocI), # specify the scope: upper = largest model allowed
     direction = "forward", k = log(nrow(weatherExc)))

# first step:
# best = add pressure
# second best = add temp
# third best = add location
# 4th best = do nothing

# second step (after adding pressure):
# best = add temp
# second best = add location
# third best = do nothing

# third step (after adding temp):
# best = add location
# second best = add temp:pressure
# third best = do nothing

# 4th step (after adding location):
# best = add temp:pressure
# second best = add pressure:location
# third best = add temp:location
# 4th best = do nothing

# 5th step (after adding temp:pressure):
# best = do nothing
# second best = add pressure:location
# third best = add temp:location

# Model suggestion from automatic forward stepping
# modelAutoForward <- lm(formula = log(rain) ~ I(pressure - 1012) + temp + location + 
#                         I(pressure - 1012):temp, data = weatherExc)

modelForward <- lm(formula = log(rain) ~ I(pressure - 1012)*temp
                   + location, data = weatherExc)
modelForwardSummary <- summary(modelForward)

# Conclusion: Forward selection chooses same model as backward elimination => good!

##### 3 (k) #####
# Adding season as a categorial variable
weatherExc$season <- "Winter"
weatherExc$season[weatherExc$monthnr == 3] <- "Spring"
weatherExc$season[weatherExc$monthnr == 4] <- "Spring"
weatherExc$season[weatherExc$monthnr == 5] <- "Spring"
weatherExc$season[weatherExc$monthnr == 6] <- "Summer"
weatherExc$season[weatherExc$monthnr == 7] <- "Summer"
weatherExc$season[weatherExc$monthnr == 8] <- "Summer"
weatherExc$season[weatherExc$monthnr == 9] <- "Autumn"
weatherExc$season[weatherExc$monthnr == 10] <- "Autumn"
weatherExc$season[weatherExc$monthnr == 11] <- "Autumn"

modelTpLocSeasonI <- lm(log(rain) ~ temp*I(pressure-1012)*location*season, data = weatherExc)
modelTpLocSeasonISummary <- summary(modelTpLocSeasonI)

# Partial F-test for the seasons
# Compare the models:
(weatherAnova <- anova(modelTpiLoc, modelTpLocSeasonI))

# Compare the F-value with upper F-quantile:
(Fvalue <- weatherAnova$F[2]) # F-value: 3.28
qf(1 - 0.05, 2, 1036) # Quantile value: 3.004

# calculate P-value:
pf(Fvalue, 2, 1036, lower.tail = FALSE) # p-value 0.038
weatherAnova$`Pr(>F)`[2]

# Conclusion: Significant difference since F-value is bigger than the quantile
# and the p-value is less than 0.05

# Forward selection
step(modelIntercept, 
     scope = list(upper = modelTpLocSeasonI),
     direction = "forward", k = log(nrow(weatherExc)))

finalModel <- lm(formula = log(rain) ~ I(pressure - 1012)*temp + location + 
                   season, data = weatherExc)

finalModelSummary <- summary(finalModel)

# NOTE: summer NOT significant!

aic <- AIC(finalModel)
bic <- BIC(finalModel)

# Conslusion: Better R2adj than before, 0.43 compared to 0.4, and also better
# AIC and BIC, 1884 and 1919 compared to 1834 abd 1884.

# Since summer not significant, we need to create a new categorical variable
# so we can exclude summer in the modelling, so we add "seasonExc" and refit
# the model with this instead of "season"
weatherExc$seasonExc <- "exc"
weatherExc$seasonExc[weatherExc$season == "Winter"] <- "Winter"
weatherExc$seasonExc[weatherExc$season == "Spring"] <- "Spring"

modelTpLocSeasonExcI <- lm(log(rain) ~ temp*I(pressure-1012)*location*seasonExc,
                           data = weatherExc)
modelTpLocSeasonExcISummary <- summary(modelTpLocSeasonExcI)

# Partial F-test for the seasons
# Compare the models:
(weatherAnova <- anova(modelTpiLoc, modelTpLocSeasonExcI))

# Compare the F-value with upper F-quantile:
(Fvalue <- weatherAnova$F[2]) # F-value: 3.92
qf(1 - 0.05, 2, 1048) # Quantile value: 3.004

# calculate P-value:
pf(Fvalue, 2, 1048, lower.tail = FALSE) # p-value 0.02
weatherAnova$`Pr(>F)`[2]

# Conclusion: Significant difference since F-value is bigger than the quantile
# and the p-value is less than 0.05

# Forward selection
step(modelIntercept, 
     scope = list(upper = modelTpLocSeasonExcI),
     direction = "forward", k = log(nrow(weatherExc)))

finalModelSeasonExc <- lm(formula = log(rain) ~ I(pressure - 1012)*temp + location + 
                            seasonExc, data = weatherExc)

finalModelSeasonExcSummary <- summary(finalModelSeasonExc) # all significant!

aic <- AIC(finalModelSeasonExc)
bic <- BIC(finalModelSeasonExc)

# HOWEVER marginally better results, AIC/BIC from 1834/1884 to 1832/1877
# and R2adj from 0.4242 to 0.4246, same for R2.

# Are there seasonal differences? - Yes!
# Does a change in air pressure affect the precipitation in the same way regardless
# of the season? - Yes, since no interaction between pressure and season is included
# in the model.

# How much of the variability can this final model explain? - About 43 %
