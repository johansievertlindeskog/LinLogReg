##### Project 2 - part 1  #####

# Initializing
rm(list = ls())
load("Data/weather.rda")

##### 1 The null model####

##### 1 (a) #####
weather$lowrain <- as.numeric(weather$rain < 25)
p <- mean(weather$lowrain)

# Odds and log-odds for LOW precipitation (<25 mm)
odds <- p/(1 - p)
logOdds <- log(odds)

##### 1 (b) #####
# Fit null logistic regression model with intercept only (g = generalized)
model1b <- glm(lowrain ~ 1, family = "binomial", data = weather)
summary(model1b)

# Beta estimates and CI for log-odds
model1b$coefficients
confLogOdds <- confint(model1b)

# Beta estimates and CI for odds
odds <- exp(model1b$coefficients)
confOdds <- exp(confint(model1b))

# Probability of a month having low precipitation
p <- odds/(1 + odds)
confP <- confOdds/(1 + confOdds)

##### 2 Temperature...####

##### 2 (a) #####
library(ggplot2) # activate ggplot2 commands

# Plot lowrain vs temp with moving average
(
  plotRainTemp <- ggplot(data = weather, 
                         aes(x = temp, y = lowrain )) + 
    geom_smooth(method = loess) + # moving average
    geom_point(size = 1) +
    xlab("Temperature (°C)") +
    ylab("Lowrain (1/0)") +
    labs(title = "Probability of low rain vs temperature") +
    theme(text = element_text(size = 18))
)

# Does it seem reasonable to use temp as covariate? - Yes!

##### 2 (b) Fit with temp#####
# Fit logistic regression model with temperature
model2b <- glm(lowrain ~ temp, family = "binomial", data = weather)
summary(model2b) # temp significant

# Beta estimates and CI for log-odds
cbind(model2b$coefficients,
      confint(model2b))

# Beta estimates and CI for odds
cbind(exp(model2b$coefficients),
      exp(confint(model2b)))




# ska man bara kolla på z-värdet i summary och kolla om den är mindre än 0.05? wald test?




# logistic regression model
# log(Y) = B0 + B1*x + epsilon
# Transformed model in untransformed scale
# Y = exp(B0) * exp(B1)^x * exp(epsilon) = a * b^x * exp(epsilon)
# where a = exp(B0) and b = exp(B1)

# How does the odds of low rain change when the temperature is increased by 1 °C?
# Ans: b^1 = 0.93 i.e. odds decreases by 7%
# How does the odds of low rain change when the temperature is decreased by 1 °C?
# Ans: b^(-1) = 0.93^-1 = 1.08 i.e. odds increases by 8%

##### 2 (c) #####
# Estimated p for some values of temperature with CI
x0 <- data.frame(temp = c(-10, -9, 9, 10))

pred2c <- cbind(x0,
                 phat = predict(model2b, x0,
                                type = "response"))

# logit = logodds with s.e. for constructing C.I.
pred2c <- cbind(
  pred2c,
  logit = predict(model2b, x0, se.fit = TRUE))
head(pred2c)

# An unnecessary variable:
pred2c$logit.residual.scale <- NULL

# Calculate confidence intervals for the log odds
# standard normal quantile:
(lambda <- qnorm(1 - 0.05/2))
pred2c$logit.lwr <- pred2c$logit.fit - lambda*pred2c$logit.se.fit
pred2c$logit.upr <- pred2c$logit.fit + lambda*pred2c$logit.se.fit

# transform the log-odds intervals into C.I. for odds
pred2c$odds.lwr <- exp(pred2c$logit.lwr)
pred2c$odds.upr <- exp(pred2c$logit.upr)

# transform the odds intervals into C.I. for p
pred2c$p.lwr <- pred2c$odds.lwr/(1 + pred2c$odds.lwr)
pred2c$p.upr <- pred2c$odds.upr/(1 + pred2c$odds.upr)
head(pred2c)

# There is a difference in the probability of low rain when we change the temparature
# from -10 °C to -9 °C, compared with when we change from 9 °C to 10 °C since 

# SVARA PÅ FRÅGA!!!!!!!!!!!!!!!!!!





##### 2 (d) #####
# predict for plotting
# phat = estimated probabilities
lowrainPred <- cbind(weather, phat = predict(model2b, type = "response"))

ggplot(lowrainPred, aes(temp, lowrain)) +
  geom_point() +
  geom_smooth(se = FALSE, linetype = "dashed") +
  geom_line(aes(y = phat), color = "red", size = 1) +
  xlab("Temperature (°C)") +
  ylab("Probability of low rain") +
  labs(title = "Low precipitaion (=1) or Not low precipitaion (=0) vs temperature")
      # caption = "red = fitted line, blue dashed = moving average") +
  theme(text = element_text(size = 14))

# logit = logodds with s.e. for constructing C.I.
lowrainPred <- cbind(lowrainPred, logit = predict(model2b, se.fit = TRUE))

# Remove unnecessary variable:
lowrainPred$logit.residual.scale <- NULL

# Calculate confidence intervals for the log odds
# standard normal quantile:
(lambda <- qnorm(1 - 0.05/2))
lowrainPred$logit.lwr <- lowrainPred$logit.fit - lambda*lowrainPred$logit.se.fit
lowrainPred$logit.upr <- lowrainPred$logit.fit + lambda*lowrainPred$logit.se.fit

# transform the log-odds intervals into C.I. for odds
lowrainPred$odds.lwr <- exp(lowrainPred$logit.lwr)
lowrainPred$odds.upr <- exp(lowrainPred$logit.upr)

# transform the odds intervals into C.I. for p
lowrainPred$p.lwr <- lowrainPred$odds.lwr/(1 + lowrainPred$odds.lwr)
lowrainPred$p.upr <- lowrainPred$odds.upr/(1 + lowrainPred$odds.upr)
head(lowrainPred)

# plot the intervals:
ggplot(lowrainPred, aes(temp, lowrain)) +
  geom_point() +
  geom_smooth(se = FALSE, linetype = "dashed") +
  geom_line(aes(y = phat), color = "red", size = 1) +
  geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2) +
  xlab("Temperature (°C)") +
  ylab("Probability of low rain") +
  labs(title = "Low precipitaion (=1) or Not low precipitaion (=0) vs temperature")
# caption = "red = fitted line, blue dashed = moving average") +
theme(text = element_text(size = 14))

##### 2 (e) Leverage #####
leverage <- influence(model2b)

lowrainPred <- cbind(weather, xbeta = predict(model2b), v = leverage$hat)
head(lowrainPred)

# KOLLA HUT VI LÄGGER TILL LEVERAGE, KAN VI INTE BARA GÖRA SOM GAMLA SÄTTET?



# VAD ÄR XBETA? BORDE VARA YHAT???????????????????????????????????????





# Plot of leverage vs temperature with 1/n and 2(p+1)/n horizontal lines
(plotLevTemp2b <- ggplot(lowrainPred, aes(temp, v, ymin = 0, color = as.factor(lowrain))) + 
    geom_point() +
    geom_hline(yintercept = c(I(1/nrow(weather)))) +
    geom_hline(yintercept = 2*4/nrow(weather),
               color = "red", size = 1) +#(2*2 = 4 parameters)
    facet_wrap(~ lowrain) +
    xlab("Temperature (°C)") +
    ylab("Leverage") +
    labs(title = "Leverage vs temperature",
         color = "Y") +
    theme(text = element_text(size = 14)))

# Find all leverage above 2*4/nrow(weather) = 0.0073 (arbitrary choice):
outliersLeverage <- which(lowrainPred$v > I(2*4/nrow(weather)))

# ... and highlight the outliers
(plotLevTemp2bOutliers <- plotLevTemp2b +
  geom_point(data = lowrainPred[outliersLeverage, ], size = 3, 
             color = "red", shape = 24) +
  labs(title = "Leverage vs temperature")) #red = 2(p+1)/n, black = 0.0073

# Conclusion: Some observations with high leverage.



# FRÅGA: HUR MÅNGA LEVERAGE RÄKNAS SOM HIGH??????????????????????????



##### 2 (f) Standardised deviance residuals #####
# Deviance residuals
lowrainPred$devres <- leverage$dev.res

# Standardised deviance residuals
lowrainPred$devstd <- lowrainPred$devres/sqrt(1 - lowrainPred$v)
head(lowrainPred)

# Plot of standardized deviance residuals vs temperature
# with horizontal lines for 0, +-2 and +-4.
(plotResTemp2b <- ggplot(lowrainPred, aes(temp, devstd, color = as.factor(lowrain))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), color = "red", linetype = "dashed",
             size = 1) +
  geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dotted",
             size = 1) +
  xlab("Temperature (°C)") +
  ylab("Standardized deviance residuals") +
  labs(title = "Standardized deviance residuals vs temperature",
       color = "Y") +
  theme(text = element_text(size = 14)))

# Conclusion: No alarmingly large standardized deviance residuals.


# FRÅGA: STÄMMER DET, SKA VI EJ TA BORT N¨GRA FRÅN standardized deviance residuals????

##### 2 (g) Cook's D ####
# Add Cook's D
lowrainPred$D <- cooks.distance(model2b)
head(lowrainPred)

# Plot of Cook's D vs temperature with horizontal line for 4/n
(plotCooksDTemp2b <- ggplot(lowrainPred, aes(temp, D, color = as.factor(lowrain))) +
  geom_point() +
  geom_point(data = lowrainPred[outliersLeverage, ], color = "black",
             shape = 24, size = 3) +
  #geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 4/nrow(weather), color = "red", linetype = "dotted",
             size = 1) +
  xlab("Temperature (°C)") +
  ylab("Cook's D") +
  labs(title = "Cook's D vs temp",
       color = "Y") +
  # labs(caption = "y = 4/n") +
  theme(text = element_text(size = 14)))

# Are there any observations that have had a large influence on the estimates?
# Ans: Yes.
# Are these observations the same as those that had the highest leverages?
# Ans: Yes, one of the previous 4.




# FRÅGA: SKA VI TA BORT MER BASERAT PÅ COOKS D??????????????????????????????????????



##### 3 ...or pressure...####

##### 3 (a) Fit with pressure #####
# Plot lowrain vs pressure with moving average
(
  plotRainPressure <- ggplot(data = weather, 
                         aes(x = pressure, y = lowrain )) + 
    geom_smooth(method = loess) + # moving average
    geom_point(size = 1) +
    xlab("Pressure (hPa)") +
    ylab("Lowrain (1/0)") +
    labs(title = "Probability of low rain vs pressure") +
    theme(text = element_text(size = 18))
)

# Fit logistic regression model with pressure
model3a <- glm(lowrain ~ I(pressure - 1012), family = "binomial", data = weather)
summary(model3a) # pressure significant




# FRÅN OCH MED HÄR, SKA VI ALLTID HA PRESSURE - 1012 ELLER OK MED ENDAST PRESSURE NÄR
# VI EXEMPELVIS KÖR LEVERAGE OCH DEVIANCE RESIDUALS OSV? SPELAR DET NÅGON ROLL?





# Beta estimates and CI for log-odds
cbind(model3a$coefficients,
      confint(model3a))

# Beta estimates and CI for odds
cbind(exp(model3a$coefficients),
      exp(confint(model3a)))

# predict for plotting
lowrainPred <- cbind(weather, phat = predict(model3a, type = "response"))

ggplot(lowrainPred, aes(I(pressure - 1012), lowrain)) +
  geom_point() +
  geom_smooth(se = FALSE, linetype = "dashed") +
  geom_line(aes(y = phat), color = "red", size = 1) +
  xlab("Pressure (0 = 1012 hPa)") +
  ylab("Probability of low rain") +
  labs(title = "Low precipitaion (=1) or Not low precipitaion (=0) vs pressure") +
  theme(text = element_text(size = 14))
# caption = "red = fitted line, blue dashed = moving average")

# logit = logodds with s.e. for constructing C.I.
lowrainPred <- cbind(
  lowrainPred,
  logit = predict(model3a, se.fit = TRUE))
head(lowrainPred)
# Remove unnecessary variable:
lowrainPred$logit.residual.scale <- NULL

# Calculate confidence intervals for the log odds
# standard normal quantile:
(lambda <- qnorm(1 - 0.05/2))
lowrainPred$logit.lwr <- lowrainPred$logit.fit - lambda*lowrainPred$logit.se.fit
lowrainPred$logit.upr <- lowrainPred$logit.fit + lambda*lowrainPred$logit.se.fit

# transform the log-odds intervals into C.I. for odds
lowrainPred$odds.lwr <- exp(lowrainPred$logit.lwr)
lowrainPred$odds.upr <- exp(lowrainPred$logit.upr)

# transform the odds intervals into C.I. for p
lowrainPred$p.lwr <- lowrainPred$odds.lwr/(1 + lowrainPred$odds.lwr)
lowrainPred$p.upr <- lowrainPred$odds.upr/(1 + lowrainPred$odds.upr)
head(lowrainPred)

# plot the intervals:
ggplot(lowrainPred, aes(I(pressure - 1012), lowrain)) +
  geom_point() +
  geom_smooth(se = FALSE, linetype = "dashed") +
  geom_line(aes(y = phat), color = "red", size = 1) +
  geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2) +
  xlab("Pressure (0 = 1012 hPa)") +
  ylab("Probability of low rain") +
  labs(title = "Low precipitaion (=1) or Not low precipitaion (=0) vs pressure") +
  theme(text = element_text(size = 14))
# caption = "red = fitted line, blue dashed = moving average")

##### 3 (b) Leverage #####
leverage <- influence(model3a)

lowrainPred <- cbind(weather, xbeta = predict(model3a), v = leverage$hat)
head(lowrainPred)

# Plot of leverage vs pressure with 1/n and 2(p+1)/n horizontal lines
(plotLevPressure3a <- ggplot(lowrainPred, aes(pressure, v, ymin = 0, color = as.factor(lowrain))) + 
    geom_point() +
    geom_hline(yintercept = c(I(1/nrow(weather)))) +
    geom_hline(yintercept = 2*4/nrow(weather),
               color = "red", size = 1) +#(2*2 = 4 parameters)
    facet_wrap(~ lowrain) +
    xlab("Pressure (hPa)") +
    ylab("Leverage") +
    labs(title = "Leverage vs pressure", color = "Y") +
    theme(text = element_text(size = 14)))




# FRÅGA: PRESSURE ELLER PRESSURE - 1012???? ÄNDRA ENHET I SÅ FALL!!!




# Find all leverage above 2*4/nrow(weather) = 0.0073 (arbitrary choice):
outliersLeverage <- which(lowrainPred$v > I(2*4/nrow(weather)))

# ... and highlight the outliers
(plotLevPressure3aOutliers <- plotLevPressure3a +
  geom_point(data = lowrainPred[outliersLeverage, ], size = 3, 
             color = "red", shape = 24) +
  labs(title = "Leverage vs pressure")) #red = 2(p+1)/n, black = 0.0073

# Conclusion: Some observations with high leverage.

##### 3 (c) Standardised deviance residuals #####
# Deviance residuals
lowrainPred$devres <- leverage$dev.res

# Standardised deviance residuals
lowrainPred$devstd <- lowrainPred$devres/sqrt(1 - lowrainPred$v)
head(lowrainPred)

# Plot of standardized deviance residuals vs pressure
# with horizontal lines for 0, +-2 and +-4.
(plotResPressure3a <- ggplot(lowrainPred, aes(pressure, devstd, color = as.factor(lowrain))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), color = "red", linetype = "dashed",
             size = 1) +
  geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dotted",
             size = 1) +
  xlab("Pressure (hPa)") +
  ylab("Standardized deviance residuals") +
  labs(title = "Standardized deviance residuals vs pressure",
       color = "Y") +
  theme(text = element_text(size = 14)))

# Conclusion: Some alarmingly large standardized deviance residuals.




# FRÅGA: VILKA SKA VI TA BORT? FINNS MASSOR AV KANDIDATER!





##### 3 (d) Cook's D#####
# Add Cook's D
lowrainPred$D <- cooks.distance(model3a)
head(lowrainPred)

# Plot of Cook's D vs pressure with horizontal line for 4/n
(plotCooksDPressure3a <- ggplot(lowrainPred, aes(pressure, D, color = as.factor(lowrain))) +
  geom_point() +
  geom_point(data = lowrainPred[outliersLeverage, ], color = "black",
             shape = 24, size = 3) +
  #geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 4/nrow(weather), color = "red", linetype = "dotted",
             size = 1) +
  xlab("Pressure (hPa)") +
  ylab("Cook's D") +
  labs(title = "Cook's D vs pressure",
       color = "Y") +
  # labs(caption = "y = 4/n") +
  theme(text = element_text(size = 14)))

# Conclusion: Only 1 with high leverage and Cook's D.


# SKA VI TA BORT FLER ÄN DEN ENSAMMA UPPE VID 0.08????????



##### 3 (e) Comparing#####
# Compare the leverage, residual, and Cook’s distance plots with those for the temperature
# model from 2.(b). Which model seems best?

#install.packages("gridExtra")
library("gridExtra")

# Plots for comparing
grid.arrange(plotLevTemp2bOutliers, plotLevPressure3aOutliers, nrow=1, ncol=2)
grid.arrange(plotResTemp2b, plotResPressure3a, nrow=1, ncol=2)
grid.arrange(plotCooksDTemp2b, plotCooksDPressure3a, nrow=1, ncol=2)

# Conslusion: Lower leverage outliers for temperature, deviance residuals maybe better for
# pressure and lower Cook's D for temperature as well.



# DOCK SER DEVIANCE RESIDUALS LITE KOPNSTIGA UT FÖR ÅRESSURE, LITE FÖR RAKA JÄMFÖRE MED TEMP???





# Ans: RÄTT CONCLUSION????????????!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!






##### 3 (f) R2 Cox-Snell and R2 Nagelkerke #####

##### 3 (g) AIC and BIC#####
(collectAIC <- data.frame(
  nr = seq(1, 2),
  model = c("temp", "pressure"),
  AIC(model2b, model3a),
  BIC(model2b, model3a)))

# Conclusion: AIC and BIC lower for model3a with pressure.





# HELA 3AN ÄR KLAR SÅ FORT VI GÖR R2 Cox-Snell and R2 Nagelkerke OCH TAR BORT RÄTT AV DET
# SOM SKA BORT EFTER ÖEVERAGE, RESIDUALS AND COOKS D





##### 4 ...or both with location?####

# Initializ again
rm(list = ls())
load("Data/weather.rda")

weather$lowrain <- as.numeric(weather$rain < 25)
model2b <- glm(lowrain ~ temp, family = "binomial", data = weather)
model3a <- glm(lowrain ~ I(pressure - 1012), family = "binomial", data = weather)

##### 4 (a) Fit with interaction and location #####
# Change reference location to Uppsala since most observations
weather$location <- relevel(weather$location, "Uppsala")




# ANTAR ATT DE INTE SYFTAR PÅ ATT BYRA REFERENS FÖR LOWRAIN? TÄNKTE DET ÄR MER 0:OR ÄN 1:OR




# Fit logistic regression model with temperature, pressure
model4a <- glm(lowrain ~ temp*I(pressure - 1012) + location, family = "binomial", data = weather)
summary(model4a) # pressure significant

# compare with model2b using anova:
(anovaModel2b4a <- anova(model2b, model4a))
(D_diff <- anovaModel2b4a$Deviance[2])
(f_diff <- anovaModel2b4a$Df[2])

#chi2-quantile to compare D_diff with:
qchisq(1 - 0.05, f_diff)
# or P-value:
pchisq(D_diff, f_diff, lower.tail = FALSE)

# Conclusion: VAD SKA JÄMFÖRAS MED VAD?????????????????????????????????

# compare with model3a using anova:
(anovaModel3a4a <- anova(model3a, model4a))
(D_diff <- anovaModel3a4a$Deviance[2])
(f_diff <- anovaModel3a4a$Df[2])

#chi2-quantile to compare D_diff with:
qchisq(1 - 0.05, f_diff)
# or P-value:
pchisq(D_diff, f_diff, lower.tail = FALSE)

# Conclusion: VAD SKA JÄMFÖRAS MED VAD?????????????????????????????????





# SOME TEST TO COMPARE THE MODELS?? HOW TOOOOO?????





##### 4 (b) #####
# predict for plotting
lowrainPred <- cbind(weather, phat = predict(model4a, type = "response"))

# Lowrain vs temp, in subplots by location
ggplot(lowrainPred, aes(x = temp, y = lowrain)) +
  geom_point() +
  facet_wrap(~ location) +
  geom_smooth(se = FALSE, linetype = "dashed") +
  geom_line(aes(y = phat), color = "red", size = 1) +
  geom_point(aes(y = phat, color = pressure)) +
  scale_color_viridis_c() +
  xlab("Temperature (°C)") +
  ylab("Probability of low rain") +
  labs(title = "Low precipitaion (=1) or Not low precipitaion (=0) vs temperature",
       color = "Pressure (hPa)") +
  theme(text = element_text(size = 14))
# caption = "red = fitted line, blue dashed = moving average")




# THE PREDICTED PROBABILITIES PHAT SER UT SOM SKIT HUR FÅR MAN PLOTTEN ATT SE OK UT??????????????

# HUR KAN MAN GÖRA SÅ ATT DET BLIR FÄRG PÅ LOCATION??? FÅR FELMEDELANDE:
# "Error: Discrete value supplied to continuous scale" NÄR VI FÖRSÖKER 
# LÄGGA TILL: color = as.factor(lowrain) inom aes()




# logit = logodds with s.e. for constructing C.I.
lowrainPred <- cbind(
  lowrainPred,
  logit = predict(model4a, se.fit = TRUE))

# Remove unnecessary variable:
lowrainPred$logit.residual.scale <- NULL

# Calculate confidence intervals for the log odds
# standard normal quantile:
(lambda <- qnorm(1 - 0.05/2))
lowrainPred$logit.lwr <- lowrainPred$logit.fit - lambda*lowrainPred$logit.se.fit
lowrainPred$logit.upr <- lowrainPred$logit.fit + lambda*lowrainPred$logit.se.fit

# transform the log-odds intervals into C.I. for odds
lowrainPred$odds.lwr <- exp(lowrainPred$logit.lwr)
lowrainPred$odds.upr <- exp(lowrainPred$logit.upr)

# transform the odds intervals into C.I. for p
lowrainPred$p.lwr <- lowrainPred$odds.lwr/(1 + lowrainPred$odds.lwr)
lowrainPred$p.upr <- lowrainPred$odds.upr/(1 + lowrainPred$odds.upr)
head(lowrainPred)

# plot the intervals:
ggplot(lowrainPred, aes(x = temp, y = lowrain)) +
  geom_point() +
  facet_wrap(~ location) +
  geom_smooth(se = FALSE, linetype = "dashed") +
  geom_line(aes(y = phat), color = "red", size = 1) +
  geom_point(aes(y = phat, color = pressure)) +
  scale_color_viridis_c() +
  geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2) +
  xlab("Temperature (°C)") +
  ylab("Probability of low rain") +
  labs(title = "Low precipitaion (=1) or Not low precipitaion (=0) vs temperature",
       color = "Pressure (hPa)") +
  theme(text = element_text(size = 14))
# caption = "red = fitted line, blue dashed = moving average")

# Same again but now lowrain vs pressure, in subplots by location
ggplot(lowrainPred, aes(x = pressure, y = lowrain)) +
  geom_point() +
  facet_wrap(~ location) +
  geom_smooth(se = FALSE, linetype = "dashed") +
  geom_line(aes(y = phat), color = "red", size = 1) +
  geom_point(aes(y = phat, color = temp)) +
  scale_color_viridis_c() +
  geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2) +
  xlab("Pressure (hPa)") +
  ylab("Probability of low rain") +
  labs(title = "Low precipitaion (=1) or Not low precipitaion (=0) vs pressure",
       color = "Temperature (°C)") +
  theme(text = element_text(size = 14))
# caption = "red = fitted line, blue dashed = moving average")



# THE PREDICTED PROBABILITIES PHAT SER UT SOM SKIT HÄR MED!! 



# Which variable causes the largest variability in the probability of low rain:
# temperature or air pressure?
# Ans: 
# In which location does the temperature add the most extra information,
# in addition to pressure?
# Ans: 


# SVARA PÅ FRÅGORNA


##### 4 (c) #####
# If you want to predict the probability of low rain in Lund, which variable seems more
# useful, according to 4.(b): temperature or air pressure? - 


# MORE TODO HERE!!!

##### 4 (d) Standardized deviance residuals #####

##### 4 (e) Cook's D #####

##### 4 (f) R2 Cox-Snell and R2 Nagelkerke#####

##### 4 (g) AIC and BIC #####
(collectAIC <- data.frame(
  nr = seq(1, 3),
  model = c("temp", "pressure", "temp*pressure + loc"),
  AIC(model2b, model3a, model4a),
  BIC(model2b, model3a, model4a)))

# Conclusion: AIC and BIC lower for model4a with temp*pressure + loc.






