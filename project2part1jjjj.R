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
model0 <- glm(lowrain ~ 1, family = "binomial", data = weather)
summary(model0)

# Beta estimates and CI for log-odds
model0$coefficients
confLogOdds <- confint(model0)

# Beta estimates and CI for odds
odds <- exp(model0$coefficients)
confOdds <- exp(confint(model0))

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
    xlab("Temperature (Â°C)") +
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




# ska man bara kolla på z-värdet i summary och kolla om den är mindre än 0.05?
# wald test!!!!!!!!!!!!!!!!!!!!!!!!!




# logistic regression model
# log(Y) = B0 + B1*x + epsilon
# Transformed model in untransformed scale
# Y = exp(B0) * exp(B1)^x * exp(epsilon) = a * b^x * exp(epsilon)
# where a = exp(B0) and b = exp(B1)

# How does the odds of low rain change when the temperature is increased by 1 Â°C?
# Ans: b^1 = 0.93 i.e. odds decreases by 7%
# How does the odds of low rain change when the temperature is decreased by 1 Â°C?
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
  xlab("Temperature (Â°C)") +
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
  xlab("Temperature (Â°C)") +
  ylab("Probability of low rain") +
  labs(title = "Low precipitaion (=1) or Not low precipitaion (=0) vs temperature")
# caption = "red = fitted line, blue dashed = moving average") +
theme(text = element_text(size = 14))

##### 2 (e) Leverage #####
leverage <- influence(model2b)

lowrainPred <- cbind(weather, xbeta = predict(model2b), v = leverage$hat)
head(lowrainPred)



# VAD ÄR XBETA? samma som YHAT??????????????
#xbetahat, linjär prediktor, logoddsenhat




# Plot of leverage vs temperature with 1/n and 2(p+1)/n horizontal lines
(plotLevTemp2b <- ggplot(lowrainPred, aes(temp, v, ymin = 0, color = as.factor(lowrain))) + 
    geom_point() +
    geom_hline(yintercept = c(I(1/nrow(weather)))) +
    geom_hline(yintercept = 2*4/nrow(weather),
               color = "red", size = 1) +#(2*2 = 4 parameters)
    facet_wrap(~ lowrain) +
    xlab("Temperature (Â°C)") +
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



# FRÃGA: HUR MÃNGA LEVERAGE RÄKNAS SOM HIGH??????????????????????????



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
    xlab("Temperature (Â°C)") +
    ylab("Standardized deviance residuals") +
    labs(title = "Standardized deviance residuals vs temperature",
         color = "Y") +
    theme(text = element_text(size = 14)))

# Conclusion: No alarmingly large standardized deviance residuals.



# FRÃGA: STÄMMER DET, SKA VI EJ TA BORT NÂGRA FRÃN standardized deviance residuals???? Ta ej bort



##### 2 (g) Cook's D ####
# Add Cook's D
lowrainPred$D <- cooks.distance(model2b)
head(lowrainPred)

# Plot of Cook's D vs temperature with horizontal line for 4/n
(plotCooksDTemp2b <- ggplot(lowrainPred, aes(temp, D, color = as.factor(lowrain))) +
    geom_point() +
    geom_point(data = lowrainPred[outliersLeverage, ], color = "black",
               shape = 24, size = 3) +
    geom_hline(yintercept = 4/nrow(weather), color = "red", linetype = "dotted",
               size = 1) +
    facet_grid(cols = vars(lowrain)) +
    xlab("Temperature (Â°C)") +
    ylab("Cook's D") +
    labs(title = "Cook's D vs temp",
         color = "Y") +
    theme(text = element_text(size = 14)))

# Are there any observations that have had a large influence on the estimates?
# Ans: Yes.
# Are these observations the same as those that had the highest leverages?
# Ans: Yes, one of the previous 4.



# FRÃGA: SKA VI TA BORT MER BASERAT PÃ COOKS D??????????????????????????????????????
# Kallt och inte regnar (röd) - ej ta bort
# ovanlig temperatur, men normal regnmängd (blå)


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




# FRÃN OCH MED HÄR, SKA VI ALLTID HA PRESSURE - 1012 ELLER OK MED ENDAST PRESSURE NÄR
# VI EXEMPELVIS KÅR LEVERAGE OCH DEVIANCE RESIDUALS OSV? SPELAR DET NÅGON ROLL?





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




# FRÃGA: PRESSURE ELLER PRESSURE - 1012???? ÃNDRA ENHET I SÃ FALL!!!




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




# FRÅGA: VILKA SKA VI TA BORT? FINNS MASSOR AV KANDIDATER! Nej





##### 3 (d) Cook's D#####
# Add Cook's D
lowrainPred$D <- cooks.distance(model3a)
head(lowrainPred)

# Plot of Cook's D vs pressure with horizontal line for 4/n
(plotCooksDPressure3a <- ggplot(lowrainPred, aes(pressure, D, color = as.factor(lowrain))) +
    geom_point() +
    geom_point(data = lowrainPred[outliersLeverage, ], color = "black",
               shape = 24, size = 3) +
    geom_hline(yintercept = 4/nrow(weather), color = "red", linetype = "dotted",
               size = 1) +
    facet_grid(cols = vars(lowrain)) +
    xlab("Pressure (hPa)") +
    ylab("Cook's D") +
    labs(title = "Cook's D vs pressure",
         color = "Y") +
    theme(text = element_text(size = 14)))

# Conclusion: Only 1 with high leverage and Cook's D.


# SKA VI TA BORT FLER ÄN DEN ENSAMMA UPPE VID 0.08???????? Ja, men inga andra. Men avvakta med att ta bort tills senare



##### 3 (e) Comparing#####
# Compare the leverage, residual, and Cookâs distance plots with those for the temperature
# model from 2.(b). Which model seems best?

#install.packages("gridExtra")
library("gridExtra")

# Plots for comparing
grid.arrange(plotLevTemp2bOutliers, plotLevPressure3aOutliers, nrow=1, ncol=2)
grid.arrange(plotResTemp2b, plotResPressure3a, nrow=1, ncol=2)
grid.arrange(plotCooksDTemp2b, plotCooksDPressure3a, nrow=1, ncol=2)

# Conslusion: Lower leverage outliers for temperature, deviance residuals maybe better for
# pressure and lower Cook's D for temperature as well.



# Ans: RÄTT CONCLUSION????????????!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Temp


##### 3 (g) AIC and BIC#####
(collectAIC <- data.frame(
  nr = seq(1, 2),
  model = c("temp", "pressure"),
  AIC(model2b, model3a),
  BIC(model2b, model3a)))

# Conclusion: AIC and BIC lower for model3a with pressure.

##### 3 (f) R2 Cox-Snell and R2 Nagelkerke #####
logLik(model0)
(lnL0 <- logLik(model0)[1])
(R2CSMax <- 1 - (exp(lnL0))^(2/500))

# Collect the log likelihoods L(betahat)
collectAIC$loglik <- 
  c(logLik(model2b)[1],
    logLik(model3a)[1])

collectAIC$R2CS <- 1 - (exp(lnL0 - collectAIC$loglik))^(2/nrow(weather))
collectAIC$R2N <- collectAIC$R2CS/R2CSMax



#  TODO: TAR BORT RÄTT AV DET SOM SKA BORT EFTER ÃEVERAGE, RESIDUALS AND COOKS D



##### 4 ...or both with location?####
##### 4 (a) Fit with interaction and location #####
# Change reference location to Uppsala
weather$location <- relevel(weather$location, "Uppsala")

# Fit logistic regression model with temperature, pressure
model4a <- glm(lowrain ~ temp*I(pressure - 1012) + location, family = "binomial", data = weather)
summary(model4a) # pressure significant

# We use the deviance to construct a so called "likelihood ratio test". If D_diff bigger than
# chisq quantile then reject H0 at sig. level alpha since the diff is chisq distributed.

# Compare with model2b using anova
(anovaModel2b4a <- anova(model2b, model4a))
(D_diff <- anovaModel2b4a$Deviance[2]) # Diff in deviance
(f_diff <- anovaModel2b4a$Df[2]) # Diff in deg. of freedom. 

#chi2-quantile to compare D_diff with
qchisq(1 - 0.05, f_diff)
# or P-value:
pchisq(D_diff, f_diff, lower.tail = FALSE)

# Compare with model3a using anova
(anovaModel3a4a <- anova(model3a, model4a))
(D_diff <- anovaModel3a4a$Deviance[2])
(f_diff <- anovaModel3a4a$Df[2])

#chi2-quantile to compare D_diff with:
qchisq(1 - 0.05, f_diff)
# or P-value:
pchisq(D_diff, f_diff, lower.tail = FALSE)

# Conclusion: model4a is significantly better than both model2b and model3a.

##### 4 (b) #####
# predict for plotting
lowrainPred <- cbind(weather, phat = predict(model4a, type = "response"))

# Lowrain vs temp, in subplots by location
ggplot(lowrainPred, aes(x = temp, y = lowrain)) +
  geom_point() +
  facet_wrap(~ location) +
  # geom_smooth(se = FALSE, linetype = "dashed") +
  # geom_line(aes(y = phat), color = "red", size = 1) +
  geom_point(aes(y = phat, color = pressure)) +
  scale_color_viridis_c() +
  xlab("Temperature (Â°C)") +
  ylab("Probability of low rain") +
  labs(title = "Low precipitaion (=1) or Not low precipitaion (=0) vs temperature",
       color = "Pressure (hPa)") +
  theme(text = element_text(size = 14))
# caption = "red = fitted line, blue dashed = moving average")




# THE PREDICTED PROBABILITIES PHAT SER UT SOM SKIT HUR FÃR MAN PLOTTEN ATT SE OK UT??????????????


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
       color = "Temperature (Â°C)") +
  theme(text = element_text(size = 14))
# caption = "red = fitted line, blue dashed = moving average")



# THE PREDICTED PROBABILITIES PHAT SER UT SOM SKIT HÄR MED!! 



# Which variable causes the largest variability in the probability of low rain:
# temperature or air pressure? - Temperature.
# In which location does the temperature add the most extra information,
# in addition to pressure? - ????????????????????????????????????????????????


# SVARA PÃ FRÃGORNA!!!!


##### 4 (c) Weather in Lund#####
# If you want to predict the probability of low rain in Lund, which variable seems more
# useful, according to 4.(b): temperature or air pressure? - Pressure. 

# Extracting data for Lund only
I_lund <- weather$location == "Lund"
weatherLund <- weather[I_lund, ]
head(weatherLund)

model4c <- glm(lowrain ~ temp*I(pressure - 1012), family = "binomial", data = weatherLund)

# Beta estimates and CI for log-odds
cbind(model4c$coefficients,
      confint(model4c))

# Beta estimates and CI for odds
cbind(exp(model4c$coefficients),
      exp(confint(model4c)))

# Conclusion: Interaction term NOT significant. 

# Backward elimination using BIC as criterion
step(model4c, k = log(nrow(weatherLund))) # default AIC (k = 2):

# first step:
# best = remove interaction
# second best = do nothing

# second step (after removing interaction):
# best = remove temp
# second best = do nothing

# third step (after removing temp):
# best = do nothing

# Conclusion: Yes, it only chooses pressure although temparature was significant in model4c.



# MEN TEMP SIGIFICANT??????????????? VARFÖR TAR BACKWARD BORT DEN????



##### 4 (d) Standardized deviance residuals #####
# Leverage
leverage <- influence(model4a)

# Prediction
lowrainPred <- cbind(weather, xbeta = predict(model4a), v = leverage$hat)
head(lowrainPred)

# Deviance residuals
lowrainPred$devres <- leverage$dev.res

# Standardised deviance residuals
lowrainPred$devstd <- lowrainPred$devres/sqrt(1 - lowrainPred$v)
head(lowrainPred)

# Plot of standardized deviance residuals vs xbeta
# with horizontal lines for 0, +-2 and +-4.
(plotResXbeta4a <- ggplot(lowrainPred, aes(xbeta, devstd, color = as.factor(lowrain))) +
    geom_point() +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = c(-2, 2), color = "red", linetype = "dashed",
               size = 1) +
    geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dotted",
               size = 1) +
    xlab("xbeta") +
    ylab("Standardized deviance residuals") +
    labs(title = "Standardized deviance residuals vs xbeta",
         color = "Y") +
    theme(text = element_text(size = 14)))

# Plot of standardized deviance residuals vs temperature with colors according
# to pressure with horizontal lines for 0, +-2 and +-4.
(plotResTemp4a <- ggplot(lowrainPred, aes(x = temp, y = devstd)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = c(-2, 2), color = "red", linetype = "dashed",
               size = 1) +
    geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dotted",
               size = 1) +
    facet_wrap(~ location) +
    geom_point(aes(y = devstd, color = pressure)) +
    scale_color_viridis_c() +
    xlab("Temperature (°C)") +
    ylab("Standardized deviance residuals") +
    labs(title = "Standardized deviance residuals vs temperature",
         color = "Pressure (hPa)") +
    theme(text = element_text(size = 14)))

# Plot of standardized deviance residuals vs pressure with colors according
# to temperature with horizontal lines for 0, +-2 and +-4.
(plotResPressure4a <- ggplot(lowrainPred, aes(x = pressure, y = devstd)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = c(-2, 2), color = "red", linetype = "dashed",
               size = 1) +
    geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dotted",
               size = 1) +
    facet_wrap(~ location) +
    geom_point(aes(y = devstd, color = temp)) +
    scale_color_viridis_c() +
    xlab("Pressure (hPa)") +
    ylab("Standardized deviance residuals") +
    labs(title = "Standardized deviance residuals vs pressure",
         color = "Temp. (°C)") +
    theme(text = element_text(size = 14)))

# Plots for comparing
grid.arrange(plotResTemp2b, plotResTemp4a, nrow=2, ncol=1)
grid.arrange(plotResPressure3a, plotResPressure4a, nrow=2, ncol=1)

# Any problematic residuals? Also compare with the plots in 2.(f ) and 3.(c). Have the
# residuals improved?

# Conclusion: Maybe some problematic residuals, mostly for Y = 1 (upper) and Uppsala (lower).




# Hard to say if they hafe improved since we did not plot the both with color coding?????
# HOW TO COMPARE?????



##### 4 (e) Cook's D #####
# Add Cook's D
lowrainPred$D <- cooks.distance(model4a)
head(lowrainPred)

# Plot of Cook's D vs xbeta, horizontal line for 4/n
(plotCooksDXbeta4a <- ggplot(lowrainPred, aes(xbeta, D, color = as.factor(lowrain))) +
    geom_point() +
    geom_hline(yintercept = 4/nrow(weather), color = "red", linetype = "dotted",
               size = 1) +
    facet_grid(cols = vars(lowrain)) +
    xlab("xbeta") +
    ylab("Cook's D") +
    labs(title = "Cook's D vs xbeta",
         color = "Y") +
    theme(text = element_text(size = 14)))

# Plot of Cook's D vs temperature with colors according
# to pressure, horizontal line for 4/n
(plotCooksDTemp4a <- ggplot(lowrainPred, aes(temp, D)) +
    geom_point() +
    geom_hline(yintercept = 4/nrow(weather), color = "red", linetype = "dotted",
               size = 1) +
    facet_wrap(~ location) +
    geom_point(aes(y = D, color = pressure)) +
    scale_color_viridis_c() +
    xlab("Temperature (°C)") +
    ylab("Cook's D") +
    labs(title = "Cook's D vs temperature",
         color = "Pressure (hPa)") +
    theme(text = element_text(size = 14)))

# Plot of Cook's D vs pressure with colors according
# to temperature, horizontal line for 4/n
(plotCooksDPressure4a <- ggplot(lowrainPred, aes(pressure, D)) +
    geom_point() +
    geom_hline(yintercept = 4/nrow(weather), color = "red", linetype = "dotted",
               size = 1) +
    facet_wrap(~ location) +
    geom_point(aes(y = D, color = temp)) +
    scale_color_viridis_c() +
    xlab("Pressure (hPa)") +
    ylab("Cook's D") +
    labs(title = "Cook's D vs pressure",
         color = "Temp. (°C)") +
    theme(text = element_text(size = 14)))

# Plot for comparing
grid.arrange(plotCooksDTemp2b, plotCooksDTemp4a, nrow=1, ncol=2)
grid.arrange(plotCooksDPressure3a, plotCooksDPressure4a, nrow=1, ncol=2)

# Conclusion: Better for pressure but not temp.

##### 4 (g) AIC and BIC #####
(collectAIC <- data.frame(
  nr = seq(1, 3),
  model = c("temp", "pressure", "temp*pressure + loc"),
  AIC(model2b, model3a, model4a),
  BIC(model2b, model3a, model4a)))

# Conclusion: AIC and BIC lower for model4a with temp*pressure + loc.

##### 4 (f) R2 Cox-Snell and R2 Nagelkerke#####
logLik(model0)
(lnL0 <- logLik(model0)[1])
(R2CSMax <- 1 - (exp(lnL0))^(2/500))

# Collect the log likelihoods L(betahat)
collectAIC$loglik <- 
  c(logLik(model2b)[1],
    logLik(model3a)[1],
    logLik(model4a)[1])

collectAIC$R2CS <- 1 - (exp(lnL0 - collectAIC$loglik))^(2/nrow(weather))
collectAIC$R2N <- collectAIC$R2CS/R2CSMax

# Conslusion: R2CS and R2N highest for model4a.

##### 5 Goodness of fit####
library(pROC)
library(ResourceSelection)

##### 5 (a) #####
# Predicted probabilities phat for model2b, model3a and model4a
predPhat <- cbind(
  weather,
  phat2b = predict(model2b, type = "response"),
  phat3a = predict(model3a, type = "response"),
  phat4a = predict(model4a, type = "response"))
head(predPhat)

# Confusion matrix
# Calculate Y-hat using model2b, model3a and model4a
predPhat$yhat2b <- as.numeric(predPhat$phat2b > 0.5)
predPhat$yhat3a <- as.numeric(predPhat$phat3a > 0.5)
predPhat$yhat4a <- as.numeric(predPhat$phat4a > 0.5)

(row01 <- table(weather$lowrain))

(col012b <- table(predPhat$yhat2b))
(confusion2b <- table(predPhat$lowrain, predPhat$yhat2b))
(spec2b <- confusion2b[1, 1] / row01[1])
(sens2b <- confusion2b[2, 2] / row01[2])
(accu2b <- sum(diag(confusion2b)) / sum(confusion2b))
(prec2b <- confusion2b[2, 2] / col012b[2])

(col013a <- table(predPhat$yhat3a))
(confusion3a <- table(predPhat$lowrain, predPhat$yhat3a))
(spec3a <- confusion3a[1, 1] / row01[1])
(sens3a <- confusion3a[2, 2] / row01[2])
(accu3a <- sum(diag(confusion3a)) / sum(confusion3a))
(prec3a <- confusion3a[2, 2] / col013a[2])

(col014a <- table(predPhat$yhat4a))
(confusion4a <- table(predPhat$lowrain, predPhat$yhat4a))
(spec4a <- confusion4a[1, 1] / row01[1])
(sens4a <- confusion4a[2, 2] / row01[2])
(accu4a <- sum(diag(confusion4a)) / sum(confusion4a))
(prec4a <- confusion4a[2, 2] / col014a[2])

(collectConfusion <- data.frame(
  model = c("model2b", "model3a", "model4a"),
  Specificity = c(spec2b, spec3a, spec4a),
  Sensitivity = c(sens2b, sens3a, sens4a),
  Accuracy = c(accu2b, accu3a, accu4a),
  Precision = c(prec2b, prec3a, prec4a)))

#Is any of the models out-performing the others in all, or some, of the aspects?

# Conclusion: TODOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO!

# ##### 5 (b) #####
# # ROC-curves####
# # Calculate for model 0 and 3.
# (roc.0 <- roc(highpm10 ~ p.0, data = pred.phat))
# # save the coordinates in a data frame for plotting.
# roc.df.0 <- coords(roc.0, transpose = FALSE)
# roc.df.0$model <- "0"
# roc.df.0
# 
# (roc.3 <- roc(highpm10 ~ p.3, data = pred.phat))
# # save the coordinates in a data frame for plotting.
# roc.df.3 <- coords(roc.3, transpose = FALSE)
# roc.df.3$model <- "3"
# head(roc.df.3)
# 
# # Create the data for the Ideal model by hand:
# roc.df.ideal <- data.frame(sensitivity = c(0, 1, 1),
#                            specificity = c(1, 1, 0),
#                            threshold = c(NA, NA, NA))
# roc.df.ideal$model <- "ideal"
# 
# # experiment with different values of "limit" to find the
# # optimal combination of sens and spec.
# limit <- 0.635
# roc.df.3[roc.df.3$sensitivity > limit & 
#            roc.df.3$specificity > limit, ]
# ##
# I_max.3 <- which(roc.df.3$sensitivity > limit & 
#                    roc.df.3$specificity > limit)
# 
# # Built-in function for plotting one Roc-curve
# # Note that the x-axis is reversed!
# # If we want the diagonal with geom_abline, it has to be reversed!
# # Since both axes are 0-1, we want a square plot area:
# # + coord_fixed()
# ggroc(roc.3) +
#   geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
#   coord_fixed() +
#   labs(title = "ROC-curve for model 3")
# 
# # Plot the three ROC-curves:
# # Use geom_path() instead of geom_line()
# #
# # For model 3 the curve is color coded according to
# # the threshold. The color scheme is set by
# # + scale_color_gradientn(colours = rainbow(5)) +
# #
# # Note that the x-axis is reversed!
# # + scale_x_reverse()
# # You could use 1 - spec instead.
# # If we want the diagonal with geom_abline, it has to be reversed!
# #
# # Since both axes are 0-1, we want a square plot area:
# # + coord_fixed()
# #
# ggplot(roc.df.3, aes(specificity, sensitivity)) +
#   geom_path(aes(color = threshold), size = 2) +
#   geom_path(data = roc.df.ideal, color = "black", size = 1) +
#   geom_path(data = roc.df.0, color = "red", size = 1,
#             linetype = "dashed") +
#   geom_point(data = roc.df.3[I_max.3, ], color = "black", size = 3) +
#   #  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
#   scale_color_gradientn(colours = rainbow(5)) +
#   coord_fixed() +       # square plotting area
#   scale_x_reverse() +   # Reverse scale on the x-axis!
#   labs(title = "ROC-curve for model 3",
#        caption = "Black dot = optimal threshold") +
#   theme(text = element_text(size = 14))
# 
# # ROC-curves for all models####
# roc.1 <- roc(highpm10 ~ p.1, data = pred.phat)
# roc.df.1 <- coords(roc.1, transpose = FALSE)
# roc.df.1$model <- "1"
# roc.2 <- roc(highpm10 ~ p.2, data = pred.phat)
# roc.df.2 <- coords(roc.2, transpose = FALSE)
# roc.df.2$model <- "2"
# roc.4 <- roc(highpm10 ~ p.4, data = pred.phat)
# roc.df.4 <- coords(roc.4, transpose = FALSE)
# roc.df.4$model <- "4"
# roc.5 <- roc(highpm10 ~ p.5, data = pred.phat)
# roc.df.5 <- coords(roc.5, transpose = FALSE)
# roc.df.5$model <- "5"
# roc.6 <- roc(highpm10 ~ p.6, data = pred.phat)
# roc.df.6 <- coords(roc.6, transpose = FALSE)
# roc.df.6$model <- "6"
# roc.red <- roc(highpm10 ~ p.red, data = pred.phat)
# roc.df.red <- coords(roc.red, transpose = FALSE)
# roc.df.red$model <- "red"
# roc.oslo <- roc(highpm10 ~ p.oslo, data = pred.phat)
# roc.df.oslo <- coords(roc.oslo, transpose = FALSE)
# roc.df.oslo$model <- "oslo"
# 
# roc.df <- rbind(roc.df.0, roc.df.1, roc.df.2, roc.df.3, 
#                 roc.df.4, roc.df.5, roc.df.6, roc.df.red,
#                 roc.df.oslo)
# 
# # Plot all the curves, in different colors:
# ggplot(roc.df, aes(specificity, sensitivity,
#                    color = model)) +
#   geom_path(size = 1) +
#   coord_fixed() +       # square plotting area
#   scale_x_reverse() +   # Reverse scale on the x-axis!
#   labs(title = "ROC-curves for all the models") +
#   theme(text = element_text(size = 14))
# 
# # AUC####
# roc.3
# auc(roc.3)
# # Confidence interval for AUC
# (ci.3 <- ci(roc.3))
# # lower limit:
# ci.3[1]
# # AUC:
# ci.3[2]
# # upper limit:
# ci.3[3]
# 
# #Collect AUC and intervals for all the models:
# (aucs <- 
#     data.frame(
#       model = c("0", "1", "2", "red", "3", "4", "5", "6", "oslo"),
#       auc = c(auc(roc.0), auc(roc.1), auc(roc.2), auc(roc.red),
#               auc(roc.3), auc(roc.4), auc(roc.5), auc(roc.6),
#               auc(roc.oslo)),
#       lwr = c(ci(roc.0)[1], ci(roc.1)[1],
#               ci(roc.2)[1], ci(roc.red)[1],
#               ci(roc.3)[1], ci(roc.4)[1],
#               ci(roc.5)[1], ci(roc.6)[1],
#               ci(roc.oslo)[1]),
#       upr = c(ci(auc(roc.0))[3], ci(auc(roc.1))[3],
#               ci(auc(roc.2))[3], ci(auc(roc.red))[3],
#               ci(auc(roc.3))[3], ci(auc(roc.4))[3],
#               ci(auc(roc.5))[3], ci(auc(roc.6))[3],
#               ci(auc(roc.oslo))[3])))
# 
# # Compare the AUC for the models:
# roc.test(roc.0, roc.oslo)
# roc.test(roc.1, roc.oslo)
# roc.test(roc.2, roc.oslo)


##### 5 (c) #####

##### 5 (d) #####
