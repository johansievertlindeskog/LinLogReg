##### Project 2 - part 1  #####

# Initializing
rm(list = ls())
load("~/R/LinLogReg/Data/weather.rda")

##### 1 The null model####

##### 1 (a) #####
weather$lowrain <- as.numeric(weather$rain < 25)
p <- mean(weather$lowrain)

# Odds and log-odds for LOW precipitation (<25 mm)
odds <- p/(1 - p)
logOdds <- log(odds)

##### 1 (b) Fit with intercept #####
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
    xlab("Temperature (°C)") +
    ylab("Lowrain (1/0)") +
    labs(title = "Probability of low rain vs temperature") +
    theme(text = element_text(size = 18))
)

# Does it seem reasonable to use temp as covariate? - Yes!

##### 2 (b) Fit with temp #####
# Fit logistic regression model with temperature
model2b <- glm(lowrain ~ temp, family = "binomial", data = weather)
summary(model2b) # temp significant

# Beta estimates and CI for log-odds
cbind(model2b$coefficients,
      confint(model2b))

# Beta estimates and CI for odds
cbind(exp(model2b$coefficients),
      exp(confint(model2b)))

# Wald test: kolla på belopp av z-värde och jämför med kvantilen, eller se p-värdet som vanligt.

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

# There is a difference in the probability of low rain when we change the temparature from
# -10 °C to -9 °C, compared with when we change from 9 °C to 10 °C. This is due to the S-shape
# of the probability curve, which is growing more slowly at probabilities closer to 0 and 1
# compared to 0.5. However, the decrease is still, as shown before, 7% for the odds when
# increasing the temperature with 1°C in both cases.

##### 2 (d) Lowrain vs temp #####
# predict for plotting
# phat = estimated probabilities
lowrainPred2b <- cbind(weather, phat = predict(model2b, type = "response"))

ggplot(lowrainPred2b, aes(temp, lowrain)) +
  geom_point() +
  geom_smooth(se = FALSE, linetype = "dashed") +
  geom_line(aes(y = phat), color = "red", size = 1) +
  xlab("Temperature (°C)") +
  ylab("Lowrain") +
  labs(title = "Low precipitaion (=1) or Not low precipitaion (=0) vs temperature") +
  theme(text = element_text(size = 15))
# caption = "red = fitted line, blue dashed = moving average")

# logit = logodds with s.e. for constructing C.I.
lowrainPred2b <- cbind(lowrainPred2b, logit = predict(model2b, se.fit = TRUE))

# Remove unnecessary variable:
lowrainPred2b$logit.residual.scale <- NULL

# Calculate confidence intervals for the log odds
# standard normal quantile:
(lambda <- qnorm(1 - 0.05/2))
lowrainPred2b$logit.lwr <- lowrainPred2b$logit.fit - lambda*lowrainPred2b$logit.se.fit
lowrainPred2b$logit.upr <- lowrainPred2b$logit.fit + lambda*lowrainPred2b$logit.se.fit

# transform the log-odds intervals into C.I. for odds
lowrainPred2b$odds.lwr <- exp(lowrainPred2b$logit.lwr)
lowrainPred2b$odds.upr <- exp(lowrainPred2b$logit.upr)

# transform the odds intervals into C.I. for p
lowrainPred2b$p.lwr <- lowrainPred2b$odds.lwr/(1 + lowrainPred2b$odds.lwr)
lowrainPred2b$p.upr <- lowrainPred2b$odds.upr/(1 + lowrainPred2b$odds.upr)
head(lowrainPred2b)

# plot the intervals:
ggplot(lowrainPred2b, aes(temp, lowrain)) +
  geom_point() +
  geom_smooth(se = FALSE, linetype = "dashed") +
  geom_line(aes(y = phat), color = "red", size = 1) +
  geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2) +
  xlab("Temperature (°C)") +
  ylab("Lowrain") +
  labs(title = "Low precipitaion (=1) or Not low precipitaion (=0) vs temperature") +
theme(text = element_text(size = 15))
# caption = "red = fitted line, blue dashed = moving average") +

##### 2 (e) Leverage #####
leverage <- influence(model2b)

lowrainPred2b <- cbind(weather, xbeta = predict(model2b), v = leverage$hat)
head(lowrainPred2b)

# xbetahat är vår linjära prediktor dvs logoddshat. Yhat är det vi modellerar
# dvs lowrain eller not lowrain. xbetahat är värdena som vi drar slutsats ifrån
# huruvida det var lowrain eller not lowrain.

# Plot of leverage vs temperature with 1/n and 2(p+1)/n horizontal lines
(plotLevTemp2b <- ggplot(lowrainPred2b, aes(temp, v, ymin = 0, color = as.factor(lowrain))) + 
    geom_point() +
    geom_hline(yintercept = c(I(1/nrow(weather)))) +
    geom_hline(yintercept = 2*2/nrow(weather),
               color = "red", size = 1) +#(2*nbr of parameters = 2*2)
    facet_wrap(~ lowrain) +
    xlab("Temperature (°C)") +
    ylab("Leverage") +
    labs(title = "Leverage vs temperature",
         color = "Y") +
    theme(text = element_text(size = 14)))

# Find all leverage above 0.006 (arbitrary choice):
outliersLeverage <- which(lowrainPred2b$v > 0.006)

# ... and highlight the outliers
(plotLevTemp2bOutliers <- plotLevTemp2b +
    geom_point(data = lowrainPred2b[outliersLeverage, ], size = 3, 
               color = "black", shape = 24) +
    labs(title = "Leverage vs temperature")) #red = 2(p+1)/n, black = 0.0073

# Conclusion: Some observations with high leverage.

##### 2 (f) Standardised deviance residuals #####
# Deviance residuals
lowrainPred2b$devres <- leverage$dev.res

# Standardised deviance residuals
lowrainPred2b$devstd <- lowrainPred2b$devres/sqrt(1 - lowrainPred2b$v)
head(lowrainPred2b)

# Plot of standardized deviance residuals vs temperature
# with horizontal lines for 0, +-2 and +-4.
(plotResTemp2b <- ggplot(lowrainPred2b, aes(temp, devstd, color = as.factor(lowrain))) +
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

##### 2 (g) Cook's D #####
# Add Cook's D
lowrainPred2b$D <- cooks.distance(model2b)
head(lowrainPred2b)

# Plot of Cook's D vs temperature with horizontal line for 4/n
(plotCooksDTemp2b <- ggplot(lowrainPred2b, aes(temp, D, color = as.factor(lowrain))) +
    geom_point() +
    geom_point(data = lowrainPred2b[outliersLeverage, ], color = "black",
               shape = 24, size = 3) +
    geom_hline(yintercept = 4/nrow(weather), color = "red", linetype = "dotted",
               size = 1) +
    facet_grid(cols = vars(lowrain)) +
    xlab("Temperature (°C)") +
    ylab("Cook's D") +
    labs(title = "Cook's D vs temperature",
         color = "Y") +
    theme(text = element_text(size = 14)))

# Are there any observations that have had a large influence on the estimates?
# Ans: Maybe some but since they are in a group together we keep them.
# Are these observations the same as those that had the highest leverages?
# Ans: Yes, one of the previous 4 for the red line, found when we dont have
# low rain and the temperature is unusually low.

##### 3 ...or pressure...####

##### 3 (a) Fit with pressure #####
# Plot lowrain vs pressure with moving average
(
  plotRainPressure <- ggplot(data = weather, 
                             aes(x = pressure, y = lowrain )) + 
    geom_smooth(method = loess) + # moving average
    geom_point(size = 1) +
    xlab("Pressure (hPa)") +
    ylab("Lowrain") +
    labs(title = "Probability of low rain vs pressure") +
    theme(text = element_text(size = 15))
)

# Fit logistic regression model with pressure
model3a <- glm(lowrain ~ I(pressure - 1012), family = "binomial", data = weather)
summary(model3a) # pressure significant

# Beta estimates and CI for log-odds
cbind(model3a$coefficients,
      confint(model3a))

# Beta estimates and CI for odds
cbind(exp(model3a$coefficients),
      exp(confint(model3a)))

# predict for plotting
lowrainPred3a <- cbind(weather, phat = predict(model3a, type = "response"))

ggplot(lowrainPred3a, aes(I(pressure - 1012), lowrain)) +
  geom_point() +
  geom_smooth(se = FALSE, linetype = "dashed") +
  geom_line(aes(y = phat), color = "red", size = 1) +
  xlab("Pressure (0 = 1012 hPa)") +
  ylab("Lowrain") +
  labs(title = "Low precipitaion (=1) or Not low precipitaion (=0) vs pressure") +
  theme(text = element_text(size = 15))
# caption = "red = fitted line, blue dashed = moving average")

# logit = logodds with s.e. for constructing C.I.
lowrainPred3a <- cbind(
  lowrainPred3a,
  logit = predict(model3a, se.fit = TRUE))
head(lowrainPred3a)

# Remove unnecessary variable:
lowrainPred3a$logit.residual.scale <- NULL

# Calculate confidence intervals for the log odds
# standard normal quantile:
(lambda <- qnorm(1 - 0.05/2))
lowrainPred3a$logit.lwr <- lowrainPred3a$logit.fit - lambda*lowrainPred3a$logit.se.fit
lowrainPred3a$logit.upr <- lowrainPred3a$logit.fit + lambda*lowrainPred3a$logit.se.fit

# transform the log-odds intervals into C.I. for odds
lowrainPred3a$odds.lwr <- exp(lowrainPred3a$logit.lwr)
lowrainPred3a$odds.upr <- exp(lowrainPred3a$logit.upr)

# transform the odds intervals into C.I. for p
lowrainPred3a$p.lwr <- lowrainPred3a$odds.lwr/(1 + lowrainPred3a$odds.lwr)
lowrainPred3a$p.upr <- lowrainPred3a$odds.upr/(1 + lowrainPred3a$odds.upr)
head(lowrainPred3a)

# plot the intervals:
ggplot(lowrainPred3a, aes(I(pressure - 1012), lowrain)) +
  geom_point() +
  geom_smooth(se = FALSE, linetype = "dashed") +
  geom_line(aes(y = phat), color = "red", size = 1) +
  geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2) +
  xlab("Pressure (0 = 1012 hPa)") +
  ylab("Lowrain") +
  labs(title = "Low precipitaion (=1) or Not low precipitaion (=0) vs pressure") +
  theme(text = element_text(size = 15))
# caption = "red = fitted line, blue dashed = moving average")

##### 3 (b) Leverage #####
leverage <- influence(model3a)

lowrainPred3a <- cbind(weather, xbeta = predict(model3a), v = leverage$hat)
head(lowrainPred3a)

# Plot of leverage vs pressure with 1/n and 2(p+1)/n horizontal lines
(plotLevPressure3a <- ggplot(lowrainPred3a, aes(pressure, v, ymin = 0, color = as.factor(lowrain))) + 
    geom_point() +
    geom_hline(yintercept = c(I(1/nrow(weather)))) +
    geom_hline(yintercept = 2*2/nrow(weather),
               color = "red", size = 1) + #(2*nbr of parametes = 2*2)
    facet_wrap(~ lowrain) +
    xlab("Pressure (hPa)") +
    ylab("Leverage") +
    labs(title = "Leverage vs pressure", color = "Y") +
    theme(text = element_text(size = 14)))

# Find all leverage above 0.006 (arbitrary choice):
outliersLeverage <- which(lowrainPred3a$v > 0.006)

# ... and highlight the outliers
(plotLevPressure3aOutliers <- plotLevPressure3a +
    geom_point(data = lowrainPred3a[outliersLeverage, ], size = 3, 
               color = "black", shape = 24) +
    labs(title = "Leverage vs pressure")) #red = 2(p+1)/n, black = 0.0073

# Conclusion: Some observations with high leverage but we will not remove them,
# we wait until we have fitted to biggest model first.

##### 3 (c) Standardised deviance residuals #####
# Deviance residuals
lowrainPred3a$devres <- leverage$dev.res

# Standardised deviance residuals
lowrainPred3a$devstd <- lowrainPred3a$devres/sqrt(1 - lowrainPred3a$v)
head(lowrainPred3a)

# Plot of standardized deviance residuals vs pressure
# with horizontal lines for 0, +-2 and +-4.
(plotResPressure3a <- ggplot(lowrainPred3a, aes(pressure, devstd, color = as.factor(lowrain))) +
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

##### 3 (d) Cook's D #####
# Add Cook's D
lowrainPred3a$D <- cooks.distance(model3a)
head(lowrainPred3a)

# Plot of Cook's D vs pressure with horizontal line for 4/n
(plotCooksDPressure3a <- ggplot(lowrainPred3a, aes(pressure, D, color = as.factor(lowrain))) +
    geom_point() +
    geom_point(data = lowrainPred3a[outliersLeverage, ], color = "black",
               shape = 24, size = 3) +
    geom_hline(yintercept = 4/nrow(weather), color = "red", linetype = "dotted",
               size = 1) +
    facet_grid(cols = vars(lowrain)) +
    xlab("Pressure (hPa)") +
    ylab("Cook's D") +
    labs(title = "Cook's D vs pressure",
         color = "Y") +
    theme(text = element_text(size = 14)))

# Highlight the outlier in Cook's D
outliersCooksD <- which(lowrainPred3a$D > 0.06)

# Plot of Cook's D vs pressure with highlited outlier, with horizontal line for 4/n
(plotCooksDPressure3a <- ggplot(lowrainPred3a, aes(pressure, D, color = as.factor(lowrain))) +
    geom_point() +
    geom_point(data = lowrainPred3a[outliersLeverage, ], color = "black",
               shape = 24, size = 3) +
    geom_point(data = lowrainPred3a[outliersCooksD, ], color = "red",
               shape = 24, size = 3) +
    geom_hline(yintercept = 4/nrow(weather), color = "red", linetype = "dotted",
               size = 1) +
    facet_grid(cols = vars(lowrain)) +
    xlab("Pressure (hPa)") +
    ylab("Cook's D") +
    labs(title = "Cook's D vs pressure",
         color = "Y") +
    theme(text = element_text(size = 14)))

# Conclusion: On for the red line with both high leverage and Cook's D but also one
# outlier for the blue line from only Cook's D.

##### 3 (e) Comparing #####
# Compare the leverage, residual, and Cook's distance plots with those for the temperature
# model from 2.(b). Which model seems best?

#install.packages("gridExtra")
library("gridExtra")

# Plots for comparing
grid.arrange(plotLevTemp2bOutliers, plotLevPressure3aOutliers, nrow=1, ncol=2)
grid.arrange(plotResTemp2b, plotResPressure3a, nrow=1, ncol=2)
grid.arrange(plotCooksDTemp2b, plotCooksDPressure3a, nrow=1, ncol=2)

# Conslusion: The model with temperature looks better, looking at leverage outliers,
# deviance residuals Cook's D.

##### 3 (g) AIC and BIC #####
(collectAIC <- data.frame(
  nr = seq(1, 2),
  model = c("temp", "pressure"),
  AIC(model2b, model3a),
  BIC(model2b, model3a)))

# Conclusion: AIC and BIC lower for model3a with pressure.

##### 3 (f) R2 Cox-Snell and R2 Nagelkerke #####
logLik(model0)
(lnL0 <- logLik(model0)[1])
(R2CSMax <- 1 - (exp(lnL0))^(2/nrow(weather)))

# Collect the log likelihoods L(betahat)
collectAIC$loglik <- 
  c(logLik(model2b)[1],
    logLik(model3a)[1])

collectAIC$R2CS <- 1 - (exp(lnL0 - collectAIC$loglik))^(2/nrow(weather))
collectAIC$R2N <- collectAIC$R2CS/R2CSMax
collectAIC

# Conclusion: R2 Cox-Snell and R2 Nagelkerke higher for model3a with pressure.

##### 4 ...or both with location?####

##### 4 (a) Fit with interaction and location #####
# Investigating the reference location
table(weather$location, weather$lowrain)

# Conclusion: Choose from lowrain (=1) since it has the smallest number of observations,
# and from this choose Uppsala as reference since it got the largest of observations in
# in this group.

# Set Uppsala as reference location
weather$location <- relevel(weather$location, "Uppsala")

# Fit logistic regression model with temperature, pressure
model4a <- glm(lowrain ~ temp*I(pressure - 1012) + location,
               family = "binomial", data = weather)
summary(model4a) # pressure significant

confint(model4a)

# We use the deviance to construct a so called "likelihood ratio test". If D_diff bigger than
# chisq quantile then reject H0 at sig. level alpha since the diff is chisq distributed.

# Remember to compare NESTED models when using anova for likelihood ratio test!

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
lowrainPred4a <- cbind(weather, phat = predict(model4a, type = "response"))

# Lowrain vs temp with pressure in color, in subplots by location
ggplot(lowrainPred4a, aes(x = temp, y = lowrain)) +
  geom_point() +
  facet_wrap(~ location) +
  geom_point(aes(y = phat, color = pressure)) +
  scale_color_viridis_c() +
  xlab("Temperature (°C)") +
  ylab("Probability of low rain") +
  labs(title = "Low precipitaion (=1) or Not low precipitaion (=0) vs temperature",
       color = "Pressure (hPa)") +
  theme(text = element_text(size = 14))
# caption = "red = fitted line, blue dashed = moving average")

# Same again but now lowrain vs pressure with temp in color, in subplots by location
ggplot(lowrainPred4a, aes(x = pressure, y = lowrain)) +
  geom_point() +
  facet_wrap(~ location) +
  geom_point(aes(y = phat, color = temp)) +
  scale_color_viridis_c() +
  xlab("Pressure (hPa)") +
  ylab("Probability of low rain") +
  labs(title = "Low precipitaion (=1) or Not low precipitaion (=0) vs pressure",
       color = "Temperature (°C)") +
  theme(text = element_text(size = 14))
# caption = "red = fitted line, blue dashed = moving average")

# Conslusion: Temperature causes the largest variability in the probability of low rain
# by far. The probabilities are way more spread out for a given temperature compared
# to pressure. Temperature add the most extra information for Abisko. If we look 
# carefully, we can find sort of S-shaped kurves if we follow the data points with
# the same pressure.

##### 4 (c) Weather in Lund #####
# Pressure seems more useful for predicting the probability of low rain in Lund
# since a very nice S-shaped pattern is seen for this curve.

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

# Conclusion: Backward elimination chooses pressure although temparature was significant
# in model4c. Expected since we before noticed that pressue gave most information.

##### 4 (d) Standardized deviance residuals #####
# Leverage
leverage <- influence(model4a)

# Prediction
lowrainPred4a <- cbind(weather, xbeta = predict(model4a), v = leverage$hat)
head(lowrainPred4a)

# Deviance residuals
lowrainPred4a$devres <- leverage$dev.res

# Standardised deviance residuals
lowrainPred4a$devstd <- lowrainPred4a$devres/sqrt(1 - lowrainPred4a$v)
head(lowrainPred4a)

# Plot of standardized deviance residuals vs xbeta
# with horizontal lines for 0, +-2 and +-4.
(plotResXbeta4a <- ggplot(lowrainPred4a, aes(xbeta, devstd, color = as.factor(lowrain))) +
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
(plotResTemp4a <- ggplot(lowrainPred4a, aes(x = temp, y = devstd)) +
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
(plotResPressure4a <- ggplot(lowrainPred4a, aes(x = pressure, y = devstd)) +
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

# Conclusion: Some of the residual values looks better but some worse. For Lund and
# Abisko they look fine but for Uppsala some high residuals. In generall no
# problematic residuals. 

##### 4 (e) Cook's D #####
# Add Cook's D
lowrainPred4a$D <- cooks.distance(model4a)
head(lowrainPred4a)

# Highlighting the 2 possible outliers (seen when plotting below)
outliersCooksD4a <- which(lowrainPred4a$D > 0.02)

# Plot of Cook's D vs xbeta, horizontal line for 4/n
(plotCooksDXbeta4a <- ggplot(lowrainPred4a, aes(xbeta, D, color = as.factor(lowrain))) +
    geom_point() +
    geom_hline(yintercept = 4/nrow(weather), color = "red", linetype = "dotted",
               size = 1) +
    geom_point(data = lowrainPred4a[outliersCooksD4a, ], color = "red",
               shape = 24, size = 3) +
    facet_grid(cols = vars(lowrain)) +
    xlab("xbeta") +
    ylab("Cook's D") +
    labs(title = "Cook's D vs xbeta",
         color = "Y") +
    theme(text = element_text(size = 14)))

# Plot of Cook's D vs temperature with colors according
# to pressure, horizontal line for 4/n
(plotCooksDTemp4a <- ggplot(lowrainPred4a, aes(temp, D)) +
    geom_point() +
    geom_hline(yintercept = 4/nrow(weather), color = "red", linetype = "dotted",
               size = 1) +
    geom_point(data = lowrainPred4a[outliersCooksD4a, ], color = "red",
               shape = 24, size = 3) +
    facet_grid(rows = vars(lowrain), cols = vars(location)) +
    geom_point(aes(y = D, color = pressure)) +
    scale_color_viridis_c() +
    xlab("Temperature (°C)") +
    ylab("Cook's D") +
    labs(title = "Cook's D vs temperature",
         color = "Pressure (hPa)") +
    theme(text = element_text(size = 14)))

# Plot of Cook's D vs pressure with colors according
# to temperature, horizontal line for 4/n
(plotCooksDPressure4a <- ggplot(lowrainPred4a, aes(pressure, D)) +
    geom_point() +
    geom_hline(yintercept = 4/nrow(weather), color = "red", linetype = "dotted",
               size = 1) +
    geom_point(data = lowrainPred4a[outliersCooksD4a, ], color = "red",
               shape = 24, size = 3) +
    facet_grid(rows = vars(lowrain), cols = vars(location)) +
    geom_point(aes(y = D, color = temp)) +
    scale_color_viridis_c() +
    xlab("Pressure (hPa)") +
    ylab("Cook's D") +
    labs(title = "Cook's D vs pressure",
         color = "Temp. (°C)") +
    theme(text = element_text(size = 14)))

# Conclusion: At least two alarmingly high outliers, will be removed later.

# Plot for comparing
grid.arrange(plotCooksDTemp2b, plotCooksDPressure3a, nrow=2, ncol=1)
plotCooksDTemp4a
plotCooksDPressure4a

# Conclusion: Better Cook's D for pressure but not temp.

##### 4 (g) AIC and BIC #####
(collectAIC <- data.frame(
  nr = seq(1, 3),
  model = c("temp", "pressure", "temp*pressure + loc"),
  AIC(model2b, model3a, model4a),
  BIC(model2b, model3a, model4a)))

# Conclusion: AIC and BIC lower for model4a with temp*pressure + loc.

##### 4 (f) R2 Cox-Snell and R2 Nagelkerke #####
logLik(model0)
(lnL0 <- logLik(model0)[1])
(R2CSMax <- 1 - (exp(lnL0))^(2/nrow(weather)))

# Collect the log likelihoods L(betahat)
collectAIC$loglik <- 
  c(logLik(model2b)[1],
    logLik(model3a)[1],
    logLik(model4a)[1])

collectAIC$R2CS <- 1 - (exp(lnL0 - collectAIC$loglik))^(2/nrow(weather))
collectAIC$R2N <- collectAIC$R2CS/R2CSMax
collectAIC

# Conslusion: R2CS and R2N highest for model4a.

##### 5 Goodness of fit####
library(pROC)
library(ResourceSelection)

##### 5 (a) Specificity, sensitivity, accuracy and precision #####
# Removing the previously identified outliers from our data set and re-fit ALL models with
# this data set, otherwise we cannot use predict with the new data set since the old models
# are fitted with the old data set. Use update-function to avoid re-fitting manually.
weatherExc <- weather[-outliersCooksD4a, ]

# Predicted probabilities phat for model2b, model3a and model4a
predPhat <- cbind(
  weatherExc,
  phat0 = predict(update(model0, data = weatherExc), type = "response"),
  phat2b = predict(update(model2b, data = weatherExc), type = "response"),
  phat3a = predict(update(model3a, data = weatherExc), type = "response"),
  phat4a = predict(update(model4a, data = weatherExc), type = "response"))
head(predPhat)

# Confusion matrix and specificity, sensitivity, accuracy and precision
# Calculate Y-hat (i.e. lowrain or not lowrain) using model2b, model3a and model4a
predPhat$yhat0 <- as.numeric(predPhat$phat0 > 0.5)
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

# Conclusion: model4a has by far the best sensitivity, other values similar to each other.

# ##### 5 (b) ROC-curves and AUC #####
# ROC-curves for all models
(roc0 <- roc(lowrain ~ phat0, data = predPhat))
# save the coordinates in a data frame for plotting.
rocDf0 <- coords(roc0, transpose = FALSE)
rocDf0$model <- "(5)"

roc2b <- roc(lowrain ~ phat2b, data = predPhat)
rocDf2b <- coords(roc2b, transpose = FALSE)
rocDf2b$model <- "(6)"

roc3a <- roc(lowrain ~ phat3a, data = predPhat)
rocDf3a <- coords(roc3a, transpose = FALSE)
rocDf3a$model <- "(7)"

roc4a <- roc(lowrain ~ phat4a, data = predPhat)
rocDf4a <- coords(roc4a, transpose = FALSE)
rocDf4a$model <- "(8)"

rocDf <- rbind(rocDf0, rocDf2b, rocDf3a, rocDf4a)

# Create the data for the Ideal model by hand (to illustate ideal ROC-curve in graph)
rocDfIdeal <- data.frame(sensitivity = c(0, 1, 1),
                           specificity = c(1, 1, 0),
                           threshold = c(NA, NA, NA))
rocDfIdeal$model <- "ideal"

# Finding optimal threshold for model4a
limit4a <- 0.7339 # this value was found by trial and error (manually)
rocDf4a[rocDf4a$sensitivity > limit4a & 
          rocDf4a$specificity > limit4a, ]

maxIndex4a <- which(rocDf4a$sensitivity > limit4a & 
                      rocDf4a$specificity > limit4a)

maxIndex4a <- maxIndex4a[2] # Pick out the one with the biggest specificity since same sensitivity
threshold4a <- rocDf4a$threshold[maxIndex4a]

# Built-in function for plotting one Roc-curve
# Note that the x-axis is reversed!
# If we want the diagonal with geom_abline, it has to be reversed!
# Since both axes are 0-1, we want a square plot area: + coord_fixed()

# ROC-curve for model4a
ggroc(roc4a) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  coord_fixed() +
  labs(title = "ROC-curve for model4a")

# Plot the three ROC-curves:
# Use geom_path() instead of geom_line()
# For model4a the curve is color coded according to
# the threshold. The color scheme is set by
# + scale_color_gradientn(colours = rainbow(5)) +

# ROC-curve for model4a with ideal ROC-curve and threshold with color coding
ggplot(rocDf4a, aes(specificity, sensitivity)) +
  geom_path(aes(color = threshold), size = 2) +
  geom_path(data = rocDfIdeal, color = "black", size = 1) +
  geom_path(data = rocDf0, color = "red", size = 1,
            linetype = "dashed") +
  geom_point(data = rocDf4a[maxIndex4a, ], color = "black", size = 3) +
  #  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  scale_color_gradientn(colours = rainbow(5)) +
  coord_fixed() +       # square plotting area
  scale_x_reverse() +   # Reverse scale on the x-axis!
  labs(title = "ROC-curve for model4a") +
  theme(text = element_text(size = 14))
# caption = "Black dot = optimal threshold"

# Plot all the curves in different colors
ggplot(rocDf, aes(specificity, sensitivity,
                   color = model)) +
  geom_path(data = rocDfIdeal, color = "black", size = 1) +
  geom_path(size = 1) +
  coord_fixed() +       # square plotting area
  scale_x_reverse() +   # Reverse scale on the x-axis!
  xlab("Specificity") +
  ylab("Sensitivity") +
  labs(title = "ROC-curves") +
  theme(text = element_text(size = 14))

# Calculating AUC (Area Under the Curve)
roc4a
auc(roc4a)
# CI for AUC
(ci4a <- ci(roc4a))
# lower limit (index 1)
ci4a[1]
# AUC (index 2)
ci4a[2]
# upper limit (index 3)
ci4a[3]

#Collect AUC and intervals for all the models:
(aucs <- 
    data.frame(
      model = c("0", "2b", "3a", "4a"),
      auc = c(auc(roc0), auc(roc2b), auc(roc3a), auc(roc4a)),
      lwr = c(ci(roc0)[1], ci(roc2b)[1],
              ci(roc3a)[1], ci(roc4a)[1]),
      upr = c(ci(auc(roc0))[3], ci(auc(roc2b))[3],
              ci(auc(roc3a))[3], ci(auc(roc4a))[3])))

# Compare the AUC for the models:
roc.test(roc2b, roc3a)
roc.test(roc2b, roc4a)
roc.test(roc3a, roc4a)

# Conclusion: 4a biggest AUC. comparing 2b with 3a we get no significant difference.

##### 5 (c) Opitmal threshold #####
# Also finding optimal threshold for 2b and 3a
limit2b <- 0.607 # this value was found by trial and error (manually)
rocDf2b[rocDf2b$sensitivity > limit2b & 
          rocDf2b$specificity > limit2b, ]

maxIndex2b <- which(rocDf2b$sensitivity > limit2b & 
                      rocDf2b$specificity > limit2b)
threshold2b <- rocDf2b$threshold[maxIndex2b]

limit3a <- 0.656 # this value was found by trial and error (manually)
rocDf3a[rocDf3a$sensitivity > limit3a & 
          rocDf3a$specificity > limit3a, ]

maxIndex3a <- which(rocDf3a$sensitivity > limit3a & 
                      rocDf3a$specificity > limit3a)
threshold3a <- rocDf3a$threshold[maxIndex3a]

predPhat$yhatOpt2b <- as.numeric(predPhat$phat2b > threshold2b)
predPhat$yhatOpt3a <- as.numeric(predPhat$phat3a > threshold3a)
predPhat$yhatOpt4a <- as.numeric(predPhat$phat4a > threshold4a)

(col012bOpt <- table(predPhat$yhatOpt2b))
(confusionOpt2b <- table(predPhat$lowrain, predPhat$yhatOpt2b))
(specOpt2b <- confusionOpt2b[1, 1] / row01[1])
(sensOpt2b <- confusionOpt2b[2, 2] / row01[2])
(accuOpt2b <- sum(diag(confusionOpt2b)) / sum(confusionOpt2b))
(precOpt2b <- confusionOpt2b[2, 2] / col012bOpt[2])

(col013aOpt <- table(predPhat$yhatOpt3a))
(confusionOpt3a <- table(predPhat$lowrain, predPhat$yhatOpt3a))
(specOpt3a <- confusionOpt3a[1, 1] / row01[1])
(sensOpt3a <- confusionOpt3a[2, 2] / row01[2])
(accuOpt3a <- sum(diag(confusionOpt3a)) / sum(confusionOpt3a))
(precOpt3a <- confusionOpt3a[2, 2] / col013aOpt[2])

(col014aOpt <- table(predPhat$yhatOpt4a))
(confusionOpt4a <- table(predPhat$lowrain, predPhat$yhatOpt4a))
(specOpt4a <- confusionOpt4a[1, 1] / row01[1])
(sensOpt4a <- confusionOpt4a[2, 2] / row01[2])
(accuOpt4a <- sum(diag(confusionOpt4a)) / sum(confusionOpt4a))
(precOpt4a <- confusionOpt4a[2, 2] / col014aOpt[2])

(collectConfusionOpt <- data.frame(
  model = c("model2b", "model3a", "model4a"),
  Specificity = c(specOpt2b, specOpt3a, specOpt4a),
  Sensitivity = c(sensOpt2b, sensOpt3a, sensOpt4a),
  Accuracy = c(accuOpt2b, accuOpt3a, accuOpt4a),
  Precision = c(precOpt2b, precOpt3a, precOpt4a)))

# Conslusion: Lower spec but higher sens, also lower accu and prec.

##### 5 (d) Hosmer-Lemeshow-test #####
# Hosmer-Lemeshow-test
#plot in sorted p-order
# order(variable) gives the ranks for the values in variable.
# It can then be used to sort the data frame:
predSort <- predPhat[order(predPhat$phat2b), ] # Sorted for 2b, used for HL by hand

predSort$rank <- seq(1, nrow(predSort))
head(predSort)

# Divide the n=1091 observations into g=10 groups:
n <- nrow(predSort)
g <- 10
# with ng =  observations each:
ng <- n/g

# Plot p_i and Y_i for model2b
# Add i vertical jitter to Y_i to separate them
ggplot(predSort, aes(rank, phat2b)) +
  geom_point() +
  geom_jitter(aes(y = lowrain), height = 0.01) +
  geom_vline(xintercept = seq(ng, nrow(predSort) - ng, ng)) +
  labs(title = "Estimated probabilities by increasing size",
       x = "(i) = 1,...,n", y = "p-hat") +
  theme(text = element_text(size = 14))

# HL by hand: illustration of the steps 
# (the following can be done using the output of the hoslem.test function)

# A for-loop to set the group numbers:
predSort$group <- NA
for (k in seq(1, g)) {
  I <- (k - 1)*ng + seq(1, ng)
  predSort$group[I] <- k
}
head(predSort)

# Calculate Observed and Expected in each group:
# aggregate(y ~ x, FUN = mean) calculates the mean of y
# separately for each group.
# merge(data1, data2) joins two data frames using any common
# variables as keys, in this case, "group".

# Number of successes:
(OE1 <- merge(aggregate(lowrain ~ group, data = predSort, FUN = sum),
              aggregate(phat2b ~ group, data = predSort, FUN = sum)))

# Number of failures = n_g - successes:
OE0 <- OE1
OE0$lowrain <- ng - OE1$lowrain
OE0$phat2b <- ng - OE1$phat2b
# A variable to cure for color coding:
OE1$outcome <- "O1k, E1k"
OE0$outcome <- "O0k, E0k"
# Bind the two data sets as rows (r):
(OE <- rbind(OE1, OE0))

# And plot:
# Set the tickmarks on the x-axis to integers 1,...,g
ggplot(OE, aes(group, phat2b, color = outcome)) +
  geom_line(size = 1) +
  geom_line(aes(y = lowrain), linetype = "dashed", size = 1) +
  labs(title = "Model 2b: Observed and expected in each group",
       caption = "solid = expected, dashed = observed",
       y = "number of observations") +
  theme(text = element_text(size = 14)) +
  scale_x_continuous(breaks = seq(1, g))

# The test:
(chi2HL <- sum((OE$lowrain - OE$phat2b)^2/OE$phat2b))
# chi2-quantile to compare with:
qchisq(1 - 0.05, g - 2)
# or P-value:
pchisq(chi2HL, g - 2, lower.tail = FALSE)

# HL for the models using built in function hoslem.test
length(model2b$coefficients)

# so we need g > 2 since p = 1 while the smallest expected value in each group should be
# at least approx 5. Manually found the best g = 7
(HL2b <- hoslem.test(predSort$lowrain, predSort$phat2b, g = 9))
HL2b$expected

# Collect the data in a useful form for plotting:
(HLDf2b <- data.frame(group = seq(1, 9),
                       Obs0 = HL2b$observed[, 1],
                       Obs1 = HL2b$observed[, 2],
                       Exp0 = HL2b$expected[, 1],
                       Exp1 = HL2b$expected[, 2]))

ggplot(HLDf2b, aes(group, Obs0)) +
  geom_line(linetype = "dashed", color = "red", size = 1) +
  geom_line(aes(y = Obs1), linetype = "dashed", size = 1) +
  geom_line(aes(y = Exp0), color = "red", size = 1) +
  geom_line(aes(y = Exp1), size = 1) +
  labs(title = "Observed and expected in each group",
       y = "number of observations") +
  scale_x_continuous(breaks = seq(1, 11)) +
  theme(text = element_text(size = 14))

# caption = "solid = expected, dashed = observed, red = 0, black = 1",


# HL for model3a
length(model3a$coefficients)

(HL3a <- hoslem.test(predSort$lowrain, predSort$phat3a, g = 9))
HL3a$expected

# Collect the data in a useful form for plotting:
(HLDf3a <- data.frame(group = seq(1, 9),
                      Obs0 = HL3a$observed[, 1],
                      Obs1 = HL3a$observed[, 2],
                      Exp0 = HL3a$expected[, 1],
                      Exp1 = HL3a$expected[, 2]))

ggplot(HLDf3a, aes(group, Obs0)) +
  geom_line(linetype = "dashed", color = "red", size = 1) +
  geom_line(aes(y = Obs1), linetype = "dashed", size = 1) +
  geom_line(aes(y = Exp0), color = "red", size = 1) +
  geom_line(aes(y = Exp1), size = 1) +
  labs(title = "Observed and expected in each group",
       y = "number of observations") +
  scale_x_continuous(breaks = seq(1, 11)) +
  theme(text = element_text(size = 14))

# caption = "solid = expected, dashed = observed, red = 0, black = 1",


# HL for model4a
length(model4a$coefficients)

k = 9

# Starting at p + 2 = 7
(HL4a <- hoslem.test(predSort$lowrain, predSort$phat4a, g = k)) # 7 is the best choise
HL4a$expected

# Collect the data in a useful form for plotting:
(HLDf4a <- data.frame(group = seq(1, k),
                      Obs0 = HL4a$observed[, 1],
                      Obs1 = HL4a$observed[, 2],
                      Exp0 = HL4a$expected[, 1],
                      Exp1 = HL4a$expected[, 2]))

ggplot(HLDf4a, aes(group, Obs0)) +
  geom_line(linetype = "dashed", color = "red", size = 1) +
  geom_line(aes(y = Obs1), linetype = "dashed", size = 1) +
  geom_line(aes(y = Exp0), color = "red", size = 1) +
  geom_line(aes(y = Exp1), size = 1) +
  labs(title = "Observed and expected in each group",
       y = "number of observations") +
  scale_x_continuous(breaks = seq(1, 11)) +
  theme(text = element_text(size = 14))

# caption = "solid = expected, dashed = observed, red = 0, black = 1",

