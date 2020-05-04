##### Lab 2 #####

rm(list = ls())
load("Data/Pb_mossa.rda")
#summary(Pb_mossa)

##### 4 (a) #####
# Re-fit same model from 2 (c) i.e. log-linear regression model
loglinModel <- lm(log(Pb) ~ I(year - 1975), data = Pb_mossa)
modelSummary <- summary(loglinModel)

# Beta estimates and CI
betaEstimates <- loglinModel$coefficients
confInt <- confint(loglinModel)

# Standard errors - estimation of standard deviation
# seB0 <- modelSummary$coefficients[1,2]
# seB1 <- modelSummary$coefficients[2,2]

# ConfInt does not cover zero => parameters statistically significant.
# p-value = 2.2*10^-16 < 0.05. Always see p-value in summary.

##### 5 (a) #####
library(ggplot2)

# Plotting the data with respect to location in 1 plot
  ggplot(data = Pb_mossa, 
                    aes(x = I(year - 1975), y = Pb, color = loc)) +
  #geom_point(size = 1) +
  geom_smooth(se = FALSE) + # use geom_smooth instead of geom_point for average value in a line
  xlab("Year (0 = 1975)") +
  ylab("Concentration (mg Pb/Kg moss)") +
  labs(title = "Pb concentration vs year") +
  theme(text = element_text(size = 16))

##### 5 (b) #####
# Plotting the data for each location (add color = loc in aes)
  (
    plotLocExp <- ggplot(data = Pb_mossa, 
         aes(x = I(year - 1975), y = Pb, color = loc)) +
    geom_jitter(width = 1) +            # used genom_jitter instead of geom_point here
    facet_wrap(~ loc) +
    xlab("Year (0 = 1975)") +
    ylab("Concentration (mg Pb/Kg moss)") +
    labs(title = "Pb concentration vs year") +
    theme(text = element_text(size = 16))
   )

  # Conclusion: looks like exponential decay for all locations

##### 5 (c) #####
  # Plotting the data for each location BUT WITH LOG DATA (add color = loc in aes)
  (
    plotLocLog <- ggplot(data = Pb_mossa, 
         aes(x = I(year - 1975), y = log(Pb), color = loc)) +
    facet_wrap(~ loc) +
    geom_point(size = 1) +
    xlab("Year (0 = 1975)") +
    ylab("Concentration (ln(mg Pb/Kg moss))") +
    labs(title = "Pb concentration vs year") +
    theme(text = element_text(size = 16))
    )
  
  # Conclusion: looks like linear as expected, works fine for all locations. 

##### 5 (d) #####
# Fit linear MULTIPLE regression model
lm(log(Pb) ~ I(year - 1975) + loc, data = Pb_mossa)

# Conclusion: Orebro used as ref, not good since few data points!

##### 5 (e) #####
# Norrbotten har most observations => use as ref!

# change of reference location
Pb_mossa$loc <- relevel(Pb_mossa$loc, "Norrbotten")
  
  # Plotting the data for each location
  ggplot(data = Pb_mossa, 
         aes(x = I(year-1975), y = Pb, color = loc)) +
    facet_wrap(~ loc) +
    geom_point(size = 1) +
    xlab("Year (0 = 1975)") +
    ylab("Concentration (mg Pb/Kg moss)") +
    labs(title = "Pb concentration vs year") +
    theme(text = element_text(size = 16))

  # Plotting the data for each location BUT WITH THE LOG DATA (add color = loc in aes)
  ggplot(data = Pb_mossa, 
         aes(x = I(year-1975), y = log(Pb), color = loc)) +
    facet_wrap(~ loc) +
    geom_point(size = 1) +
    xlab("Year (0 = 1975)") +
    ylab("Concentration (ln(mg Pb/Kg moss))") +
    labs(title = "Pb concentration vs year") +
    theme(text = element_text(size = 16))

# Re-fit log-linear multiple regression model with correct ref
multiModel <- lm(log(Pb) ~ I(year - 1975) + loc, data = Pb_mossa)

##### 5 (f) #####
multiModelSummary <- summary(multiModel)

# Beta estimates and CI (better way for displaying estimates and CI)
cbind(exp(multiModel$coefficients),
      exp(confint(multiModel)))

# Beta (B0 = 2.91 , B1 = -0.0811) and CI
betaEstimates <- multiModel$coefficients
confIntMultiModel <- confint(multiModel)

# e^(beta estimates) and CI for orginal scale
expEstimates <- exp(betaEstimates)
confIntMultiModelExp <- exp(confInt)

# Do NOT report standars errors for e^betaEstimates, no need and makes no sense. 

##### 5 (g) #####
# Average concentration of lead in Norrbotten in 1975, according to the model is a*b^0 = a = 18.27.
# Rate of decrease = 1 - b = 7.8%

##### 5 (h) #####
# Estimating the expected log(Pb)-level in Orebro in 1975 using model (since Orebro
# does not have any observations for 1975), together with a 95% CI.
# Same for expected Pb-level (no log).

x0 <- data.frame(year = 1975, loc = "Orebro")
y0 <- predict(multiModel, x0, interval = "confidence")
cbind(x0, logPbConf = y0, PbConf = exp(y0))

# Average level of log(Pb) = 3.6 with conf [3.5 3.7] 
# and Pb = 38 with conf [34 42]

##### 5 (i) #####
# For any given year, the ratio between the expected Pb-levels in each of the other
# locations, and the level in Norrbotten is the e^(beta estimates), i.e. c,d,e and f.
# J?mtland (i.e. d) has lowest levels on average. 

##### 6 (a) #####
# Yes, there is a significant relationship between log(Pb) and time when using multiModel, i.e.
# log(Pb) vs time using different locations. This is confirmed by the confidence interval for
# B1 [-0.084 -0.079] not cotaining 0. p-value found in multiModelSummary, p = 2.2*10^-16 < 0.05 
# => we can reject H0: B1 = 0.

# t-test for location####
# The t-test for different location is in:
multiModelSummary$coefficients

# compare t-value with t-quantile:
# upper alpha/2-t-quantile with df = 1225:
(tvalue <- multiModelSummary$coefficients[2, 3])
qt(1 - 0.05/2, 1225)

# abs of tvalue > qt => reject H_0

# calculate P-value:
# 2 * P(|t| > |tvalue|) to cover both tails:
2*pt(abs(tvalue), 1225, lower.tail = FALSE)

# pValue < 0.05 => reject H_0!

multiModelSummary$coefficients[2, 4]

##### 6 (b) #####
# Are there significant differences in the starting levels (1975)
# between the different locations?
# Test this using a (partial) F-test, comparing the model from 2.(c) with 5.(e). Report
# the P-value for the test, and the conclusion.

# Model from 5 e: 
multiModel

# Partial F-test for the locations####
# Fit the reduced model (original loglinmodel) without locations:
modelReduced <- lm(data = Pb_mossa, log(Pb) ~ I(year - 1975))

# Compare the models:
(PbAnova <- anova(modelReduced, multiModel))
# -> p-value 2.2e-16

# Compare the F-value with upper F-quantile:
(Fvalue <- PbAnova$F[2]) # F-value: 250
qf(1 - 0.05, 2, 1225) # Fvalue > 3 (quantile value) so add location => significant difference!

# calculate P-value:
pf(Fvalue, 2, 1225, lower.tail = FALSE)
PbAnova$`Pr(>F)`[2]

# Pbanova < pf so reject H_0

##### 6 (c) #####
#Add the fitted lines, CI and PI to the plots in 5 (c) and in 5 (b)
multiModelPred <- cbind(Pb_mossa,
                        conf = predict(multiModel, interval = "confidence"),
                        pred = predict(multiModel, interval = "prediction"))
head(multiModelPred)

# Plot of the unscaled data with fitted line, CI and PI (plotLocExp saved in 5 (b))
(plotFinalExp <- plotLocExp +
    geom_line(data = multiModelPred,
              aes(y = exp(conf.fit)),
              color = "blue", size = 1) +
    geom_ribbon(data = multiModelPred,
                aes(ymin = exp(conf.lwr), 
                    ymax = exp(conf.upr)),
                alpha = 0.2) +
    geom_line(data = multiModelPred, 
              aes(y = exp(pred.lwr)),
              color = "red", linetype = "dashed",
              size = 1) +
    geom_line(data = multiModelPred, 
              aes(y = exp(pred.upr)),
              color = "red", linetype = "dashed",
              size = 1)
  )

# Plot of the log data with fitted line, CI and PI (plotLocLog saved in 5 (c))
(
  plotFinalLog <- plotLocLog +
    geom_line(data = multiModelPred,
              aes(y = conf.fit),
              color = "blue", size = 1) +
    geom_ribbon(data = multiModelPred,
                aes(ymin = conf.lwr, 
                    ymax = conf.upr),
                alpha = 0.2) +
    geom_line(data = multiModelPred, 
              aes(y = pred.lwr),
              color = "red", linetype = "dashed",
              size = 1) +
    geom_line(data = multiModelPred, 
              aes(y = pred.upr),
              color = "red", linetype = "dashed",
              size = 1)
  )

# VAD ÄR ALPHA?????????????????????????????????

##### 6 (d) #####
### Add the residuals to the predicted data
multiModelPred$e <- multiModel$residuals
head(multiModelPred)

# Save the max-value in order to make the y-axis symmetrical in the plots.
max.e <- max(abs(multiModelPred$e))
PbElimits <- c(-max.e, max.e)

# Plot residuals against yhat
ggplot(data = multiModelPred,
       aes(x = pred.fit, y = e, color = loc)) +
  facet_wrap(~ loc) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0) +
  expand_limits(y = PbElimits) +
  xlab("Predicted ln(Pb) concentration") +
  ylab("Residual") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 18))

# Plot residuals against year
ggplot(data = multiModelPred,
       aes(x = year, y = e, color = loc)) +
  facet_wrap(~ loc) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0) +
  expand_limits(y = PbElimits) +
  xlab("Year") +
  ylab("Residual") +
  labs(title = "Residuals vs year") +
  theme(text = element_text(size = 18))

# Make a normal qq-plot of the residuals (all at once)
ggplot(data = multiModelPred, 
       aes(sample = e)) +
  geom_qq(size = 1) +
  geom_qq_line() +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 18))

# Make a normal qq-plot of the residuals for every location
ggplot(data = multiModelPred, 
       aes(sample = e, color = loc)) +
  facet_wrap(~ loc) +
  geom_qq(size = 1) +
  geom_qq_line() +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 18))

