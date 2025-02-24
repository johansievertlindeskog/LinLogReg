##### Project 3 #####

# Initializing
rm(list = ls())
load("~/R/LinLogReg/Data/fhm_data.RData")

#install.packages("MASS")
library("MASS")
library(ggplot2)

# Defining functions:

# Simulates bootstrap prediction intervals for Poisson regression.

# Parameters;
# model = the fitted model from the glm()
# newdata = data frame x0 to predict on,
# N = number of bootstrap sample
# p = confidence level of the interval

boot.pois <- function(model, newdata, N, p) {
  odata <- model$data # odata är datan vi drar stickprov ifrån
  lp <- (1 - p) / 2
  up <- 1 - lp
  # comment out the following line:
  set.seed(2016)
  for (i in seq(1, N)) {
    bdata <- odata[sample(seq(nrow(odata)), size = nrow(odata), replace = TRUE), ]
    bpred <- predict(update(model, data = bdata), type = "response", newdata = newdata)
    new_y <- rpois(length(bpred), lambda = bpred)
    if (i == 1) {
      boot_y <- new_y
    } else {
      boot_y <- rbind(boot_y, new_y)
    }
  }
  boot_ci <- t(apply(boot_y, 2, quantile, c(lp, up)))
  return(data.frame(pred = predict(model, newdata = newdata, 
                                   type = "response"), 
                    lower = boot_ci[, 1], 
                    upper = boot_ci[, 2]))
}

# Simulates bootstrap prediction intervals for Negative binomial regression.

# Parameters;
# model = the fitted model from the glm()
# odata = the data that was used to fit the model.
# (since there is no model$data in glm.nb())
# newdata = data frame x0 to predict on,
# N = number of bootstrap sample
# p = confidence level of the interval

boot.nb <- function(model, odata, newdata, N, p) {
  lp <- (1 - p) / 2
  up <- 1 - lp
  # comment out the following line:
  set.seed(2016)
  for (i in seq(1, N)) {
    bdata <- odata[sample(seq(nrow(odata)), size = nrow(odata), replace = TRUE), ]
    bmodel <- update(model, data = bdata)
    bpred <- predict(bmodel, type = "response", newdata = newdata)
    new_y <- rnegbin(length(bpred), mu = bpred, theta = bmodel$theta)
    if (i == 1) {
      boot_y <- new_y
    } else {
      boot_y <- rbind(boot_y, new_y)
    }
  }
  boot_ci <- t(apply(boot_y, 2, quantile, c(lp, up)))
  return(data.frame(pred = predict(model, newdata = newdata, 
                                   type = "response"), 
                    lower = boot_ci[, 1], 
                    upper = boot_ci[, 2]))
}

##### Part 1- Stockholm Poisson #####

# Extracting data for Stockholm only
indexStockholm <- fhm.data$region == "Stockholm"
covidStockholm <- fhm.data[indexStockholm, ]
head(covidStockholm)

(
  plotSthlmCasesVsDays <- ggplot(data = covidStockholm, 
                                  aes(x = day_nbr_region, y = new_cases)) + 
    geom_smooth(method = loess) + # moving average
    geom_point(size = 1) +
    xlab("Days (1 = 25/2-20)") +
    ylab("New cases") +
    labs(title = "Stockholm: Number of new cases vs days",
         caption = "with moving average and 95% confidence interval") +
    theme(text = element_text(size = 18))
)

# Removing data with decrease in new cases (starts at day 60)
indexExc <- which(covidStockholm$day_nbr_region < 60)
covidStockholmExc <- covidStockholm[indexExc, ]

# Plot new cases vs days
(
  plotSthlmCasesVsDaysExc <- ggplot(data = covidStockholmExc, 
                         aes(x = day_nbr_region, y = new_cases)) + 
    geom_smooth(method = loess) + # moving average
    geom_point(size = 1) +
    xlab("Days (1 = 25/2-20)") +
    ylab("New cases") +
    labs(title = "Stockholm: Number of new cases vs days",
         caption = "with moving average and 95% confidence interval") +
    theme(text = element_text(size = 18))
)

##### Fit with intercept and days #####
# Fit null poisson regression model (with intercept only)
modelP0 <- glm(new_cases ~ 1, offset(log(population)), family = "poisson", data = covidStockholmExc)
summary(modelP0)

# Beta estimates and CI for log-odds
exp(modelP0$coefficients)
exp(confint(modelP0))

# Fit poisson regression model with days 
modelPdays <- glm(new_cases ~ day_nbr_region, offset(log(population)), family = "poisson",
                  data = covidStockholmExc)
summary(modelPdays)

# Beta estimates and CI log expected
cbind(exp(modelPdays$coefficients),
      exp(confint(modelPdays)))

# Use deviance to construct a LR-test. If D_diff bigger than
# chisq quantile then reject H0 at sig. level alpha since the diff is chisq distributed.
# Compare modelP0 and modelPdays using anova
(anovaModelP0days <- anova(modelP0, modelPdays))
(D_diff <- anovaModelP0days$Deviance[2]) # Diff in deviance
(f_diff <- anovaModelP0days$Df[2]) # Diff in deg. of freedom 

#chi2-quantile to compare D_diff with
qchisq(1 - 0.05, f_diff)
# or P-value:
pchisq(D_diff, f_diff, lower.tail = FALSE)

# Conclusion: D_diff bigger than quantile so modelPdays significantly better than modelP0.

##### Estimated mu with CI using xbeta ##### 
predSthlmP <- cbind(covidStockholmExc,
                    muhat = predict(modelPdays, type = "response"),
                    xb = predict(modelPdays, se.fit = TRUE))
predSthlmP$xb.residual.scale <- NULL

predSthlmP$xb.lwr <- predSthlmP$xb.fit - 1.96*predSthlmP$xb.se.fit
predSthlmP$xb.upr <- predSthlmP$xb.fit + 1.96*predSthlmP$xb.se.fit
predSthlmP$mu.lwr <- exp(predSthlmP$xb.lwr)
predSthlmP$mu.upr <- exp(predSthlmP$xb.upr)
head(predSthlmP)

# Calculating bootstrap prediction interval
bootPredintP <- boot.pois(modelPdays, covidStockholmExc, 1000, 0.95)

predSthlmP$bootpred.lwr <- bootPredintP$lower
predSthlmP$bootpred.upr <- bootPredintP$upper

# Plot of expected number of new cases (mu) with bootstrap prediction interval
ggplot(predSthlmP, aes(day_nbr_region, new_cases)) +
  geom_jitter(height = 0.1, width = 0) +
  geom_line(aes(y = muhat), size = 1) +
  geom_ribbon(aes(ymin = mu.lwr, ymax = mu.upr), alpha = 0.9, color = 'green') +
  geom_ribbon(aes(ymin = bootpred.lwr, ymax = bootpred.upr), alpha = 0.1, color = 'red') +
  xlab("Days (1 = 25/2-20)") +
  ylab("New cases") +
  labs(title = "Stockholm: Expected number of new cases",
       caption = "expected value with 95% CI (green) and bootstrap prediction interval (red)") +
  theme(text = element_text(size = 16))

##### Influence measures #####
inflSthlmPdays <- influence(modelPdays)
predSthlmP$v <- inflSthlmPdays$hat
predSthlmP$devres <- inflSthlmPdays$dev.res

predSthlmP$std.devres <- predSthlmP$devres/sqrt(1 - predSthlmP$v)
predSthlmP$D <- cooks.distance(modelPdays)

# leverage vs day_nbr_region
(plotSthlmLevVsDaysP <- ggplot(predSthlmP, aes(day_nbr_region, v)) +
  geom_point(size = 2) +
  geom_hline(yintercept = c(I(1/nrow(covidStockholmExc)))) +
  geom_hline(yintercept = 2*(length(modelPdays$coefficients))/nrow(covidStockholmExc),
             color = "red", size = 1) +
  xlab("Days (1 = 25/2-20)") +
  ylab("Leverage") +
  labs(title = "Stockholm: Leverage vs days",
       caption = "horizontal lines at 1/n (black) and 2(p+1)/n (red)") +
  theme(text = element_text(size = 16))
)

# Find all leverage above  2*(length(modelPdays$coefficients))/nrow(covidStockholmExc) = 0.0687
outliersLeverageP <- which(predSthlmP$v >
                             2*(length(modelPdays$coefficients))/nrow(covidStockholmExc))

# ... and highlight the outliers
(plotSthlmLevVsDaysPOutliers <- plotSthlmLevVsDaysP +
    geom_point(data = predSthlmP[outliersLeverageP, ], size = 3, 
               color = "red", shape = 24)
)

# standardized deviance residuals vs day_nbr_region
(plotSthlmResVsDaysP <- ggplot(predSthlmP, aes(day_nbr_region, std.devres)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-2, 2), color = "red",
             linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-4, 4), color = "red",
             linetype = "dotted", size = 1) +
  xlab("Days (1 = 25/2-20)") +
  ylab("Standardized deviance residuals") +
  labs(title = "Stockholm: Standardized deviance residuals vs days",
       caption = "horizontal lines at +- 2 and +- 4") +
  theme(text = element_text(size = 16))
)

outliersResP <- which(abs(predSthlmP$std.devres) > 4)

# ... and highlight the outliers
(plotSthlmResVsDaysPOutliers <- plotSthlmResVsDaysP +
    geom_point(data = predSthlmP[outliersResP, ], size = 3, 
               color = "red", shape = 24)
)

# Cook's D
plotSthlmCooksDVsDaysP <- ggplot(predSthlmP, aes(day_nbr_region, D)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 4/nrow(covidStockholmExc), color = "red") +
  xlab("Days (1 = 25/2-20)") +
  ylab("Cook's D") +
  labs(title = "Stockholm: Cook's D vs days", caption = "horizontal line at 4/n") +
  theme(text = element_text(size = 16))

outliersDP <- which(predSthlmP$D > 4/nrow(covidStockholmExc))

(plotSthlmCooksDVsDaysPOutliers <- plotSthlmCooksDVsDaysP +
    geom_point(data = predSthlmP[outliersDP, ], size = 3, 
               color = "red", shape = 24)
)

# Conclusion: Some obs with high leverage and almost all obs with unreasonable high 
# residuals and Cook'd D which proves that the data is NOT poisson distributed (expected).

##### Part 2- Stockholm Negative binomial #####

##### Fit with intercept and days #####
# Fit null negative binomial regression model (with intercept only)
modelNB0 <- glm.nb(new_cases ~ 1, offset(log(population)), data = covidStockholmExc)
summary(modelNB0)

# Beta estimates and CI
exp(modelNB0$coefficients)
exp(confint(modelNB0))

# Fit negative binomial regression model with days 
modelNBdays <- glm.nb(new_cases ~ day_nbr_region, offset(log(population)), data = covidStockholmExc)
summary(modelNBdays)

# Beta estimates and CI
cbind(exp(modelNBdays$coefficients),
      exp(confint(modelNBdays)))

# LR-test comparing modelNB0 and modelNBdays
(anovaModelNB0days <- anova(modelNB0, modelNBdays))
(D_diff <- anovaModelNB0days$`LR stat.`[2]) # Diff in deviance
(f_diff <- anovaModelNB0days$`   df`[2]) # Diff in deg. of freedom 

#chi2-quantile to compare D_diff with
qchisq(1 - 0.05, f_diff)
# or P-value:
pchisq(D_diff, f_diff, lower.tail = FALSE)

# Conclusion: D_diff bigger than quantile so modelNBdays significantly better than modelNB0.

# LR-test comparing POISSON and NEGBIN model (cannot use anova since requires same distribution of models)
D_diff <- -2*(logLik(modelPdays)[1] - logLik(modelNBdays)[1])
f_diff <- 1 # NEGBIN has one extra theta parameter which counts in the deg. of freedom in this test

#chi2-quantile to compare D_diff with
qchisq(1 - 0.05, f_diff)
# or P-value:
pchisq(D_diff, f_diff, lower.tail = FALSE)

# Conclusion: D_diff bigger than quantile => modelNBdays significantly better than modelPdays

##### Estimated mu with CI using xbeta #####
predSthlmNB <- cbind(covidStockholmExc,
                    muhat = predict(modelNBdays, type = "response"),
                    xb = predict(modelNBdays, se.fit = TRUE))
predSthlmNB$xb.residual.scale <- NULL

predSthlmNB$xb.lwr <- predSthlmNB$xb.fit - 1.96*predSthlmNB$xb.se.fit
predSthlmNB$xb.upr <- predSthlmNB$xb.fit + 1.96*predSthlmNB$xb.se.fit
predSthlmNB$mu.lwr <- exp(predSthlmNB$xb.lwr)
predSthlmNB$mu.upr <- exp(predSthlmNB$xb.upr)
head(predSthlmNB)

# Calculating bootstrap prediction interval
bootPredintNB <- boot.nb(modelNBdays, covidStockholmExc, covidStockholmExc, 1000, 0.95)

predSthlmNB$bootpred.lwr <- bootPredintNB$lower
predSthlmNB$bootpred.upr <- bootPredintNB$upper

# Plot of expected number of new cases (mu) with bootstrap prediction interval
ggplot(predSthlmNB, aes(day_nbr_region, new_cases)) +
  geom_jitter(height = 0.1, width = 0) +
  geom_line(aes(y = muhat), size = 1) +
  geom_ribbon(aes(ymin = mu.lwr, ymax = mu.upr), alpha = 0.1, color = 'green') +
  geom_ribbon(aes(ymin = bootpred.lwr, ymax = bootpred.upr), alpha = 0.1, color = 'red') +
  xlab("Days (1 = 25/2-20)") +
  ylab("New cases") +
  labs(title = "Stockholm: Expected number of new cases",
       caption = "expected value with 95% CI (green) and bootstrap prediction interval (red)") +
  theme(text = element_text(size = 16))

##### Influence measures #####
inflSthlmNBdays <- influence(modelNBdays)
predSthlmNB$v <- inflSthlmNBdays$hat
predSthlmNB$devres <- inflSthlmNBdays$dev.res

predSthlmNB$std.devres <- predSthlmNB$devres/sqrt(1 - predSthlmNB$v)
predSthlmNB$D <- cooks.distance(modelNBdays)

# leverage vs day_nbr_region
(plotSthlmLevVsDaysNB <- ggplot(predSthlmNB, aes(day_nbr_region, v)) +
    geom_point(size = 2) +
    geom_hline(yintercept = c(I(1/nrow(covidStockholmExc)))) +
    geom_hline(yintercept = 2*(length(modelNBdays$coefficients))/nrow(covidStockholmExc),
               color = "red", size = 1) +
    xlab("Days (1 = 25/2-20)") +
    ylab("Leverage") +
    labs(title = "Stockholm: Leverage vs days",
         caption = "horizontal lines at 1/n (black) and 2(p+1)/n (red)") +
    theme(text = element_text(size = 14))
)

# standardized deviance residuals vs day_nbr_region
(plotSthlmResVsDaysNB <- ggplot(predSthlmNB, aes(day_nbr_region, std.devres)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-2, 2), color = "red",
             linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-4, 4), color = "red",
             linetype = "dotted", size = 1) +
  xlab("Days (1 = 25/2-20)") +
  ylab("Standardized deviance residuals") +
  labs(title = "Stockholm: Standardized deviance residuals vs days",
       caption = "horizontal lines at +- 2 and +- 4") +
  theme(text = element_text(size = 14))
)

outliersResNB <- which(abs(predSthlmNB$std.devres) > 4)

# remove only the first week (containg the biggest residuals)
someOutliersResNB <- which(predSthlmNB$std.devres < -7)

# ... and highlight the outliers
(plotSthlmResVsDaysNBOutliers <- plotSthlmResVsDaysNB +
    geom_point(data = predSthlmNB[outliersResNB, ], size = 3, 
               color = "red", shape = 24) +
    geom_point(data = predSthlmNB[someOutliersResNB, ], size = 3, 
               color = "blue", shape = 24)
)

# Cook's D
(plotSthlmCooksDVsDaysNB <- ggplot(predSthlmNB, aes(day_nbr_region, D)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 4/nrow(covidStockholmExc), color = "red") +
  xlab("Days (1 = 25/2-20)") +
  ylab("Cook's D") +
  labs(title = "Stockholm: Cook's D vs days", caption = "horizontal line at 4/n") +
  theme(text = element_text(size = 18))
)

outliersDNB <- which(predSthlmNB$D > 4/nrow(covidStockholmExc))

(plotSthlmCooksDVsDaysNBOutliers <- plotSthlmCooksDVsDaysNB +
    geom_point(data = predSthlmNB[outliersDNB, ], size = 3, 
               color = "red", shape = 24) +
    geom_point(data = predSthlmNB[someOutliersResNB, ], size = 3, 
               color = "blue", shape = 24)
)

# Conclusion: No outliers from leverage. Some alarmingly large standardized deviance residual,
# only the first ones will be removed. A lot of high values from from Cook's D but nothing to 
# do about it except the one with the highest Cook's D and the ones from high residuals (blue).
# Still much better compared to Poisson, note the scale difference!

##### Part 3- Removing outliers, re-fitting and comparing #####
# Remove the outliers from leverage and the one with the highest Cook's D
outlierHighestDNB <- which(predSthlmNB$D > 1.25)

indexOutliers <- c(someOutliersResNB, outlierHighestDNB)
covidStockholmExc2 <- covidStockholmExc[-indexOutliers, ]

# Fit POISSON with days using REDUCED data set
modelPdaysExc <- glm(new_cases ~ day_nbr_region, offset(log(population)), family = "poisson",
                  data = covidStockholmExc2)
summary(modelPdaysExc)

# Beta estimates and CI log expected
cbind(exp(modelPdaysExc$coefficients),
      exp(confint(modelPdaysExc)))

# Fit NEGBIN with days using REDUCED data set
modelNBdaysExc <- glm.nb(new_cases ~ day_nbr_region, offset(log(population)),
                         data = covidStockholmExc2)
summary(modelNBdaysExc)

# Beta estimates and CI
cbind(exp(modelNBdaysExc$coefficients),
      exp(confint(modelNBdaysExc)))

##### Estimated mu with confidence interval using xbeta ##### 
predSthlmNBExc <- cbind(covidStockholmExc2,
                     muhat = predict(modelNBdaysExc, type = "response"),
                     xb = predict(modelNBdaysExc, se.fit = TRUE))
predSthlmNBExc$xb.residual.scale <- NULL

# Plot of expected number of new cases (mu) with bootstrap prediction interval
predSthlmNBExc$xb.lwr <- predSthlmNBExc$xb.fit - 1.96*predSthlmNBExc$xb.se.fit
predSthlmNBExc$xb.upr <- predSthlmNBExc$xb.fit + 1.96*predSthlmNBExc$xb.se.fit
predSthlmNBExc$mu.lwr <- exp(predSthlmNBExc$xb.lwr)
predSthlmNBExc$mu.upr <- exp(predSthlmNBExc$xb.upr)
head(predSthlmNBExc)

# Calculating bootstrap prediction interval
bootPredintNBExc <- boot.nb(modelNBdaysExc, covidStockholmExc2, covidStockholmExc2, 1000, 0.95)

predSthlmNBExc$bootpred.lwr <- bootPredintNBExc$lower
predSthlmNBExc$bootpred.upr <- bootPredintNBExc$upper

ggplot(predSthlmNBExc, aes(day_nbr_region, new_cases)) +
  geom_jitter(height = 0.1, width = 0) +
  geom_line(aes(y = muhat), size = 1) +
  geom_ribbon(aes(ymin = mu.lwr, ymax = mu.upr), alpha = 0.1, color = 'green') +
  geom_ribbon(aes(ymin = bootpred.lwr, ymax = bootpred.upr), alpha = 0.1, color = 'red') +
  xlab("Days (1 = 25/2-20)") +
  ylab("New cases") +
  labs(title = "Stockholm: Expected number of new cases",
       caption = "expected value with 95% CI (green) and bootstrap prediction interval (red)") +
  theme(text = element_text(size = 16))

# Note that the first data point is at day 8 (3/3-20)

##### TEST CHECKING INFLUENCE #####

inflSthlmNBdaysExc <- influence(modelNBdaysExc)
predSthlmNBExc$v <- inflSthlmNBdaysExc$hat
predSthlmNBExc$devres <- inflSthlmNBdaysExc$dev.res

predSthlmNBExc$std.devres <- predSthlmNBExc$devres/sqrt(1 - predSthlmNBExc$v)
predSthlmNBExc$D <- cooks.distance(modelNBdaysExc)

# leverage vs day_nbr_region
(ggplot(predSthlmNBExc, aes(day_nbr_region, v)) +
   geom_point(size = 2) +
   geom_hline(yintercept = c(I(1/nrow(covidStockholmExc2)))) +
   geom_hline(yintercept = 2*(length(modelNBdaysExc$coefficients))/nrow(covidStockholmExc2),
              color = "red", size = 1) +
   xlab("Days (1 = 25/2-20)") +
   ylab("Leverage") +
   labs(title = "Stockholm: Leverage vs days",
        caption = "horizontal lines at 1/n (black) and 2(p+1)/n (red)") +
   theme(text = element_text(size = 14))
)

# standardized deviance residuals vs day_nbr_region
(ggplot(predSthlmNBExc, aes(day_nbr_region, std.devres)) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
    geom_hline(yintercept = c(-2, 2), color = "red",
               linetype = "dashed", size = 1) +
    geom_hline(yintercept = c(-4, 4), color = "red",
               linetype = "dotted", size = 1) +
    xlab("Days (1 = 25/2-20)") +
    ylab("Standardized deviance residuals") +
    labs(title = "Stockholm: Standardized deviance residuals vs days",
         caption = "horizontal lines at +- 2 and +- 4") +
    theme(text = element_text(size = 14))
)

# Cook's D
(ggplot(predSthlmNBExc, aes(day_nbr_region, D)) +
    geom_point(size = 2) +
    geom_hline(yintercept = 4/nrow(covidStockholmExc2), color = "red") +
    xlab("Days (1 = 25/2-20)") +
    ylab("Cook's D") +
    labs(title = "Stockholm: Cook's D vs days", caption = "horizontal line at 4/n") +
    theme(text = element_text(size = 18))
)

# Conclusion: No major improvement comparing residuals and Cook's D using the reduced data set.

# NOTE: If we want to compare 2 different models, they have to use the SAME data set. This is why
# we also re-fitted the Poisson model with the reduced data set. We are NOT allowed to compare e.g.
# the old NegBin model and the REDUCED NegBin model since the deviance is containing the likelihood
# function which is containing probabilities for every data point etc.

# LR-test comparing REDUCED Poisson and REDUCED NegBin model
D_diff <- -2*(logLik(modelPdaysExc)[1] - logLik(modelNBdaysExc)[1])
f_diff <- 1 # NEGBIN has one extra theta parameter which counts in the deg. of freedom in this test

#chi2-quantile to compare D_diff with
qchisq(1 - 0.05, f_diff)
# or P-value:
pchisq(D_diff, f_diff, lower.tail = FALSE)

# Conclusion: D_diff is less compared to before when NOT using the reduced data set. However, this
# does not mean that we should go back to our old model. See visual results instead.
