## R-code for lecture 10, vt20:
# 6/5-20. AL

library(ggplot2)
# Library for negative binomial. glm.nb()
# MASS = Modern Applied Statistics with S
# ("S" is the statistical software "R" is inspired by)
library(MASS)
# The Negbin example data is in Stata (another statistical 
# software) format. The package "foreign" has functions for
# reading, e.g., SAS, Stata, Minitab, Octave, SPSS formats.
# Use it if you like.
# If not, I have make an R version of the data.

# library(foreign)

## Plot some Poisson-distributions:

# dpois(k, m) = Pr(Y = k) when Y is Po(m)
Py <- data.frame(
  y = rep(seq(0, 20), 4),
  mu = c(rep(0.2, 21), rep(1, 21), rep(5, 21), rep(10, 21)),
  label = c(rep("m = 0.2", 21), rep("m = 1", 21),
            rep("m = 5", 21), rep("m = 10", 21)))
Py$prob <- dpois(Py$y, Py$mu)

# geom_col() plots columns of the specified height
# position = "dodge" places the columns side-by-side
ggplot(Py, aes(y, prob, fill = label)) +
  geom_col(position = "dodge") +
  labs(title = "Some Poisson distributions",
       fill = "Mean",
       y = "probability") +
  theme(text = element_text(size = 18))

# Poisson-regression ####

#read the data from a comma separated (csv) file on the net.
award <- read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")
head(award)
summary(award)

# give the programs their correct factor labels, not numbers
award$prog <- factor(
  award$prog, 
  levels = c(1, 2, 3), 
  labels = c("General", "Academic", "Vocational"))

# Make a frequency table of the number of awards:
(award.table <- table(award$num_awards))

(award.df <- data.frame(
  num_awards = as.numeric(names(award.table)),
  award.table))
award.df$prob <- award.df$Freq / sum(award.df$Freq)
award.df$Var1 <- NULL
award.df$model <- "Observed"
award.df

# Calculate the average number of awards
(mu <- mean(award$num_awards))
# and a variance estimates s2 = sum(x_i - mu)^2/(n-1)
var(award$num_awards)

award.df2 <- award.df
award.df2$prob <- dpois(award.df2$num_awards, mu)
award.df2$model <- "Poisson"

award.df <- rbind(award.df, award.df2)

ggplot(award.df, aes(num_awards, prob, fill = model)) +
  geom_col(position = "dodge") +
  labs(title = "Distribution of awards",
       x = "number of awards",
       y = "relative frequency") +
  theme(text = element_text(size = 18))

# by program
table(award$prog, award$num_awards)
aggregate(num_awards ~ prog, data = award, FUN = mean)
aggregate(num_awards ~ prog, data = award, FUN = var)
# large variance in Academic. Need maths?
# variance larger than the mean. Might be explained by covariates?

# Fit a poisson regression model ####
(model.award <- glm(num_awards ~ prog + math, family = "poisson", data = award))
summary(model.award)
cbind(summary(model.award)$coefficients, ci = confint(model.award))
cbind(exp(model.award$coefficients), exp(confint(model.award)))

# differences between programs:
beta <- model.award$coefficients
exp(beta[1] + beta[2])
exp(beta[1] + beta[3])
# compared to the general program and for fixed final math exam, 
# students from the academic program get about 3 times more awards.

# For a fixed study program, a unit increase in the students maths grade
# predicts a 7% increase (from 1 to 1.07) in the number of awards. Or, should 
# the students population get on average an increase of 10 points in the
# maths grade (for fixed program), we would have an increase of awards equal 
# to exp(10*0.07)=2, that is double the awards

# estimated mu with confidence interval using xbeta####
pred.award <- cbind(
  award,
  muhat = predict(model.award, type = "response"),
  xb = predict(model.award, se.fit = TRUE))
pred.award$xb.residual.scale <- NULL

pred.award$xb.lwr <- pred.award$xb.fit - 1.96*pred.award$xb.se.fit
pred.award$xb.upr <- pred.award$xb.fit + 1.96*pred.award$xb.se.fit
pred.award$mu.lwr <- exp(pred.award$xb.lwr)
pred.award$mu.upr <- exp(pred.award$xb.upr)
head(pred.award)

ggplot(pred.award, aes(math, num_awards, color = prog)) +
  geom_jitter(height = 0.1, width = 0) +
  geom_line(aes(y = muhat), size = 1) +
  geom_ribbon(aes(ymin = mu.lwr, ymax = mu.upr), alpha = 0.1) +
  labs(title = "Expected number of awards",
       caption = "95% confidence interval",
       color = "program") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ prog)

# Deviance test against null model:
model.award$null.deviance - model.award$deviance
qchisq(1 - 0.05, model.award$df.null - model.award$df.residual)

# Deviance test against removing prog:
model.math <- update(model.award, . ~ . - prog)
model.math$deviance - model.award$deviance
qchisq(1 - 0.05, model.math$df.residual - model.award$df.residual)

anova(model.math, model.award)
qchisq(1 - 0.05, 2)
pchisq(model.math$deviance - model.award$deviance,
       model.math$df.residual - model.award$df.residual,
       lower.tail = FALSE)

# Influence measures:
infl.award <- influence(model.award)
pred.award$v <- infl.award$hat
pred.award$devres <- infl.award$dev.res
pred.award$std.devres <- pred.award$devres/sqrt(1 - pred.award$v)
pred.award$D <- cooks.distance(model.award)

# leverage####
ggplot(pred.award, aes(math, v, color = prog)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 2*(length(model.award$coefficients))/nrow(award), 
             color = "red") +
  labs(title = "Leverage",
       color = "program", caption = "horizontal line = 2(p+1)/n") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ prog)

# std dev res####
ggplot(pred.award, aes(math, std.devres, color = prog)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-2, 2), color = "red",
             linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-4, 4), color = "red",
             linetype = "dotted", size = 1) +
  labs(title = "Standardized deviance residuals",
       color = "program") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ prog)

# Cook's D####
ggplot(pred.award, aes(math, D, color = prog)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 4/nrow(award), color = "red") +
  labs(title = "Cook's distance",
       color = "program", caption = "horizontal line = 4/n") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ prog)
  
# prediction intervals using bootstrap####
# see poisson_bootstrap_predint.R for generating the data.
load("Data/award_predint.RData")

ggplot(pred.award, aes(math, num_awards, color = prog)) +
  geom_point() +
  geom_line(aes(y = muhat)) +
  geom_ribbon(aes(ymin = mu.lwr, ymax = mu.upr), alpha = 0.1) +
  geom_line(data = x0.award, aes(y = pred.lwr), size = 1) +
  geom_line(data = x0.award, aes(y = pred.upr), size = 1) +
  labs(title = "Expected number of awards",
       caption = "95% conf.int. and 95% bootstrap pred.int.") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ prog)

# Read more at http://stats.idre.ucla.edu/r/dae/poisson-regression/


## Negative binomial regression ####

## Negbin examples####
Pnb <- data.frame(
  label = c(rep("theta = 50", 16), rep("theta = 5", 16),
            rep("theta = 0.5", 16), rep("theta = 0.05", 16)),
  y = rep(seq(0, 15, 1), 4),
  theta = c(rep(50, 16), rep(5, 16), rep(0.5, 16), rep(0.05, 16)))
Pnb$prob <- dnbinom(Pnb$y, mu = 5, size = Pnb$theta)
head(Pnb)

ggplot(Pnb, aes(y, prob, fill = label)) +
  geom_col(position = "dodge") +
  labs(title = "Some negative binomial distributions with mu = 5") +
  theme(text = element_text(size = 14))
  
## Read the data and fit the model:
# This requires the foreign library:

# data.nb <- read.dta("https://stats.idre.ucla.edu/stat/stata/dae/nb_data.dta")

# or use the R-file from the course home page:
load("Data/nb_data.RData")

# Set the progam category names:
data.nb$prog <- factor(
  data.nb$prog, levels = c(1, 2, 3), 
  labels = c("General", "Academic", "Vocational"))
# Also make the student id categorical
data.nb$id <- factor(data.nb$id)
summary(data.nb)
head(data.nb)

ggplot(data.nb, aes(math, daysabs, color = prog)) +
  geom_point() +
  facet_wrap(~ prog)

# Fit a negbin model, reuires MASS####
(model.nb <- glm.nb(daysabs ~ math + prog, data = data.nb))
(sum.nb <- summary(model.nb))
cbind(model.nb$coefficients, confint(model.nb))
exp(cbind(model.nb$coefficients, confint(model.nb)))

# estimated means with confidence intervals####
pred.nb <- cbind(
  data.nb,
  mu = predict(model.nb, type = "response"),
  xb = predict(model.nb, se.fit = TRUE))
pred.nb$xb.residual.scale <- NULL

pred.nb$xb.lwr <- pred.nb$xb.fit - 1.96 * pred.nb$xb.se.fit
pred.nb$xb.upr <- pred.nb$xb.fit + 1.96 * pred.nb$xb.se.fit
pred.nb$mu.lwr <- exp(pred.nb$xb.lwr)
pred.nb$mu.upr <- exp(pred.nb$xb.upr)

ggplot(pred.nb, aes(math, daysabs, color = prog)) +
  geom_point() +
  geom_line(aes(y = mu), size = 1) +
  geom_ribbon(aes(ymin = mu.lwr, ymax = mu.upr), alpha = 0.1) +
  facet_wrap(~ prog)

# Bootstrap prediction intervals####
# calculated using negbin_bootstrap_predint.R

load("Data/nb_predint.RData")

ggplot(pred.nb, aes(math, daysabs, color = prog)) +
  geom_point() +
  geom_line(aes(y = mu)) +
  geom_ribbon(aes(ymin = mu.lwr, ymax = mu.upr), alpha = 0.1) +
  geom_smooth(data = nb.predint, aes(y = pred.lwr), size = 1, se = FALSE) +
  geom_smooth(data = nb.predint, aes(y = pred.upr), size = 1, se = FALSE) +
  labs(title = "Expected number of days absent",
       caption = "95% conf.int. and 95% bootstrap pred.int.") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ prog)

# LR-test comparing Poisson and Negbin####
## Compare with a poisson model
model.pois <- glm(daysabs ~ math + prog, family = "poisson", data = data.nb)
summary(model.pois)

# Likelihood ratio test
# Do NOT use "anova" as that tests two models following 
# the same distributions but this is not our case. 
# Hence I compute everything "by hand".

-2*logLik(model.pois)
-2*logLik(model.nb)

-2*logLik(model.pois)[1] + 2*logLik(model.nb)[1]
qchisq(1 - 0.05, 1)

## Poisson gives the same overall mu as Negative Binomial regression, but 
# the fit does not account for increasing variability for increasing mu.
## We need that theta.

# Are the awards negative binomial####
model.award
model2 <- glm.nb(num_awards ~ math + prog, data = award)
model2
summary(model2)

-2*(logLik(model.award)[1] - logLik(model2)[1])
qchisq(1 - 0.05, 1)

# No, the number of awards does not need a negative binomial, 
# poisson is ok.
