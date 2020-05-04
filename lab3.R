##### Lab 3 #####

rm(list = ls())
load("Data/Pb_mossa.rda")
library(ggplot2)

##### 7 Regression diagnostics####

##### 7 (a) #####
# Norrbotten har most observations => use as ref!

# Change of reference location (Norrbotten)
Pb_mossa$loc <- relevel(Pb_mossa$loc, "Norrbotten")

# Fit linear MULTIPLE regression model from 5e
model5e <- lm(log(Pb) ~ I(year - 1975) + loc, data = Pb_mossa)
model5eSummary <- summary(model5e)

# Plot of data, separated by location (geom_jitter is used) 
ggplot(data = Pb_mossa, aes(x = I(year - 1975), y = log(Pb), color = loc)) +
  geom_jitter(width = 1) +
  facet_wrap(~ loc) + 
  xlab("Time (years)") +
  ylab("Pb (ln(mg/kg))") +
  labs(title = "ln(Pb) over time") +
  theme(text = element_text(size = 16))


pred5e <- cbind(Pb_mossa, 
                   fit = predict(model5e),
                   e = residuals(model5e))

# Leverage
pred5e$v <- influence(model5e)$hat
head(pred5e)

# Plot of leverage against time
# with 1/n and 2(p+1)/n horizontal lines:
ggplot(cbind(pred5e), aes(x = I(year-1975), y = v, color = loc, ymin = 0)) +
  geom_jitter(width = 1) +
  facet_wrap(~ loc) +
  geom_hline(yintercept = c(I(1/nrow(Pb_mossa)))) +
  geom_hline(yintercept = I(2*6/nrow(Pb_mossa)), color = "red") +
  labs(title = "Pb concentration: leverage vs year") +
  labs(caption = "y = 1/n (black) and 2(p+1)/n (red)") +
  theme(text = element_text(size = 18))

# High leverage for Orebro, and also some high leverage for Vasternorrland.
# All look like a squared function.

##### 7 (b) #####
# Studentized residuals
pred5e$r <- rstudent(model5e)
head(pred5e)
  
# Plot of studentized residuals against time, one plot for each location
# Horizontal lines for 0, +-2 and +-4.
ggplot(pred5e, aes(x = I(year - 1975), y = r, color = loc)) +
  geom_jitter(width = 1) +
  facet_wrap(~ loc) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), color = "red") +
  geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dashed") +
  xlab("Time (years)") +
  ylab("r*") +
  labs(title = "Pb concentration: studentized residuals vs time") +
  labs(caption = "y = +/- 2 and +/- 4") +
  theme(text = element_text(size = 18))

# Conclusion: Large residual value in VastraGotaland.

##### 7 (c) #####
# plot sqrt(abs(r)) to detect heteroscedastic variance:
ggplot(pred5e, aes(x = I(year - 1975), y = sqrt(abs(r)), color = loc)) +
  facet_wrap(~ loc) +
  geom_jitter(width = 1) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = sqrt(2), color = "red") + 
  geom_hline(yintercept = sqrt(4), color = "red", linetype = "dashed") +
  xlab("Year (0 = 1975)") +
  ylab("sqrt(|r*|)") +
  labs(title = "Pb concentration: sqrt(|r*|) vs year") +
  labs(caption = "y = sqrt(2) and sqrt(4)") +
  theme(text = element_text(size = 18))

# Conclusion: Still outlier in VastraGotaland, maybe increasing variance for
# Jamtland, VasterNorrland and VastraGotaland.

##### 7 (d) #####
# Cook's D (distance)####
pred5e$D <- cooks.distance(model5e)
head(pred5e)

# Plot against r*
ggplot(pred5e, aes(x = I(year - 1975), y = D, color = loc)) + 
  geom_jitter(width = 1) +
  facet_wrap(~ loc) +
  #geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  geom_hline(yintercept = I(4/nrow(Pb_mossa)), color = "red") +
  xlab("r_i*") +
  ylab("D_i") +
  labs(title = "Pb concentration: Cook's D vs studentized residuals") +
  labs(caption = "y = 4/n") +
  theme(text = element_text(size = 18))

# Are there any observations that has had an unpleasantly large influence on the estimates?
# Ans: Yes, 1 outlier in VastraGotaland and some in Orebro and Vasternorrland.
# Were they also observations with high leverage? 
# Ans: No since highest Cook's D in VastraGotaland.

##### 7 (e) #####
# DFBETAS####
head(dfbetas(model5e))
pred5e$df0 <- dfbetas(model5e)[,"(Intercept)"]
pred5e$df1 <- dfbetas(model5e)[,"I(year - 1975)"]

#dfbetas for beta_0:
ggplot(pred5e, aes(x = I(year - 1975), y = df0, color = loc)) +
  geom_jitter(width = 1) +
  facet_wrap(~ loc) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 2/sqrt(nrow(Pb_mossa))*c(-1, 1), color = "red") +
  xlab("Year (1975 = 0") +
  ylab("DFBETAS_0(i)") +
  labs(title = "Pb concentration: DFBETAS_0 vs year") +
  labs(caption = "y = 1 and 2/sqrt(n)") +
  theme(text = element_text(size = 18))

# dfbetas for beta1:
ggplot(pred5e, aes(x = I(year - 1975), y = df1, color = loc)) +
  geom_jitter(width = 1) +
  facet_wrap(~ loc) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 2/sqrt(nrow(Pb_mossa))*c(-1, 1), color = "red") +
  xlab("Year (1975 = 0") +
  ylab("DFBETAS_1(i)") +
  labs(title = "Pb concentration: DFBETAS_1 vs year") +
  labs(caption = "y = 1 and 2/sqrt(n)") +
  theme(text = element_text(size = 18))

# Did the influential points in 7.(d) in Orebro have a large
# influence on the estimate of the rate of decline? - Probably not.
# Did the one in Vastra Gotaland? - Yes for sure.

##### 7 (f) #####
#Select the suspiciously high value of Pb concentration (cheating way)
# looking at the data we see that the outlier is at element 1186
outlier <- 1186

# ??????????????????????????????????????????????????????????

# Plot data from 5b and highlight the outlier in red.
# pike[outlier, ] will select the row(s) with the 
# strange fish and all the columns.
ggplot(Pb_mossa, aes(x = I(year - 1975), y = Pb, color = loc)) + 
  geom_point() +
  geom_point(data = Pb_mossa[outlier, ], color = "red",
             size = 3, shape = 24) +
  facet_wrap(~loc) +
  xlab("Time (years)") + 
  ylab("Pb concentration") + 
  labs(title = "Pb concentration by year",
       caption = "including outlier") +
  theme(text = element_text(size = 18))
  
# Conclusion: Seems logical that the outlier had a large influence on the estimated rate of decline.

##### 8 Model selection####

##### 8 (a) #####
# Add interactions between time (I(year - 1975)) and location to the model (5e).
model8a <- lm(log(Pb) ~ I(year - 1975)*loc, data = Pb_mossa)
model8aSummary <- summary(model8a)

# t-value says throw away locJamtland, I(year - 1975):locOrebro,
# I(year - 1975):locVasternorrland and maybe I(year - 1975):locJamtland

##### 8 (b) #####
# Calculate R^2, R^2adj, AIC and BIC for all three models, 2.(c), 5.(e) and 8.(a).
model2c <- lm(log(Pb) ~ I(year - 1975), data = Pb_mossa)
model2cSummary <- summary(model2c)

# Collect the R2-s for plotting
(collectR2s <- data.frame(
  nr = seq(1, 3),
  model = c("ln(Pb) vs year", "ln(Pb) vs year + loc", "ln(Pb) vs year*loc"),
  R2 = c(model2cSummary$r.squared,
         model5eSummary$r.squared,
         model8aSummary$r.squared),
  R2.adj = c(model2cSummary$adj.r.squared,
             model5eSummary$adj.r.squared,
             model8aSummary$adj.r.squared)))

# ... and plot them
ggplot(collectR2s, aes(model, R2)) +
  geom_point(size = 3) + 
  geom_point(aes(y = R2), color = "red", size = 3) + 
  geom_line(aes(x = nr), size = 1) +
  geom_line(aes(x = nr, y = R2.adj), 
            color = "red", size = 1, linetype = "dashed") +
  geom_hline(yintercept = 1) +
  labs(caption = "R2 (black), R2-adj (red dashed)") +
  labs(title = "Pb concentration: R2 and R2-adjusted") +
  ylab("R2 and R2-adj") +
  theme(text = element_text(size = 18))

# AIC and BIC
(collectAIC <- data.frame(
  nr = seq(1, 3),
  model = c("ln(Pb) vs year", "ln(Pb) vs year + loc", "ln(Pb) vs year*loc"),
  AIC(model2c, model5e, model8a),
  BIC(model2c, model5e, model8a)))

# and plot:
ggplot(collectAIC, aes(model, AIC)) +
  geom_point(size = 3) + 
  geom_point(aes(y = BIC), color = "red", size = 3) + 
  geom_line(aes(x = nr), size = 1) +
  geom_line(aes(x = nr, y = BIC), 
            color = "red", size = 1, linetype = "dashed") +
  labs(caption = "AIC (black), BIC (red dashed)") +
  labs(title = "Pb concentration: AIC and BIC") +
  ylab("AIC and BIC") +
  theme(text = element_text(size = 18))

# Conclusion: They all agree, model 5e (ln(Pb) vs year + loc) is the best.
  
##### 8 (c) #####
I_VG <- which(Pb_mossa$loc == "VastraGotaland")
Pb_mossa$year_VG <- 0
Pb_mossa$year_VG[I_VG] <- Pb_mossa$year[I_VG] - 1975

model8c <- lm(log(Pb) ~ I(year - 1975) + loc + year_VG, data = Pb_mossa)
model8cSummary = summary(model8c)



# Are we fitting the model correct?????????????????????????????????????????



# Collect the R2-s for plotting
(collectR2s <- data.frame(
  nr = seq(1, 4),
  model = c("year", "year + loc", "year*loc", "year + loc + year_VG"),
  R2 = c(model2cSummary$r.squared,
         model5eSummary$r.squared,
         model8aSummary$r.squared,
         model8cSummary$r.squared),
  R2.adj = c(model2cSummary$adj.r.squared,
             model5eSummary$adj.r.squared,
             model8aSummary$adj.r.squared,
             model8cSummary$adj.r.squared)))

# ... and plot them
ggplot(collectR2s, aes(model, R2)) +
  geom_point(size = 3) + 
  geom_point(aes(y = R2), color = "red", size = 3) + 
  geom_line(aes(x = nr), size = 1) +
  geom_line(aes(x = nr, y = R2.adj), 
            color = "red", size = 1, linetype = "dashed") +
  geom_hline(yintercept = 1) +
  labs(caption = "R2 (black), R2-adj (red dashed)") +
  labs(title = "Pb concentration: R2 and R2-adjusted") +
  ylab("R2 and R2-adj") +
  theme(text = element_text(size = 18))

# AIC and BIC
(collectAIC <- data.frame(
  nr = seq(1, 4),
  model = c("year", "year + loc", "year*loc", "year + loc + year_VG"),
  AIC(model2c, model5e, model8a, model8c),
  BIC(model2c, model5e, model8a, model8c)))

# and plot:
ggplot(collectAIC, aes(model, AIC)) +
  geom_point(size = 3) + 
  geom_point(aes(y = BIC), color = "red", size = 3) + 
  geom_line(aes(x = nr), size = 1) +
  geom_line(aes(x = nr, y = BIC), 
            color = "red", size = 1, linetype = "dashed") +
  labs(caption = "AIC (black), BIC (red dashed)") +
  labs(title = "Pb concentration: AIC and BIC") +
  ylab("AIC and BIC") +
  theme(text = element_text(size = 18))

# Conclusion: Still model 5e.
# How much of the variability in log-Pb-levels does it explain? - See value of R/Radj.

##### 8 (d) #####
# Plotting the data for each location (add color = loc in aes)
(
  plotLocExp <- ggplot(data = Pb_mossa, 
                       aes(x = I(year - 1975), y = Pb, color = loc)) +
    geom_jitter(width = 1) +
    facet_wrap(~ loc) +
    xlab("Year (0 = 1975)") +
    ylab("Concentration (mg Pb/Kg moss)") +
    labs(title = "Pb concentration vs year") +
    theme(text = element_text(size = 16))
)

#Add the fitted lines, CI and PI to the plots in 5 (c) and in 5 (b)
pred8c <- cbind(Pb_mossa,
                        conf = predict(model8c, interval = "confidence"),
                        pred = predict(model8c, interval = "prediction"))

# Plot of the unscaled data with fitted line, CI and PI
(plotFinalExp <- plotLocExp +
    geom_line(data = pred8c,
              aes(y = exp(conf.fit)),
              color = "blue", size = 1) +
    geom_ribbon(data = pred8c,
                aes(ymin = exp(conf.lwr), 
                    ymax = exp(conf.upr)),
                alpha = 0.2) +
    geom_line(data = pred8c, 
              aes(y = exp(pred.lwr)),
              color = "red", linetype = "dashed",
              size = 1) +
    geom_line(data = pred8c, 
              aes(y = exp(pred.upr)),
              color = "red", linetype = "dashed",
              size = 1)
)

# Conclusion: Small improvementscompared to 6c.

# BUT probably wrongly fitted model so find that out!




