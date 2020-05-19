# Lecture 3b: 30/3-20: Categorical x-variables

# Create the data set:
cabbage <- data.frame(
  soil = c(rep(1, 10), rep(2, 10), rep(3, 10)),
  fertilize = c(10, 15, 20, 25, 30, 40, 45, 50, 55, 60,
                10, 15, 20, 25, 30, 40, 45, 50, 55, 60,
                10, 15, 20, 25, 30, 40, 45, 50, 55, 60),
  headwt = c(1.4, 1.9, 3.4, 2.4, 4.2, 1.9, 1.7, 4.7, 1.9,
             2.0, 3.1, 2.9, 3.6, 3.9, 3.6, 4.0, 3.3, 4.3,
             5.3, 4.6, 1.9, 2.0, 3.1, 3.5, 3.3, 2.4, 2.5,
                                 3.5, 1.9, 3.3))
head(cabbage)

## tell R that soil is a categorical variable, not numerical:
cabbage$soil <-
  factor(cabbage$soil,
         levels = c(1, 2, 3),
         labels = c("sand", "clay", "loam"))
head(cabbage)
summary(cabbage)

library(ggplot2)

# Plot the head weight against soil.
# By puttin color = soil and shape = soil
# inside the aes() ggplot will give each soil type
# its own colour and shape. 
# Size is outside aes() and thus the same for all points.
# Note the expand_limits. Used on all plots on any y-axis
# representing head weight. Makes it easier to compare plots.

(
  plot.data <- ggplot(cabbage, aes(x = soil, y = headwt)) +
    geom_point(aes(color = soil, shape = soil), size = 3) +
    expand_limits(y = c(0, 8)) +
    ylab("Head weight") + 
    xlab("Type of soil") +
    labs(title = "Head weight vs type of soil") +
    theme(text = element_text(size = 18)) +
    labs(caption = "observed data")
)

# Fit the model: Y + b0 + b1*clay + b2*loam
# Use soil as x-variable. Since it is a factor variable
# the lm-function will create dummy variables internally,
# using the first level as reference.
# You can change the reference to, e.g., clay by using
# relevel(soil, "clay") instead of soil.
(model.s <- lm(headwt ~ soil, data = cabbage))
(sum.s <- summary(model.s))
confint(model.s)

# Predictions to get nice horisontal lines
(x0 <- data.frame(
  x = c(0.7, 1.7, 2.7, 1.3, 2.3, 3.3),
  soil = rep(c("sand", "clay", "loam"),2)))
(y0 <- cbind(
  x0, 
  predict(model.s, x0, se.fit = TRUE),
  conf = predict(model.s, x0, interval = "confidence"),
  pred = predict(model.s, x0, interval = "prediction"))
)
y0$conf.fit <- y0$pred.fit <- NULL
y0$se.pred <- sqrt(y0$se.fit^2 + y0$residual.scale^2)
y0

(
  plot.lines <- plot.data + 
    geom_line(data = y0,
              aes(x = x, y = fit, color = soil),
              size = 1.5) +
    labs(caption = "data and fitted values")
)

# Predictions to get vertical lines from b0 to b0+b1
# and b0+b2:
(y0.v <- data.frame(
  x = c(0.7, 0.7, 1.7, 1.7, 2.7, 2.7),
  soil = c("sand", "sand", "sand", "clay", "sand", "loam"),
  type = c("sand", "sand", "clay", "clay", "loam", "loam")))
y0.v <- cbind(y0.v, fit = predict(model.s, y0.v))
y0.v
(plot.linesv <- plot.lines + 
    geom_line(data = y0.v,
              aes(x = x, y = fit, color = type),
              size = 1.5) +
    geom_hline(yintercept = predict(model.s, 
                                    data.frame(soil = "sand"))) +
    labs(caption = "data and fitted values")
)

# add confidence and prediction intervals:
plot.lines + 
  geom_ribbon(data = y0,
              aes(color = soil, y = fit, x = x, ymin = conf.lwr, ymax = conf.upr),
              alpha = 0.1) +
  geom_line(data = y0, aes(x = x, y = pred.lwr, color = soil),
            linetype = "dashed", size = 1) +
  geom_line(data = y0, aes(x = x, y = pred.upr, color = soil),
            linetype = "dashed", size = 1) +
  labs(caption = "data, fitted line, confidence and prediction intervals")

# residuals####
cabbage.pred <- cbind(cabbage, fit = predict(model.s))
cabbage.pred$e <- residuals(model.s)
head(cabbage.pred)
elim <- max(abs(cabbage.pred$e)) * c(-1, 1)

# E vs soil
ggplot(cabbage.pred, aes(x = soil, y = e, color = soil)) +
  geom_point(aes(shape = soil), size = 4) +
  geom_hline(yintercept = 0) +
  expand_limits(y = elim) +
  xlab("Type of soil") +
  ylab("Residuals") +
  labs(title = "Residuals vs type of soil") +
  theme(text = element_text(size = 18))

# e vs y-hat
ggplot(cabbage.pred, aes(x = fit, y = e, color = soil)) +
  geom_point(aes(shape = soil), size = 4) +
  geom_hline(yintercept = 0) +
  expand_limits(y = elim) +
  xlab("Fitted values") +
  ylab("Residuals") +
  labs(title = "Residuals vs fitted values") +
  theme(text = element_text(size = 18))

# qq-plot of e
ggplot(cabbage.pred, aes(sample = e)) +
  geom_qq(size = 4) + geom_qq_line(size = 1) +
  labs(title = "Q-Q-plot") +
  theme(text = element_text(size = 18))

# Separate qq-plot for e for the different soil types.
# facet_wrap(~ soil) creates separate plots:
ggplot(cabbage.pred, aes(sample = e, color = soil,
                         shape = soil)) +
  geom_qq(size = 4) + geom_qq_line(size = 1) +
  facet_wrap(~ soil) +
  labs(title = "Q-Q-plot by soil type") +
  theme(text = element_text(size = 18))

# soil and fertilizer####

# y vs fertilizer for each soil type:
ggplot(cabbage, aes(x = fertilize, y = headwt, color = soil)) +
  geom_point(aes(shape = soil), size = 3) +
  facet_wrap(~ soil) +
  xlab("Fertilizer") +
  ylab("Head weight") +
  labs(title = "Head weight by fertilizer and soil type") +
  labs(caption = "data") +
  theme(text = element_text(size = 18)) +
  expand_limits(y = c(0, 8))

# Y = b0 + b1*clay + b2*loam + b3*fertilize
model.sf <- lm(headwt ~ soil + fertilize, data = cabbage)
(sum.sf <- summary(model.sf))
confint(model.sf)

# predict with intervals for the plots
cabbage.pred.sf <- 
  cbind(cabbage,
        conf = predict(model.sf, interval = "confidence"),
        pred = predict(model.sf, interval = "prediction"))
head(cabbage.pred.sf)

# add intervals to the data.
# WARNING: we cannot use geom_smooth here.
# It uses different slopes and different sigma for each soil!
ggplot(data = cabbage.pred.sf, aes(x = fertilize, y = headwt,
                                   shape = soil, color = soil)) +
  geom_point(size = 3) +
  geom_line(aes(y = conf.fit), size = 1) +
  geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr,
                  group = soil),
                  alpha = 0.1) +
  geom_line(aes(y = pred.lwr), linetype = "dashed", size = 1) +
  geom_line(aes(y = pred.upr), linetype = "dashed", size = 1) +
  facet_wrap(~ soil) +
  xlab("Fertilizer") +
  ylab("Head weight") + 
  labs(title = "Head weight as a function of soil type and fertilizer") +
  labs(caption = "data, fitted line, conf. and pred. interval") +
  theme(text = element_text(size = 18)) +
  expand_limits(y = c(0, 8))

# residuals: not shown in the lecture slides####
cabbage.pred.sf$e <- residuals(model.sf)
head(cabbage.pred.sf)
  # old e-limits
elim
# larges residual now:
max(abs(cabbage.pred.sf$e))
# ..is smaller. Use the old limits for comparisin.

ggplot(cabbage.pred.sf, aes(x = conf.fit, y = e, color = soil)) +
  geom_point(aes(shape = soil), size = 4) +
  expand_limits(y = elim) +
  xlab("Fitted value") +
  ylab("Residuals") +
  labs(title = "Residuals vs fitted values (Y-hat)") +
  theme(text = element_text(size = 18)) +
  geom_hline(yintercept = 0)

ggplot(cabbage.pred.sf, aes(x = fertilize, y = e, color = soil)) +
  geom_point(aes(shape = soil), size = 4) +
  expand_limits(y = elim) +
  geom_hline(yintercept = 0) +
  xlab("Fertilizer") +
  ylab("Residuals") +
  labs(title = "Residuals vs fertilizer") + 
  theme(text = element_text(size = 18)) +
  facet_wrap(~ soil)

ggplot(cabbage.pred.sf, aes(x = soil, y = e, color = soil)) +
  geom_point(aes(shape = soil), size = 4) +
  expand_limits(y = elim) +
  xlab("Soil type") +
  ylab("Residuals") +
  labs(title = "Residuals vs soil type") + 
  theme(text = element_text(size = 18)) +
  geom_hline(yintercept = 0)

ggplot(cabbage.pred.sf, aes(sample = e)) +
  geom_qq(size = 3) + geom_qq_line() +
  theme(text = element_text(size = 18))
  
ggplot(cabbage.pred.sf, aes(sample = e, color = soil, shape = soil)) +
  geom_qq(size = 3) + geom_qq_line() +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ soil)

# soil and fertilizer interaction####
# Add an (actually two) interaction term(s):
model.sfi <- lm(headwt ~ soil*fertilize, data = cabbage)
(sum.sfi <- summary(model.sfi))
confint(model.sfi)

# Predict for plots:
cabbage.pred.sfi <- 
  cbind(cabbage,
        conf = predict(model.sfi, interval = "confidence"),
        pred = predict(model.sfi, interval = "prediction"),
        e = residuals(model.sfi))
head(cabbage.pred.sfi)

# Plot data , fitted lines and intervals:
ggplot(data = cabbage.pred.sfi, 
       aes(x = fertilize, y = headwt,
           shape = soil, color = soil)) +
  geom_point(size = 3) +
  geom_line(aes(y = conf.fit), size = 1) +
  geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr),
              alpha = 0.1) +
  geom_line(aes(y = pred.lwr), linetype = "dashed", size = 1) +
  geom_line(aes(y = pred.upr), linetype = "dashed", size = 1) +
  facet_wrap(~ soil) +
  xlab("Fertilizer") +
  ylab("Head weight") + 
  labs(title = "Head weight as a function of soil type and fertilizer") +
  labs(caption = "data, fitted line, conf. and pred. interval") +
  theme(text = element_text(size = 18)) +
  expand_limits(y = c(0, 8))

# residuals####
# old e-limits
elim
# largest residual now:
max(abs(cabbage.pred.sfi$e))
# ..is smaller. Use the old limits for comparisin.

ggplot(cabbage.pred.sfi, aes(x = conf.fit, y = e, color = soil)) +
  geom_point(aes(shape = soil), size = 4) +
  expand_limits(y = elim) +
  xlab("Fitted value") +
  ylab("Residuals") +
  labs(title = "Residuals vs fitted values (Y-hat)") +
  theme(text = element_text(size = 18)) +
  geom_hline(yintercept = 0)

ggplot(cabbage.pred.sfi, aes(x = fertilize, y = e, color = soil)) +
  geom_point(aes(shape = soil), size = 4) +
  expand_limits(y = elim) +
  geom_hline(yintercept = 0) +
  xlab("Fertilizer") +
  ylab("Residuals") +
  labs(title = "Residuals vs fertilizer by soil") + 
  theme(text = element_text(size = 18)) +
  facet_wrap(~ soil)

ggplot(cabbage.pred.sfi, aes(x = soil, y = e, color = soil)) +
  geom_point(aes(shape = soil), size = 4) +
  expand_limits(y = elim) +
  xlab("Soil type") +
  ylab("Residuals") +
  labs(title = "Residuals vs soil type") + 
  theme(text = element_text(size = 18)) +
  geom_hline(yintercept = 0)

ggplot(cabbage.pred.sfi, aes(sample = e)) +
  geom_qq(size = 4) + geom_qq_line() +
  expand_limits(y = elim) +
  labs(title = "Q-Q-plot") +
  theme(text = element_text(size = 18))

ggplot(cabbage.pred.sfi, aes(sample = e, color = soil, shape = soil)) +
  geom_qq(size = 3) + geom_qq_line() +
  expand_limits(y = elim) +
  labs(title = "Q-Q-plot by soil type") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ soil)

