# Lecture 3a: multiple regression
# Elasticity

library(ggplot2)

# Small data set so we can create the data frame by "hand":
elasticity <- 
  data.frame(
    tension = c(152, 150, 103, 99, 88, 89, 122, 120, 
                162, 161),
    temp = c(180, 180, 190, 190, 200, 200, 210, 210, 
             220, 220),
    pressure = c(450, 450, 375, 375, 350, 350, 375, 
                 375, 450, 450)
  )
elasticity

# Plot tension vs temperature:
ggplot(elasticity, aes(x = temp, y = tension)) +
  geom_point(size = 4) +
  xlab("Temperature (C)") +
  ylab("Tension (N/mm^2)") +
  labs(title = "Tension (Y) against temperature (x1)") +
  theme(text = element_text(size = 18)) +
  labs(caption = "Quadratic?")

# plot tension vs pressure:
ggplot(elasticity, aes(x = pressure, y = tension)) +
  geom_point(size = 4) +
  xlab("Pressure (kg/cm^2)") +
  ylab("Tension (N/mm^2)") +
  labs(title = "Tension (Y) against pressure (x2)") +
  theme(text = element_text(size = 18)) +
  labs(caption = "Linear?")

# plot pressure vs temperature:
ggplot(elasticity, aes(x = temp, y = pressure)) +
  geom_point(size = 4) +
  xlab("Temperature (C)") +
  ylab("Pressure (kg/cm^2)") +
  labs(title = "Pressure (x2) against Temperature (x1)") +
  theme(text = element_text(size = 18)) +
  labs(caption = "Quadratic x1-x2-pattern!")

# For a look at the fitted plane, download and save
# the file elasticity3d.rda.
# A rotatble version of the fitted plane is saved in
# variable p1:
load("Data/elasticity3d.rda")
p1

# fit linear model####
model.elast <- lm(tension ~ temp + pressure, data = elasticity)
summary(model.elast)
confint(model.elast)
(sigma.elast <- summary(model.elast)$sigma)


# predict Y0####
x0 <- data.frame(temp = 200, pressure = 400)
(y0 <- cbind(x0, predict(model.elast, x0, se.fit = TRUE),
             conf = predict(model.elast, x0, interval = "confidence"),
             pred = predict(model.elast, x0, interval = "prediction")))
y0$se.pred <- sqrt(sigma.elast^2 + y0$se.fit^2)
y0$conf.fit <- y0$pred.fit <- NULL
y0

# A rotatable version of the plane with intervals from
# elasticity3d.rda;
p2

# interaction####
# Use x1*x2 for a model with x1, x2 and x1x2.
# The interaction term is denoted by x1:x2 in the output:
model.inter <- lm(tension ~ temp*pressure, data = elasticity)
(sum.inter <- summary(model.inter))
confint(model.inter)
(sigma.inter <- summary(model.inter)$sigma)

# Calculate the different example slopes
model.inter$coefficients["temp"] + 
  c(350, 400, 450) * 
  model.inter$coefficients["temp:pressure"]

# rotatable plot of fitted plane from elasticity3d.rda
p3

# predict Y0: interaction####
# Note that we can use the same x0 as before.
# We do not have to create the interaction. 
# The predict function knowns it is included in the model.

(y0.int <- cbind(x0, predict(model.inter, x0, se.fit = TRUE),
             conf = predict(model.inter, x0, interval = "confidence"),
             pred = predict(model.inter, x0, interval = "prediction")))
y0.int$se.pred <- sqrt(sigma.inter^2 + y0.int$se.fit^2)
y0.int$conf.fit <- y0.int$pred.fit <- NULL
y0.int

# rotatable plot of fitted plane from elasticity3d.rda
p4

# Residual analysis####
# save predictions and residuals from both models;
elast.pred <- cbind(elasticity, 
                    fit = predict(model.elast),
                    fit.int = predict(model.inter))
elast.pred
elast.pred$e <- residuals(model.elast)
elast.pred$e.int <- residuals(model.inter)
elast.pred

(elim <- max(abs(elast.pred$e),
             abs(elast.pred$e.int)) * c(-1, 1))

# plot both sets of residuals in the same plot:

# e vs fitted plane with different colours and shapes:
ggplot(elast.pred, aes(x = fit, y = e)) +
  geom_point(size = 4) +
  geom_point(aes(x = fit.int, y = e.int), 
             color = "red", shape = 15, size = 4) +
  geom_hline(yintercept = 0) +
  expand_limits(y = elim) +
  theme(text = element_text(size = 18)) +
  labs(title = "Residuals vs Y-hat") +
  labs(caption = "without (black) and with (red) interaction") +
  xlab("Fitted values") +
  ylab("Residuals")

# e vs temperature:
ggplot(elast.pred, aes(x = temp, y = e)) +
  geom_point(size = 4) +
  geom_point(aes(y = e.int), 
             color = "red", shape = 15, size = 4) +
  geom_hline(yintercept = 0) +
  expand_limits(y = elim) +
  theme(text = element_text(size = 18)) +
  labs(caption = "without (black) and with (red) interaction") +
  labs(title = "Residuals vs temperature") +
  xlab("Temperature") +
  ylab("Residuals")

# e vs pressure
ggplot(elast.pred, aes(x = pressure, y = e)) +
  geom_point(size = 4) +
  geom_point(aes(y = e.int), 
             color = "red", shape = 15, size = 4) +
  geom_hline(yintercept = 0) +
  expand_limits(y = elim) +
  theme(text = element_text(size = 18)) +
  labs(caption = "without (black) and with (red) interaction") +
  labs(title = "Residuals vs pressure") +
  xlab("Pressure") +
  ylab("Residuals")

# e vs the interaction temp*pressure:
ggplot(elast.pred, aes(x = I(temp*pressure), y = e)) +
  geom_point(size = 4) +
  geom_point(aes(y = e.int), 
             color = "red", shape = 15, size = 4) +
  geom_hline(yintercept = 0) +
  expand_limits(y = elim) +
  theme(text = element_text(size = 18)) +
  labs(caption = "without (black) and with (red) interaction") +
  labs(title = "Residuals vs temperature * pressure") +
  xlab("Temperature * Pressure") +
  ylab("Residuals")

# qq-plots:
ggplot(elast.pred, aes(sample = e)) +
  geom_qq(size = 4) + geom_qq_line(size = 1) +
  geom_qq(aes(sample = e.int), 
          size = 4, color = "red", shape = 15) +
  geom_qq_line(aes(sample = e.int), color = "red", size = 1) +
  theme(text = element_text(size = 18)) +
  labs(caption = "without (black) and with (red) interaction") +
  labs(title = "Q-Q-plot of residuals")
