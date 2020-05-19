## R-code for lecture 11, vt20:
# Multinomial, ordinal and quantile regression
# 11/5-20. AL

library(ggplot2)
# Necessary for reading data in Stata format, .dta:
library(foreign)
# Necessary for multinomial regression:
library(nnet)
# Necessary for ordinal regression:
library(MASS)
# Necessary for quantile regresssion:
library(quantreg)

# multinomial####

hsbdemo <- read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")
head(hsbdemo)
# prog: type of program student applies to
# ses: socio-economic status
# read, write, math, science, socst: grades
summary(hsbdemo)

hsbdemo$prog2 <- relevel(hsbdemo$prog, ref = "academic")

(model.multi <- multinom(prog2 ~ ses + write, data = hsbdemo))
(sum.multi <- summary(model.multi))

# Estimated probabilities for each category
head(sum.multi$fitted.values)
# same as
head(predict(model.multi, type = "prob"))

# beta-estimates and standard errors
sum.multi$coefficients
sum.multi$standard.errors

# perform a Wald test:
(z.values <- sum.multi$coefficients/sum.multi$standard.errors)
(p.values <- 2*pnorm(abs(z.values), lower.tail = FALSE))

# confidence interval for beta, and OR:
confint(model.multi)
exp(coef(model.multi))
exp(confint(model.multi))

# predicted probabilities (again)
head(predict(model.multi, type = "probs"))
# predicted category:
predict(model.multi, type = "class")

AIC(model.multi)
BIC(model.multi)

# LR-test comparing nested models:
anova(model.multi, update(model.multi, . ~ . - ses))

pred.multi <- cbind(hsbdemo, 
                    predict(model.multi, type = "probs"),
                    yhat = predict(model.multi))
head(pred.multi)

# plot estimated probabilities
ggplot(pred.multi, aes(x = write)) +
  geom_ribbon(aes(ymin = 0, ymax = academic), 
              fill = "pink") +
  geom_ribbon(aes(ymin = academic, ymax = academic + general),
              fill = "lightblue") +
  geom_ribbon(aes(ymin = academic + general, ymax = 1),
            fill = "lightgreen") +
  facet_wrap(~ ses, labeller = "label_both") +
  labs(title = "Multinomial logistic regression",
       caption = "red = academic, blue = general, green = vocation",
       y = "probability") +
  theme(text = element_text(size = 14))


#ordinal####

my.data <- read.dta("https://stats.idre.ucla.edu/stat/data/ologit.dta")
head(my.data)
# apply: how likely are you to apply to graduate school:
# pared: at least one parent has a graduate degree
# public: the undergraduate institution is public
# gpa: student's grade point average

model.olr <- polr(apply ~ pared + public + gpa, data = my.data)
model.olr
(sum.olr <- summary(model.olr))
sum.olr$coefficients

#beta-estimates
model.olr$coefficients
#zeta-extimates
model.olr$zeta

#confidence intervals for beta and exp(beta):
confint(model.olr)
exp(model.olr$coefficients)
exp(confint(model.olr))

#estimated probabilities:
head(predict(model.olr, type = "prob"))
#predicted category:
predict(model.olr, type = "class")

AIC(model.olr)
BIC(model.olr)

# LR-test comparing nested models:
anova(model.olr, update(model.olr, . ~ . - pared))


pred.olr <- cbind(my.data,
                  predict(model.olr, type = "prob"),
                  yhat = predict(model.olr))

ggplot(pred.olr, aes(x = gpa)) +
  geom_ribbon(aes(ymin = 0, ymax = unlikely), fill = "pink") +
  geom_ribbon(aes(ymin = unlikely, 
                  ymax = unlikely + `somewhat likely`), fill = "lightblue") +
  geom_ribbon(aes(ymin = unlikely + `somewhat likely`,
                  ymax = 1), fill = "lightgreen") +
  facet_grid(pared ~ public, labeller = "label_both") +
  labs(caption = "red = unlikely, blue = somewhat likely, green = very likely") +
  theme(text = element_text(size = 14))


##Quantile regression####
## Skewed####
# distribution (exponential):

# regression with quantiles for simulated observations:
x0 <- data.frame(x = seq(0, 100, 0.5))
# True E(Y) = 4 + x/50:
x0$m <- 4 + x0$x/50

set.seed(3)
x0$s <- seq(5, 1, length.out = nrow(x0))
x0$y <- 5*x0$m + rexp(nrow(x0), 1/x0$s)

ggplot(x0, aes(x, y)) + geom_point() +
  labs(title = "A skewed distribution") +
  theme(text = element_text(size = 14))


(model.quant <- rq(y ~ x, data = x0, tau = c(.1, .5, .9)))

#summary includes confidence intervals
summary(model.quant)

# fitted lines:
yrq <- predict(model.quant)
x0.pred <- cbind(x0, quant = yrq)
head(x0.pred)

ggplot(x0.pred, aes(x, y)) + 
  geom_point() +
  geom_line(aes(y = quant.1), color = "blue", size = 1) +
  geom_line(aes(y = quant.2), color = "blue", size = 1) +
  geom_line(aes(y = quant.3), color = "blue", size = 1) +
  labs(title = "A skewed distribution",
       caption = "10, 50 and 90% quantiles") +
  theme(text = element_text(size = 14))

# global and partial F-test
anova(model.quant)

# plot beta-estimates for the different alpha-values
plot(summary(model.quant))

