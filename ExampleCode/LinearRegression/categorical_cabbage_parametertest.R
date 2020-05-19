# Lecture 4: 1/4-20: t-test and F-test
# buggfix 1/4-20.

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
## tell R that soil is a categorical variable, not numerical:
cabbage$soil <-
  factor(cabbage$soil,
         levels = c(1, 2, 3),
         labels = c("sand", "clay", "loam"))

# full modell####
# Y = b0 + b1*clay + b2*loam + b3*fertilize
model.full <- lm(headwt ~ soil + fertilize, data = cabbage)
(sum.full <- summary(model.full))

# t-test for fertilizer####
# The t-test for fertilizer is in:
sum.full$coefficients

# compare t-value with t-quantile:
# upper alpha/2-t-quantile with df = 26:
(tvalue <- sum.full$coefficients[4, 3])
qt(1 - 0.05/2, 26)

# calculate P-value:
# 2 * P(|t| > |tvalue|) to cover both tails:
2*pt(abs(tvalue), 26, lower.tail = FALSE)
sum.full$coefficients[4, 4]

# or use CI:
confint(model.full)

# Global F-test for soil and fertilizer####
# The last row of:
sum.full

# Partial F-test for soil####
# Fit the reduced model without soil:
model.reduced <- lm(headwt ~ fertilize, data = cabbage)

# Compare the models:
(cabbage.anova <- anova(model.reduced, model.full))

# Compare the F-value with upper F-quantile:
(Fvalue <- cabbage.anova$F[2])
qf(1 - 0.05, 2, 26)

# calculate P-value:
pf(Fvalue, 2, 26, lower.tail = FALSE)
cabbage.anova$`Pr(>F)`[2]
