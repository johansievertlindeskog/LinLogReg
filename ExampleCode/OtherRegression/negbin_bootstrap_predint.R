# Define a function that simulates bootstrap prediction
# intervals for Negative binomial regression.

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


# The data used to fit the model:

load("Data/nb_data.RData")
data.nb$prog <- factor(
  data.nb$prog, levels = c(1, 2, 3), 
  labels = c("General", "Academic", "Vocational"))
(model.nb <- glm.nb(daysabs ~ math + prog, data = data.nb))

# A data frame to predict on:

math.x0.nb <- seq(0, 100)
nb.predint <- data.frame(
  math = rep(math.x0.nb, 3),
  prog = c(rep("General", length(math.x0.nb)),
           rep("Academic", length(math.x0.nb)),
           rep("Vocational", length(math.x0.nb))))

boot.predint.nb <- boot.nb(model.nb, data.nb, x0.nb, 5000, 0.95)

nb.predint$pred.lwr <- boot.predint.nb$lower
nb.predint$pred.upr <- boot.predint.nb$upper

save(nb.predint, file = "Data/nb_predint.RData")
