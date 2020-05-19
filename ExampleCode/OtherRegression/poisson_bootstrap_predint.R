# Define a function that simulates bootstrap prediction
# intervals for Poisson regression.

# Parameters;
# model = the fitted model from the glm()
# newdata = data frame x0 to predict on,
# N = number of bootstrap sample
# p = confidence level of the interval

boot.pois <- function(model, newdata, N, p) {
  odata <- model$data
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

# The data used to fit the model:

award <- read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")
award$prog <- factor(
  award$prog, 
  levels = c(1, 2, 3), 
  labels = c("General", "Academic", "Vocational"))
model.award <- glm(num_awards ~ prog + math, family = "poisson", data = award)

# A data frame to predict on:

math.x0 <- seq(33, 75)
award.predint <- data.frame(
  math = rep(math.x0, 3),
  prog = c(rep("General", length(math.x0)),
           rep("Academic", length(math.x0)),
           rep("Vocational", length(math.x0))))

boot.predint <- boot.pois(model.award, award.predint, 5000, 0.95)

award.predint$pred.lwr <- boot.predint$lower
award.predint$pred.upr <- boot.predint$upper

save(award.predint, file = "Data/award_predint.RData")
