context("weighting")
preference <- factor(c(rep(c("Trump", "Clinton"), c(55, 45)), rep(c("Trump", "Clinton"), c(40, 60))))
state <- factor(c(rep("Nevada", 100), rep("California", 100)))
california <- 38800000 #Population
nevada <- 2839000 #Population
gross.weights <- c(rep(nevada / 100, 100), rep(california / 100, 100))
dat <- data.frame(preference, state, gross.weights)

test_that("sampling weights",{



xtabs(~preference + state, data = dat)

# Chi-square test
summary(xtabs(~preference + state, data = dat))$p.value

# z-test (performed using binary logit)
summary(glm(preference ~ state, family = binomial, data = dat))$coef[2,4]

# z-test using gross weights

suppressWarnings(summary(glm(preference ~ state, family = quasibinomial, weights = gross.weights, data = dat))$coef[2,4])

# z-test using weights with an average of 1 (i.e., which sum to the sample size)
unit.weights <- gross.weights / mean(gross.weights) * 200
summary(glm(preference ~ state, family = quasibinomial, weights = unit.weights, data = dat))$coef[2,4]

# z-test using weights with an average of 1 (i.e., which sum to the sample size)
unit.weights <- gross.weights / mean(gross.weights)
summary(glm(preference ~ state, family = quasibinomial, weights = unit.weights, data = dat))$coef[2,4]
xtabs(unit.weights ~ state)

# Weight calibrated to the effective sample size
ess <- flipData::EffectiveSampleSize(unit.weights)
calibrated.weight <- unit.weights / sum(unit.weights) * ess
summary(glm(preference ~ state, family = quasibinomial, weights = calibrated.weight, data = dat))$coef[2,4]

# taylor series linearized weight
calibrated.weight <- unit.weights / sum(unit.weights) * ess
flipRegression::Regression(preference ~ state, type = "Binary Logit", weights = gross.weights, data = dat)

supressWarnings(flipAnalysisOfVariance::OneWayANOVA(dat$state, dat$preference, type = "Binary Logit", weights = gross.weights, correction = "None"))

})
