# # Test evaluating a bootstrap ANOVA.
#
#
# context("Testing weight calibration on f-tests")
#
# library(survey)
# data(api)
# set.seed(123)
# n <- 33
# x <- factor(c(rep(c("A", "B", "c"), n)))
#
# test_that("ANOVA and regTermTest are similar when assumptions are met SRS")
#
#
# # Constant variance - unweighted
# n.iterations <- 1000
# result <- data.frame(survey = rep(NA, n.iterations), lm = NA)
# sigma <- rep(1, 3)
# for (i in 1:n.iterations)
# {
#     y <- rnorm(n * 3, mean = 0, sd = sigma[unclass(x)])
#     d <- svydesign(id= ~1, weights=~1, data = data.frame(x, y))
#     m <- svyglm(y ~ x, design = d)
#     result[i, 1] <- regTermTest(m, "x")$p
#     result[i, 2] <- anova(lm(y ~ x))[1, 5]
# }
# summary(result)
# expect_true(cor(result[2.1]) > 0.995) # Same conclusion as F-Test
# expect_true(ks.test(result$survey, "punif") > 0.05)
# constant.variance <- result
#
#
# #### Constant variance - weighted
# # LM does badly (as expected). survey also does poorly (but not as poorly)
# n.iterations <- 1000
# result <- data.frame(survey = rep(NA, n.iterations), lm = NA, calibrated = NA, bootstrapped.ess = NA, bootrapped = NA)
# sigma <- rep(1, 3)
# for (i in 1:n.iterations)
# {
#     y <- rnorm(n * 3, mean = 0, sd = sigma[unclass(x)])
#     yw <- runif(n * 3)
#     d <- svydesign(id= ~1, weights=~w, data = data.frame(x, y))
#     m <- svyglm(y ~ x, design = d)
#     result[i, 1] <- regTermTest(m, "x")$p
#     result[i, 2] <- anova(lm(y ~ x, weights = w))[1, 5]
#     w.calibrated.ess <- flipData::CalibrateWeight(w, x)
#     result[i, 3] <- anova(lm(y ~ x, weights = w.calibrated.ess))[1, 5]
#     boostrapped.data <- suppressWarnings(flipTransformations::AdjustDataToReflectWeights(data.frame(y, x), w.calibrated.ess, seed = NULL))
#     result[i, 4] <- anova(lm(y ~ x, data = boostrapped.data))[1, 5]
#     w.calibrated.n <- w.calibrated.ess / sum(w.calibrated.ess) * length(w.calibrated.ess)
#     boostrapped.data <- suppressWarnings(flipTransformations::AdjustDataToReflectWeights(data.frame(y, x), w.calibrated.n, seed = NULL))
#     result[i, 5] <- anova(lm(y ~ x, data = boostrapped.data))[1, 5]
# }
# summary(result)
# cor(result$survey, result$lm)
# apply(result, 2, FUN = function(q) ks.test(q, "punif") )
# ks.test(result$survey, "punif")
# ks.test(result$lm, "punif")
#
#
#
#
# constant.variance <- result
#
#
# # Non-constant variance
# n.iterations <- 1000
#
# constant.variance <- data.frame(survey = rep(NA, n.iterations), lm = NA)
# sigma <- c(0.5, 1, 4)
# for (i in 1:n.iterations)
# {
#     y <- rnorm(n * 3, mean = 0, sd = sigma[unclass(x)])
#     d <- svydesign(id= ~1, weights=~1, data = data.frame(x, y))
#     m <- svyglm(y ~ x, design = d)
#     constant.variance[i, 1] <- regTermTest(m, "x")$p
#     constant.variance[i, 2] <- anova(lm(y ~ x))[1, 5]
# }
# summary(constant.variance)
# cor(constant.variance)
#
#
# d <- svydesign(id=~dnum+snum, weights=~pw, data=apiclus2)
