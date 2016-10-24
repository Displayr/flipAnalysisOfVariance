# # Example 1
# library(WRS2)
# BootstrappedR2(viagra$libido, viagra$dose)
# t1waybt(libido ~ dose, data = viagra, tr = 0)
# summary(lm(libido ~ dose, data = viagra))
# anova(lm(libido ~ dose, data = viagra))
#
#
#
# #test_that("ANOVA and regTermTest are similar when assumptions are met SRS")
#
#
# # Simulation
# x <- factor(c(rep(c("A", "B", "c"), 33)))
#
#
# # Compare alternative tests
# compareTest <- function(n.iterations, x, sigmas, weight)
# {
#     require(survey)
#     result <- data.frame(survey = rep(NA, n.iterations), lm = NA, calibrated = NA, bootrapped = NA)
#     set.seed(123)
#     n <- length(x)
#     for (i in 1:n.iterations)
#     {
#         y <- rnorm(n , mean = 0, sd = sigmas[unclass(x)])
#         w <- if (weight) runif(n) else rep(1, n)
#         d <- svydesign(id= ~1, weights = ~w, data = data.frame(x, y))
#         m <- svyglm(y ~ x, design = d)
#         result[i, 1] <- regTermTest(m, "x")$p
#         result[i, 2] <- anova(lm(y ~ x, weights = w))[1, 5]
#         w.calibrated.ess <- flipData::CalibrateWeight(w, x)
#         result[i, 3] <- anova(lm(y ~ x, weights = w.calibrated.ess))[1, 5]
#         result[i, 4] <- BootstrappedR2(y, x, weights = w)$p
#     }
#     result
# }
# summary.compareTest <- function(x)
# {
#     print(summary(x))
#     print(cor(x))
#     print(apply(x, 2, FUN = function(q) ks.test(q, "punif") ))
#
# }
#
# n.iterations <- 1000
# homo <- compareTest(n.iterations, x, rep(1, 3), FALSE)
# summary.compareTest(homo)
# homo.weighted <- compareTest(n.iterations, x, rep(1, 3), TRUE)
# summary.compareTest(homo.weighted)
# hetero.weighted <- compareTest(n.iterations, x, c(0.5, 1, 2), TRUE)
# summary.compareTest(hetero.weighted)
#
#
