context("OneWayANOVA")
data(colas, package = "flipExampleData")
z <- unclass(colas$q4a)
#z[z == 4] <- 9
#z[z == 5] <- 4
#z[z == 9] <- 5
flipFormat::Labels(z) <- "Like Coca-Cola"
colas$like.coke <- z - 3
colas$balanced <- c(rep(1:3, rep(100,3)), rep(NA, 27))
colas$agenumeric <- car::recode(colas$d1, as.factor.result = FALSE, recodes = "'18 to 24' = 21; '25 to 29' = 27; '30 to 34' = 32; '35 to 39' = 37; '40 to 44' = 42; '45 to 49' = 47; '50 to 54' = 52; '55 to 64' = 60; '65 or more' = 77")


test_that("One Way ANOVA - vs SPSS", {
    # Unweighted - cells
    z = OneWayANOVA(colas$like.coke, colas$d1, compare = "To mean")
    # F test
    expect_equal(as.numeric(z$p), 0.268812, tolerance = 0.000001)
    # Means
    expect_equal(z$coef[2,1], 0.794872, tolerance = 0.000001)
    # Unweighted - pairwise
    z <- OneWayANOVA(colas$like.coke, colas$d1, compare = "Pairwise", correction = "None")
    # Means
    expect_equal(z$coef[2,1], 0.075758, tolerance = 0.000001)
    # pairwise p
    expect_equal(z$coef[2,4], 0.764216, tolerance = 0.000001)
    # Tukey HSD with balanced sample
    z <- OneWayANOVA(colas$like.coke, colas$balanced, compare = "Pairwise", correction = "Tukey Range")
    expect_equal(z$coef[1,4], 0.796845, tolerance = 0.00001)
})

test_that("One Way ANOVA - vs Q",
{
    # Dunnett C with balanced sample (compared to Q, being run in debug to inspect p values)
    z <- OneWayANOVA(colas$like.coke, colas$balanced, compare = "To first", correction = "Tukey Range")
    expect_equal(z$coef[1,4], 0.74516637990067847, tolerance = 0.00001)
})


test_that("One Way ANOVA - vs R",
{
    # Dunnett C single-step
    data("recovery", package = "multcomp")
    recover.aov <- aov(minutes ~ blanket, data = recovery)
    recovery.mc <- multcomp::glht(recover.aov, linfct = multcomp::mcp(blanket = "Dunnett"), alternative = "less")
    set.seed(1223)
    zr <- summary(recovery.mc)$test$pvalues[1]
    # Comparing to published example from Bretz et al 2011 (p. 73)
    expect_equal(zr, 0.241, tolerance = 0.001)
    # Comparing to OneWayANOVA
    z <- OneWayANOVA(recovery$minutes, recovery$blanket, compare = "To first", correction = "Tukey Range", alternative = "Less")
    expect_equal(z$coef[1,4], zr, tolerance = 0.00001)
})

test_that("One Way ANOVA - vs Stata",
{
    ## Unweighted - regress q4a i.d1
    # Replicating indicator coding
    z <- OneWayANOVA(colas$like.coke, colas$d1, compare = "To first", correction = "None")
    expect_equal(z$coefs[1,4], 0.157, tolerance = 0.0005)
    # F
    expect_equal(as.numeric(z$f), 1.25, tolerance = 0.005)
    expect_equal(as.numeric(z$p), 0.2688, tolerance = 0.005)

    # Weighted stata - binary predictor  "svy linearized : regress q4a age25"
    num <- as.numeric(colas$d1)
    age25 <- as.integer(colas$d1 == "25 to 29")
    z = OneWayANOVA(colas$like.coke, age25, weights = colas$agenumeric, compare = "To first", correction = "None")#    z = flipRegression::Regression(colas$like.coke ~ age25, weights = colas$agenumeric)
    expect_equal(as.numeric(z$f), 1.39, tolerance = 0.005)
    expect_equal(as.numeric(z$p), .239, tolerance = 0.005)
    expect_equal(z$coefs[1,4], .2387010622, tolerance = 0.005)

    # Weighted stata - categorical predictor  "svy linearized : regress q4a i.d1"
    z = OneWayANOVA(colas$like.coke, colas$d1, weights = colas$agenumeric, compare = "To first", correction = "None")#
    expect_equal(as.numeric(z$f), 1.28, tolerance = 0.03) # F is quite different.
    expect_equal(as.numeric(z$p), 0.2553, tolerance = 0.015)
    expect_equal(z$coefs[1,4], 0.1223921195, tolerance = 0.00005)


})

#
# summary(aov(lm(colas$like.coke ~ colas$d1)))
#
# f <- function(y, x, w)
# {
#     summary(aov(lm(y ~ x, weights = w)))[[1]][1,4]
#
# }
#
# fs <- rep(NA, 500)
# for (i in 1:500)
# {
#     s <- sample(1:327, replace = TRUE)
#     fs[i] <- f(colas$like.coke[s], colas$d1[s], rep(1, 327)[s])
# }
#
# mean(fs > 1.251)
#
#
#
# X <- runif(100)
# Y <- X + rnorm(100, sd = 2)
# fs <- rep(NA, 5000)
# z <- flipRegression::Regression(Y ~ X)
# for (i in 1:5000)
# {
#     s <- sample(1:327, replace = TRUE)
#     dep <- resid(z)[s] + colas$like.coke
#     fs[i] <- f(dep, colas$d1, rep(1, 327))
# }
#
# mean(fs > 1.246)

#
#
#
#      OneWayANOVA(colas$like.coke, colas$balanced, compare = "To first", correction = "Tukey HSD")
#
# xmean = -0.100000
# xsd = 0.154782
# xloewr = -0.468301
# xt <- (xmean - xloewr)  / xsd
#
#
#     z <- aov(lm(like.coke ~ d1, data = colas))
#     TukeyHSD(z)
#     summary(glht(lm(like.coke ~ d1, data = colas), linfct = mcp(d1 = "Tukey")))
#
#     # Weighted - cells
#     wgt <- as.numeric(unclass((colas$q7)))
#     z = OneWayANOVA(colas$like.coke, colas$d1, compare = "Cells", weights = wgt)
#     z# Weighted - pairwise
#     OneWayANOVA(colas$like.coke, colas$d1, compare = "Pairwise", weights = wgt, correction = "None")
# })
#
#
# library(multcomp)
# immer.aov <- aov(like.coke ~ d1, data = colas)
# immer.mc <- glht(immer.aov, linfct = mcp(d1 = "Tukey"))
# summary(immer.mc)
# TukeyHSD(immer.aov)$d1[, 4]
#
#
# library(TukeyC)
# z = colas$like.coke
# summary(TukeyC(x=z, y=colas$d1, model='y ~ x', which='x'))
#
#
# data("immer", package = "MASS")
# immer <- immer[-29:-30, ]
# library(multcomp)
# immer.aov <- aov(Y1 ~ Var, data = immer)
# immer.mc <- glht(immer.aov, linfct = mcp(Var = "Tukey"))
# summary(immer.mc)
# TukeyHSD(immer.aov)$Var[, 4]
#
#
# library(TukeyC)
# summary(TukeyC(x=immer$Var, y=immer$Y1, model='y ~ x', which='x'))
#
#
#
# test_that("One Way ANOVA - vs SPSS", {
#     # Unweighted - cells
#     z = OneWayANOVA(colas$like.coke, colas$d1, compare = "Cells")
#     z$p
#     # Unweighted - pairwise
#     OneWayANOVA(colas$like.coke, colas$d1, compare = "Pairwise", correction = "None")
#
#     # Weighted - cells
#     wgt <- as.numeric(unclass((colas$q7)))
#     z = OneWayANOVA(colas$like.coke, colas$d1, compare = "Cells", weights = wgt)
#     z# Weighted - pairwise
#     OneWayANOVA(colas$like.coke, colas$d1, compare = "Pairwise", weights = wgt, correction = "None")
# })
#
