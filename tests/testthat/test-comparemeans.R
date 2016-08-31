context("CompareMeans")
data(cola, package = "flipExampleData")

CompareMeans(cola$Q4_A, cola$Q3, compare = "Columns")


CompareMultipleMeans(list(cola$Q4_A, cola$Q4_B, cola$Q4_C), cola$Q3, compare = "Columns")#, type = "Linear")


test_that("PrettyMeanComparisons",{

    ft <- "Yo! This footer specifically designed
          to communicate important information.
    Since it is so important, it will of course
    extend over many lines.  In fact, on narrow tables,
    it might take >3.  On wide tables, it might only
    require one.  Feel free to adjust the width,
    and the importance and significance does not
    go away."

    data(weight, package = "flipExampleData")
    z = summary(lm(Weight ~ Height + Age, data = weight))$coef
    #expect_error(PrettyRegressionTable(z, TRUE, footer = ft,  title = "My awesome regression", subtitle = "Big brown dog"), NA)
    PrettyMeanComparisons(z, TRUE, footer = ft,  title = "Linear Discriminant Analysis", subtitle = "Predictive accuracy (94%)")
#
#     ## Linear regression
#     data(bank, package = "flipExampleData")
     library(flipRegression)
zz = Regression(Weight ~ Height + Age, data = weight, detail = FALSE)
#
#     # Linear regression with robust se
#     suppressWarnings(Regression(Overall ~  Fees + Interest + Phone + Branch + Online  +ATM, data = bank, robust.se = TRUE, detail = FALSE))
#
#     # Ordered logit (has a z statistic rather than a t)
#     suppressWarnings(Regression(Overall ~  Fees + Interest + Phone + Branch + Online  +ATM, data = bank, type = "Ordered Logit", detail = FALSE))
#
#     coef.matrix <- summary(lm(Sepal.Length ~ Species * Sepal.Width, iris))$coef
#     rownames(coef.matrix)[1] <- "Big dog"
#     PrettyRegressionTable(coef.matrix, TRUE, footer = ft,  title = "My awesome regression", subtitle = "Big brown dog")
#
#     expect_error(PrettyRegressionTable(coef.matrix, TRUE, footer = ft,  title = "My awesome regression", subtitle = "Big brown dog"), NA)
})
#
# CompareMeans(cola$Q4_A, cola$Q3, compare = "Columns pairwise")
#
#
# mod <- Regression(Q4_A ~ Q2 * Q3, data = cola)
# mod <- Regression(Q4_A ~ Q3, data = cola)
#
# attr(droplevels(cola$Q4_A), "label")
#
# x <- cola$Q3
# # Comparing one category with the rest.
# mod <- Regression(Q4_A ~ Q3, data = cola)
# p <- nlevels(x)
# K <- matrix(table(x), p, p, byrow = TRUE)
# diag(K) <- 0
# K[, 1] <- 0
# K <- -prop.table(K, 1)
# diag(K) <- 1
# K[, 1] <- 0
# result <- glht(mod, linfct = K)
# summary(result, test = adjusted(type= "none"))
#
# cola$x1 <- x == "35 to 39"
# Regression(Q4_A ~ x1, data = cola)
#
#
#
# K <- matrix(-1, p, p)
# diag(K) <- 1
# K <- K * 2
#
#
#
# K <- rbind(a = c(1,  -1, rep(0, 8)0,0,0,0,0,0),
#            b = c(1, 1, 0, -1,0,0,0,0,0))
#
# 50 to 54
#
#
#
# # Removing empty categories
# p <- nlevels(x)
# KDunnett <- cbind(0, diag(p - 1))
# rownames(KDunnett) <- levels(x)[-1]
# K <- KDunnett
#
# summary(mod)
# result <- glht(mod, linfct = K)
# summary(result, test = adjusted(type= "none"))
# summary(result, test = adjusted(type= "fdr"))
#
# result <- glht(mod$original, linfct = mcp(Q3 = "Tukey"))
# result <- glht(mod$original, linfct = mcp(Q3 = "GrandMean"))
#
# summary(result, test = adjusted(type= "none"))
# summary(result, test = adjusted(type= "fdr"))
#
# summary(result, test = adjusted(type= "fdr"))
# mod <- Regression(Q4_A ~ Q3  -1, data = cola)
# mod
#
#
# plot(print(confint(result)))
# result <- glht(mod)
# summary(result)
# plot(print(confint(result)))
#
# summary()
#
#
#
#
# K <- rbind(a = c(1,  -1, rep(0, 8)0,0,0,0,0,0),
#            b = c(1, 1, 0, -1,0,0,0,0,0))
#
#
#
# wgt <- runif(nrow(warpbreaks))
#
# K1 <- glht(mod$original, mcp(wool = "Tukey"))$linfct
# K2 <- glht(mod$original, mcp(tension = "Tukey"))$linfct
#
# library(multcomp)
# summary(glht(mod, linfct = rbind(K1, K2)))
#
# mod <- glm(breaks ~ wool + tension, data = warpbreaks, weights = wgt)
# K1 <- glht(mod, mcp(wool = "Tukey"))$linfct
# K2 <- glht(mod, mcp(tension = "Tukey"))$linfct
#
# summary(glht(mod, linfct = rbind(K1, K2)))
