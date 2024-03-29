context("OneWayANOVA")
data(colas, package = "flipExampleData")
z <- unclass(colas$q4a)
#z[z == 4] <- 9
#z[z == 5] <- 4
#z[z == 9] <- 5
flipFormat::Labels(z) <- "Like Coca-Cola"
colas$like.coke <- z - 3
colas$balanced <- c(rep(1:3, rep(100,3)), rep(NA, 27))
colas$agenumeric <- car::recode(colas$d1, as.factor = FALSE, recodes = "'18 to 24' = 21; '25 to 29' = 27; '30 to 34' = 32; '35 to 39' = 37; '40 to 44' = 42; '45 to 49' = 47; '50 to 54' = 52; '55 to 64' = 60; '65 or more' = 77")
colas$d1MISSING <- colas$d1
colas$like.cokeMISSING <-  colas$like.coke
set.seed(123)
colas$d1MISSING[runif(length(colas$d1MISSING)) > .75] <- NA
colas$like.cokeMISSING[runif(length(colas$d1MISSING)) > .75] <- NA

test_that("Fallback due to problems with underlying sandwich variance estimates", {
    captured.warnings <- capture_warnings(OneWayANOVA(colas$Q5_7_1, colas$d1, robust.se = TRUE))
    convert.warn <- paste0("Data has been automatically converted to numeric. ",
                           "Values are assigned in the order of the categories: ",
                           "1, 2, 3, ...; To use alternative numeric values, ",
                           "transform the data prior including it in this analysis ",
                           "(e.g. by changing its structure). The variable ",
                           "health-conscious - Coke has been converted.")
    tech.warn <- paste0("There is a technical problem with the parameter variance-covariance ",
                        "matrix that has been corrected. This is most likely due to either a ",
                        "problem or the appropriateness of the statistical model (e.g., ",
                        "using weights or robust standard errors where a sub-group in ",
                        "the analysis has no variation in its residuals, or lack of ",
                        "variation in one or more predictors.)")
    expected.warnings <- c(convert.warn, tech.warn)
    expect_setequal(captured.warnings, expected.warnings)
})

test_that("One Way ANOVA - with a weight that removes a category in the predictor",
{
    y <- colas$like.coke
    attr(y, "name") <- "q2a"
    attr(y, "question") <- "Q2 - Liking"
    attr(y, "label") <- "Coca-Cola"
    x <- colas$d1
    attr(x, "name") <- "d1"
    attr(x, "question") <- "D1. Age"
    attr(x, "label") <- "D1. Age"
    expect_error(suppressWarnings(OneWayANOVA(y, x, subset = x != "25 to 29", compare = "To mean")), NA)
    wgt <- as.numeric(x != "25 to 29")
    expect_error(suppressWarnings(OneWayANOVA(y, x, weights = wgt, compare = "To mean")), NA)
    expect_error(suppressWarnings(OneWayANOVA(y, x, weights = wgt, subset = x != "25 to 29", compare = "To mean")), NA)
})

test_that("One Way ANOVA - Comparing options", {
    z = OneWayANOVA(colas$like.coke, colas$d1, compare = "To first", return.all = TRUE)
    z1 = OneWayANOVA(colas$like.coke, colas$d1, compare = "To mean", return.all = TRUE)
    z2 = suppressWarnings(OneWayANOVA(colas$like.coke, colas$d1, compare = "Pairwise", return.all = TRUE))
    expect_true(nrow(z$coefs) < nrow(z1$coefs) & nrow(z1$coefs) < nrow(z2$coefs))
    # Correction
    z = suppressWarnings(OneWayANOVA(colas$like.coke, colas$d1, return.all = TRUE))
    z1 = OneWayANOVA(colas$like.coke, colas$d1, correction = "Bonferroni", return.all = TRUE)
    expect_true(all(z$coefs[, 4] <= z1$coefs[, 4]) & sum(z$coefs[, 4] < z1$coefs[, 4]) > 0)
    # Alternative
    z = suppressWarnings(OneWayANOVA(colas$like.coke, colas$d1, return.all = TRUE))
    z1 = suppressWarnings(OneWayANOVA(colas$like.coke, colas$d1, alternative = "Greater", return.all = TRUE))
    expect_true(z$coefs[9, 4] > z1$coefs[9, 4] & z$coefs[1, 4] < z1$coefs[1, 4])
    # Alternative
    z = suppressWarnings(OneWayANOVA(colas$like.coke, colas$d1, return.all = TRUE))
    z1 = suppressWarnings(OneWayANOVA(colas$like.coke, colas$d1, robust.se = TRUE, return.all = TRUE))
    expect_true(z$coefs[1, 4] != z1$coefs[1, 4])
    # Missing data
    expect_error(OneWayANOVA(colas$like.coke, colas$d1, missing = "Error if missing data", compare = "To mean"), NA)
    expect_error(OneWayANOVA(colas$like.coke, colas$d1MISSING, missing = "Error if missing data", compare = "To mean"))
    expect_error(OneWayANOVA(colas$like.cokeMISSING, colas$d1MISSING, missing = "Error if missing data", compare = "To mean"))
    expect_error(OneWayANOVA(colas$like.cokeMISSING, colas$d1, missing = "Error if missing data", compare = "To mean"))
    # Exclude missing
    expect_error(OneWayANOVA(colas$like.coke, colas$d1MISSING, compare = "To mean"), NA)
    expect_error(OneWayANOVA(colas$like.cokeMISSING, colas$d1MISSING, compare = "To mean"), NA)
    expect_error(OneWayANOVA(colas$like.cokeMISSING, colas$d1, compare = "To mean"), NA)
    # Imputation
    expect_error(OneWayANOVA(colas$like.coke, colas$d1MISSING, missing = "Imputation (replace missing values with estimates)", compare = "To mean"), NA)
    expect_error(OneWayANOVA(colas$like.cokeMISSING, colas$d1MISSING, missing = "Imputation (replace missing values with estimates)", compare = "To mean"), NA)
    expect_warning(OneWayANOVA(colas$like.cokeMISSING, colas$d1, missing = "Imputation (replace missing values with estimates)", compare = "To mean"),
                   "Imputation has been selected, but the data has no missing values.")
    # Show Labels
    z <- OneWayANOVA(colas$like.coke, colas$d1MISSING, show.labels = TRUE, compare = "To first", return.all = TRUE)
    expect_equal(z$title,  "One-way ANOVA: Like Coca-Cola by Age")
        z <- OneWayANOVA(colas$like.coke, colas$d1MISSING, show.labels = TRUE, compare = "To first", return.all = TRUE, outcome.name = "Dog", predictor.name = "Cat")
    expect_equal(z$title,  "One-way ANOVA: Like Coca-Cola by Age")
    z <- OneWayANOVA(colas$like.coke, colas$d1MISSING, show.labels = FALSE, compare = "To first", return.all = TRUE, outcome.name = "Dog", predictor.name = "Cat")
    expect_equal(z$title,  "One-way ANOVA: Dog by Cat")
    # p.cutoff
    z <- OneWayANOVA(colas$like.coke, colas$d1MISSING, show.labels = TRUE, compare = "To first", return.all = TRUE, correction = "None", p.cutoff = .5)
    expect_equal(z$subtitle, "Not significant: F: 0.645 on 8 and 240 degrees-of-freedom; p: 0.74; R-squared: 0.02105")
    z <- OneWayANOVA(colas$like.coke, colas$d1MISSING, show.labels = TRUE, return.all = TRUE, compare = "To first", correction = "None", p.cutoff = .85)
    expect_equal(z$subtitle, "Significant: F: 0.645 on 8 and 240 degrees-of-freedom; p: 0.74; R-squared: 0.02105")
})

test_that("One Way ANOVA - vs SPSS", {
    # Unweighted - cells
    z = OneWayANOVA(colas$like.coke, colas$d1, compare = "To mean", return.all = TRUE)
    # F test
    expect_equal(as.numeric(z$p), 0.268812, tolerance = 0.000001)
    # Means
    expect_equal(z$coef[2,1], 0.794872, tolerance = 0.000001)
    # Unweighted - pairwise
    z <- OneWayANOVA(colas$like.coke, colas$d1, compare = "Pairwise", correction = "None", return.all = TRUE)
    # Means
    expect_equal(z$coef[2,1], 0.075758, tolerance = 0.000001)
    # pairwise p
    expect_equal(z$coef[2,4], 0.764216, tolerance = 0.000001)
    # Tukey HSD with balanced sample
    z <- OneWayANOVA(colas$like.coke, colas$balanced, compare = "Pairwise", correction = "Tukey Range", return.all = TRUE)
    expect_equal(z$coef[1,4], 0.796845, tolerance = 0.00001)
})

test_that("One Way ANOVA - vs Q",
{
    # Dunnett C with balanced sample (compared to Q, being run in debug to inspect p values)
    z <- OneWayANOVA(colas$like.coke, colas$balanced, compare = "To first", correction = "Tukey Range", return.all = TRUE)
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
    z <- OneWayANOVA(recovery$minutes, recovery$blanket, return.all = TRUE, compare = "To first", correction = "Tukey Range", alternative = "Less")
    expect_equal(z$coef[1,4], zr, tolerance = 0.00001)
})

test_that("One Way ANOVA - vs Stata",
{
    ## Unweighted - regress q4a i.d1
    # Replicating indicator coding
    z <- OneWayANOVA(colas$like.coke, colas$d1, return.all = TRUE, compare = "To first", correction = "None")
    expect_equal(z$coefs[1,4], 0.157, tolerance = 0.0005)
    # F
    expect_equal(as.numeric(z$Ftest), 1.25, tolerance = 0.005)
    expect_equal(as.numeric(z$p), 0.2688, tolerance = 0.005)

    # Weighted stata - binary predictor  "svy linearized : regress q4a age25"
    num <- as.numeric(colas$d1)
    age25 <- as.integer(colas$d1 == "25 to 29")
    z = OneWayANOVA(colas$like.coke, age25, weights = colas$agenumeric, return.all = TRUE, compare = "To first", correction = "None")
    expect_equal(as.numeric(z$Ftest), 1.39, tolerance = 0.005)
    expect_equal(as.numeric(z$p), .239, tolerance = 0.005)
    expect_equal(z$coefs[1,4], .2387010622, tolerance = 0.005)

    # Weighted stata - categorical predictor  "svy linearized : regress q4a i.d1"
    z = OneWayANOVA(colas$like.coke, colas$d1, weights = colas$agenumeric, return.all = TRUE, compare = "To first", correction = "None")#
    expect_equal(as.numeric(z$Ftest), 1.28, tolerance = 0.03) # F is quite different.
    expect_equal(as.numeric(z$p), 0.2553, tolerance = 0.015)
    expect_equal(z$coefs[1,4], 0.1223921195, tolerance = 0.00005)


})

test_that("DS-1914 compare p-values analytical vs numeric integration", {
    set.seed(1234)
    z <- OneWayANOVA(runif(327), colas$like.coke, compare = "Pairwise", correction = "Tukey Range", return.all = TRUE)
    expect_equal(z$coef[9, 4], 0.8944926, tolerance = 0.01)
})
