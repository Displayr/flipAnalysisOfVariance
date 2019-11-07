context("pvalsByGroup")
data(colas, package = "flipExampleData")
n <- nrow(colas)
set.seed(1223)
wgt <- runif(n)
numeric <- as.numeric(colas$q1a)
# Two categories

test_that("binary",{
    ps <- flipAnalysisOfVariance:::pvalsByGroup(colas$d1 == "45 to 49", colas$d3 == "Male", weights = NULL, is.binary = TRUE)
    expect_equal(ps[1], 0.932410088487, tol = 1e-8)
})

test_that("binary weight",{
    ps <- flipAnalysisOfVariance:::pvalsByGroup(colas$d1 == "45 to 49", colas$d3 == "Male", weights = wgt, is.binary = TRUE)
    expect_equal(ps[1], 0.6627924087214, tol = 1e-8)
})

test_that("numeric",{
    ps <- flipAnalysisOfVariance:::pvalsByGroup(numeric, colas$d3 == "Male", weights = NULL, is.binary = FALSE)
    expect_equal(ps[1], 0.4080193773081, tol = 1e-8)
})

test_that("numeric weight",{
    ps <- flipAnalysisOfVariance:::pvalsByGroup(numeric, colas$d3 == "Male", weights = wgt, is.binary = FALSE)
    expect_equal(ps[1], 0.1697432031143, tol = 1e-8)
})

test_that("numeric rank",{
    # Manually dealing with the rank
    ps <- flipAnalysisOfVariance:::pvalsByGroup(rank(numeric, na.last = TRUE), colas$d3 == "Male", weights = NULL, is.binary = FALSE)
    expect_equal(ps[1], 0.3507862447847, tol = 1e-8)
    # Automatically dealing with the rank
    ps <- flipAnalysisOfVariance:::pvalsByGroup(numeric, colas$d3 == "Male", weights = NULL, is.binary = FALSE, non.parametric = TRUE)
    expect_equal(ps[1], 0.3507862, tol = 1e-7)
})

test_that("numeric rank and weight",{
    ps <- flipAnalysisOfVariance:::pvalsByGroup(numeric,
                                                colas$d3 == "Male",
                                                weights = wgt,
                                                is.binary = FALSE,
                                                non.parametric = TRUE)
    expect_equal(ps[1], 0.1423815205331, tol = 1e-8)
})

