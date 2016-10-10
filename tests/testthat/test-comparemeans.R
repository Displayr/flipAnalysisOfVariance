context("CompareMeans")
data(colas, package = "flipExampleData")
colas$like.coke <- unclass(colas$q4a)
attr(colas$like.coke, "label") <- "Like Coca-Cola"

test_that("Bug: System is computationally singular.CE-626 ",
          {
                data(colas, package = "flipExampleData")
                z <- data.frame(colas$Q5_5_1, colas$Q5_7_1)
                dep <- z[,1]
                ind <- colas$q3
                wgt <- unclass(colas$q7)
                expect_error({
                    suppressWarnings(flipRegression::Regression(dep ~ ind, weigts = wgt))
                    suppressWarnings(OneWayANOVA(dep, ind, weigts = wgt))
                }, NA)
                expect_error(CompareMultipleMeans(dep, ind, weights = wgt))
          })


test_that("Print", {
     data(cola, package = "flipExampleData")
    z <- list(cola$Q4_A, cola$Q4_B, cola$Q4_C)
    zz <- suppressWarnings(CompareMultipleMeans(z, cola$Q3, title = "Title", subtitle = "Sub title", footer = "footer"))
    expect_error(print(zz), NA)

    expect_error((print(suppressWarnings(CompareMultipleMeans(list(A = cola$Q4_A, B = cola$Q4_B, C = cola$Q4_C), cola$Q3)))), NA)
    # Weights.
     expect_error((print(suppressWarnings(CompareMultipleMeans(list(A = cola$Q4_A, B = cola$Q4_B, C = cola$Q4_C), cola$Q3, weights = 10 * runif(length(cola$Q3)))))), NA)

    data(bank, package = "flipExampleData")
    expect_error(suppressWarnings(suppressWarnings(CompareMultipleMeans(list(bank$Fees, bank$Branch, bank$ATM), bank$Overall, compare = "Columns"))), NA)
})


