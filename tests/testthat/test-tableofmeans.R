context("Table of Means")

test_that("TableOfMeans", {
    data(colas, package = "flipExampleData")

    TableOfMeans(colas$q2c, colas$d1, colas$d2)
    OneWayANOVA(colas$q2c, colas$d2, subset = colas$d1 == "18 to 24", compare = "To mean")


    # Filtered
    TableOfMeans(colas$q2c, colas$d1, colas$d2, subset = colas$d1 == "18 to 24" || colas$d1 == "25 to 29")
    OneWayANOVA(colas$q2c, colas$d2, subset = colas$d1 == "18 to 24", compare = "To mean")


    TableOfMeans(colas$q2c, colas$d1, colas$d2, correction = "Tukey Range")



    wgt <- unclass(colas$q2d)
    TableOfMeans(colas$q2c, colas$d1, colas$d2, weights = wgt)


    z <- list(cola$Q4_A, cola$Q4_B, cola$Q4_C)
    zz <- suppressWarnings(CompareMultipleMeans(z, cola$Q3, title = "Title", subtitle = "Sub title", footer = "footer"))
    expect_error(print(zz), NA)

    expect_error((print(suppressWarnings(CompareMultipleMeans(list(A = cola$Q4_A, B = cola$Q4_B, C = cola$Q4_C), cola$Q3)))), NA)
    # Weights.
    expect_error((print(suppressWarnings(CompareMultipleMeans(list(A = cola$Q4_A, B = cola$Q4_B, C = cola$Q4_C), cola$Q3, weights = 10 * runif(length(cola$Q3)))))), NA)

    data(bank, package = "flipExampleData")
    expect_error(suppressWarnings(suppressWarnings(CompareMultipleMeans(list(bank$Fees, bank$Branch, bank$ATM), bank$Overall))), NA)
})




# In SPSS, the priors are always the oberved priors when fitting the model. In MASS:lda, the priors are used when fitting.
test_that("LDA",
          {

              data(hbatwithsplits, package = "flipExampleData")
              hair <- hbatwithsplits

              hair1  <- flipTransformations::AsNumeric(hair[, paste0("x",6:18)], binary = FALSE, remove.first = TRUE)
              hair1$x1 <- hair$x1
              hair1$split60 <- hair$split60
              hair1$id <- hair$id
              library(flipMultivariates)
              zLDA <- suppressWarnings(LDA(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18, method = "moment", data = hair1, subset = split60 == "Estimation Sample", show.labels = TRUE))
              expect_error(print(zLDA), NA)

          })
