context("Table of Means")

test_that("TableOfMeans", {
    data(colas, package = "flipExampleData")

    suppressWarnings(TableOfMeans(colas$q2c, colas$d1, colas$d2))
    suppressWarnings(OneWayANOVA(colas$q2c, colas$d2, subset = colas$d1 == "18 to 24", compare = "To mean"))

    # Filtered
    suppressWarnings(TableOfMeans(colas$q2c, colas$d1, colas$d2, subset = colas$d1 == "18 to 24", correction = "None"))
    suppressWarnings(OneWayANOVA(colas$q2c, colas$d2, subset = colas$d1 == "18 to 24", compare = "To mean", correction = "None"))

    # Weighted
    wgt <- as.integer(colas$d1 == "18 to 24")
    suppressWarnings(TableOfMeans(colas$q2c, colas$d1, colas$d2, weights = wgt, correction = "None"))
    suppressWarnings(OneWayANOVA(colas$q2c, colas$d2, subset = colas$d1 == "18 to 24", compare = "To mean", correction = "None"))

    # Robust
    suppressWarnings(TableOfMeans(colas$q2c, colas$d1, colas$d2, subset = colas$d1 == "18 to 24", correction = "None", robust.se = TRUE))
    suppressWarnings(OneWayANOVA(colas$q2c, colas$d2, subset = colas$d1 == "18 to 24", compare = "To mean", correction = "None", robust.se = TRUE))

    # Variable names
    suppressWarnings(TableOfMeans(colas$q2c, colas$d1, colas$d2, show.labels = FALSE))
    })
