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
    expect_error(suppressWarnings(TableOfMeans(colas$q2c, colas$d1, colas$d2, correction = "None", robust.se = TRUE)), NA)
    suppressWarnings(OneWayANOVA(colas$q2c, colas$d2, compare = "To mean", correction = "None", robust.se = TRUE))

    # Variable names
    suppressWarnings(TableOfMeans(colas$q2c, colas$d1, colas$d2, show.labels = FALSE))

    # Numeric variables
    data(bank, package = "flipExampleData")
    attach(bank)
    expect_error(TableOfMeans(Overall, Interest, Phone, show.labels = TRUE), NA)
    detach(bank)

    bank <- bank[bank$Interest < 3  & bank$Phone > 2  & bank$Phone < 5 , ]
    attach(bank)
    expect_error(TableOfMeans(Overall, Interest, Phone, robust.se = TRUE), NA)
    detach(bank)

    })
