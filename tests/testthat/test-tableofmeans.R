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

outcome <- rnorm(10)
row.input <- as.factor(letters[rep(1:2,times=5)])
test_that("DS-4284: Check for constant inputs",
{
    const <- integer(length(outcome))
    expect_error(TableOfMeans(const, row.input, outcome),
                 "All values in Outcome are identical.")
    expect_error(TableOfMeans(outcome, const, row.input),
                 "All values in Rows are identical.")
    expect_error(TableOfMeans(outcome, row.input, const),
                 "All values in Columns are identical.")
})

test_that("DS-4284: Check for identical inputs",
{
    expect_error(TableOfMeans(outcome, row.input, row.input),
                 "The variables in Rows and Columns contain identical values")
    expect_error(TableOfMeans(outcome, row.input, outcome),
                 "The variables in Outcome and Columns contain identical values")
    expect_error(TableOfMeans(outcome, outcome, row.input),
                 "The variables in Outcome and Rows contain identical values")
})

test_that("DS-4284: Check inputs have same length (are from same dataset)",
{
    input.wrong.n <- c(1, outcome)
    expect_error(TableOfMeans(outcome, row.input, input.wrong.n),
                 "The variables in Outcome and Columns are different lengths.")
    expect_error(TableOfMeans(outcome, input.wrong.n, row.input),
                 "The variables in Outcome and Rows are different lengths.")
    expect_error(TableOfMeans(outcome, row.input, row.input,
                              subset = logical(length(outcome)+1)),
                 "The variables in Outcome and Filter\\(s\\) are different lengths.")
    expect_error(TableOfMeans(outcome, row.input, row.input,
                              weight = numeric(length(outcome)+1)),
                 "The variables in Outcome and Weight are different lengths.")
})
