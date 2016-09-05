context("CompareMeans")

test_that("Print", {
    data(cola, package = "flipExampleData")
    expect_error(capture.output(print(CompareMeans(cola$Q4_A, cola$Q3, compare = "Columns"))), NA)
    expect_error(capture.output(print(CompareMultipleMeans(list(cola$Q4_A, cola$Q4_B, cola$Q4_C), cola$Q3, compare = "Columns"))), NA)

    CompareMultipleMeans(list(A = cola$Q4_A, B = cola$Q4_B, C = cola$Q4_C), cola$Q3, compare = "Columns")
    # Weights.
    CompareMultipleMeans(list(A = cola$Q4_A, B = cola$Q4_B, C = cola$Q4_C), cola$Q3, weights = runif(length(cola$Q3)), compare = "Columns")

    data(bank, package = "flipExampleData")
    expect_error(CompareMultipleMeans(list(bank$Fees, bank$Branch, bank$ATM), bank$Overall, compare = "Columns"), NA)
})

