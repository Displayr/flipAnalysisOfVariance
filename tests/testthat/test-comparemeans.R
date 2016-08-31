context("CompareMeans")

test_that("Print", {
    data(cola, package = "flipExampleData")
    expect_error(print(CompareMeans(cola$Q4_A, cola$Q3, compare = "Columns")), NA)
    expect_error(print(CompareMultipleMeans(list(cola$Q4_A, cola$Q4_B, cola$Q4_C), cola$Q3, compare = "Columns")), NA)

CompareMultipleMeans(list(cola$Q4_A, cola$Q4_B, cola$Q4_C), cola$Q3, compare = "Columns")

    data(bank, package = "flipExampleData")
    expect_error(CompareMultipleMeans(list(bank$Fees, bank$Branch, bank$ATM), bank$Overall, compare = "Columns"), NA)
})

