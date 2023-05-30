context("Multiple Means")

outcomes <- data.frame(
    Q050__1 = c(NA, 4L, 4L, 3L, 5L, NA, 4L, NA, NA, 3L, 2L, 3L, 5L, NA, 5L, NA, NA, 4L, NA, NA),
    Q050__2 = c(4L, 4L, NA, NA, NA, NA, NA, 5L, NA, NA, 4L, 4L, 5L, NA, NA, NA, 5L, NA, NA, NA),
    Q050__3 = c(3L, NA, 4L, NA, NA, 3L, 3L, NA, NA, NA, 4L, NA, NA, 3L, 5L, 5L, NA, NA, NA, 3L),
    Q050__4 = c(4L, NA, NA, NA, 5L, 4L, 4L, 5L, NA, 4L, 4L, NA, NA, NA, NA, 2L, NA, NA, NA, NA),
    Q050__5 = c(NA, 3L, NA, NA, NA, 2L, 4L, 3L, 4L, NA, NA, 4L, 4L, 3L, 4L, NA, 4L, NA, NA, 3L),
    Q050__6 = c(5L, 4L, NA, NA, NA, 5L, NA, NA, 4L, 5L, NA, 5L, NA, NA, NA, 4L, 3L, 4L, 4L, 3L),
    Q050__7 = c(5L, 4L, NA, 5L, NA, NA, NA, NA, 4L, NA, NA, NA, 5L, 5L, NA, 5L, NA, NA, 4L, 3L),
    Q050__8 = c(NA, NA, 4L, 4L, 4L, NA, 5L, 2L, NA, 4L, NA, NA, NA, 5L, NA, NA, 4L, 4L, 4L, 3L),
    Q050__9 = c(5L, 4L, NA, NA, NA, 2L, NA, 3L, NA, NA, NA, NA, NA, NA, NA, 4L, 2L, 3L, 3L, NA),
    Q050__10 = c(NA, NA, 5L, NA, NA, 5L, 4L, NA, NA, NA, 4L, NA, 5L, NA, 4L, NA, NA, 5L, NA, NA),
    Q050__11 = c(5L, NA, NA, 5L, 5L, NA, NA, 5L, 5L, 5L, 5L, 4L, 5L, 5L, NA, 5L, NA, NA, 4L, NA),
    Q050__12 = c(5L, NA, NA, 5L, 5L, 4L, NA, NA, NA, 4L, NA, NA, NA, NA, NA, NA, NA, 4L, NA, NA),
    Q050__13 = c(4L, NA, 5L, NA, 4L, NA, NA, NA, 5L, NA, NA, 4L, 5L, 4L, NA, 3L, 5L, 4L, NA, 3L),
    Q050__14 = c(NA, 4L, 5L, NA, NA, 4L, NA, 5L, 4L, NA, NA, NA, 5L, 5L, 4L, NA, NA, 5L, 5L, NA),
    Q050__15 = c(5L, 5L, NA, NA, 5L, NA, 4L, 4L, 5L, NA, NA, 4L, NA, 5L, 5L, NA, 5L, NA, 4L, 5L),
    Q050__16 = c(NA, NA, 5L, 4L, 4L, NA, 5L, NA, NA, NA, NA, 4L, NA, NA, 5L, 5L, 5L, 5L, NA, 5L),
    Q050__17 = c(NA, NA, 5L, 4L, NA, NA, 4L, 4L, NA, NA, NA, NA, 5L, NA, NA, NA, 4L, NA, 4L, NA),
    Q050__18 = c(NA, 3L, 3L, 3L, 3L, NA, 4L, 2L, NA, 3L, 3L, 3L, NA, NA, 3L, NA, NA, NA, 3L, NA),
    Q050__19 = c(NA, NA, NA, NA, NA, 3L, NA, NA, 3L, NA, NA, 5L, NA, 1L, 2L, 5L, NA, NA, 3L, NA),
    Q050__20 = c(5L, NA, 5L, 4L, NA, NA, NA, 4L, 4L, 4L, 4L, NA, 4L, 4L, 3L, 4L, 5L, NA, NA, NA),
    Q050__21 = c(NA, 5L, 5L, NA, 5L, NA, NA, NA, 5L, NA, NA, NA, 5L, NA, 3L, 5L, NA, 5L, NA, 5L),
    Q050__22 = c(NA, NA, NA, 3L, 5L, 5L, 4L, 4L, NA, 4L, 4L, NA, NA, 3L, 4L, 4L, NA, NA, NA, 3L),
    Q050__23 = c(NA, 3L, NA, 2L, NA, NA, 4L, NA, 4L, 2L, 3L, NA, 5L, NA, NA, NA, NA, NA, 3L, 3L),
    Q050__24 = c(5L, 2L, 3L, NA, 3L, 3L, NA, NA, NA, 2L, 3L, 2L, NA, NA, NA, NA, 2L, 3L, 2L, NA),
    Q050__25 = c(NA, NA, NA, 2L, NA, 5L, NA, NA, 4L, 4L, 3L, 4L, NA, 2L, NA, NA, 3L, 5L, NA, 4L)
)

predictor <- factor(
    c(2L, 1L, 2L, 1L, 2L, 2L, 2L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 2L, 1L, 1L),
    labels = c("Cluster 1\n47%", "Cluster 2\n53%")
)

test_that("Multiple means data frame output",
{
    expected.rownames <- paste0("Q050__", 1:25)
    expected.means <- array(
        c(c(3.4, 4.5, 3.75, 4.33333333333333, 3.33333333333333, 3.8, 4.2, 3.71428571428571,
            3, 4, 4.83333333333333, 4.5, 4, 4.6, 4.71428571428571, 4.75, 4, 2.85714285714286,
            2, 4, 4.33333333333333, 3.57142857142857, 2.66666666666667, 2.2, 3),
          c(4.16666666666667, 4.33333333333333, 3.6, 3.8, 3.6, 4.5, 4.75, 4.25, 3.5, 4.8, 4.83333333333333,
            4.5, 4.25, 4.6, 4.6, 4.66666666666667, 4.66666666666667, 3.25, 4, 4.4, 5,
            4.5, 4.33333333333333, 3.16666666666667, 4.5)),
        dim = c(25L, 2L),
        dimnames = list(expected.rownames, c("Cluster 1 47% n: 10", "Cluster 2 53% n: 10"))
    )
    expected.r.square <- array(
        c(0.166352201257862, 0.0277777777777778, 0.00833333333333333, 0.0888888888888889,
          0.041025641025641, 0.237096774193548, 0.159210526315789, 0.105733082706767,
          0.0666666666666667, 0.533333333333333, 3.155e-31, 0, 0.0241935483870968,
          NA, 0.0142857142857143, 0.00793650793650794,
          0.533333333333333, 0.196428571428571, 0.533333333333333, 0.127272727272727,
          0.25, 0.447089947089947, 0.735294117647059, 0.311481481481481, 0.519230769230769),
        dim = c(25L, 1L),
        dimnames = list(expected.rownames, "R-Squared")
    )
    expected.pvalues <- array(
        c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.156019171716493,
          1, 0.936358871997522),
        dim = c(25L, 1L),
        dimnames = list(expected.rownames, "p")
    )
    expected.colnames <- c("Cluster 1 47% n: 10", "Cluster 2 53% n: 10", "R-Squared", "p")
    result <- MultipleMeans(outcomes, predictor, return.data.frame = TRUE)
    expect_is(result, "matrix")
    expect_equal(dimnames(result), list(expected.rownames, expected.colnames))
    # Means equal
    expect_equal(result[, 1:2, drop = FALSE], expected.means)
    # R-squared equal, instability in internal cor function. Run below with do.call(corr, args)
    # na.args <- list(x = c(`2` = 4.6, `3` = 4.6, `6` = 4.6, `8` = 4.6, `9` = 4.6,
    #    `13` = 4.6, `14` = 4.6, `15` = 4.6, `18` = 4.6, `19` = 4.6),
    #    y = c(4L, 5L, 4L, 5L, 4L, 5L, 5L, 4L, 5L, 5L))
    #
    #
    #small.args <- list(x = c(`1` = 4.83333333333333, `4` = 4.83333333333333, `5` = 4.83333333333333,
    #`8` = 4.83333333333333, `9` = 4.83333333333333, `10` = 4.83333333333333,
    #`11` = 4.83333333333333, `12` = 4.83333333333333, `13` = 4.83333333333333,
    #`14` = 4.83333333333333, `16` = 4.83333333333333, `19` = 4.83333333333333
    #), y = c(5L, 5L, 5L, 5L, 5L, 5L, 5L, 4L, 5L, 5L, 5L, 4L))
    unstable.idx <- c(11L, 14L)
    expect_equal(result[-unstable.idx, 3, drop = FALSE], expected.r.square[-unstable.idx, 1, drop = FALSE])
    unstable.r.square <- result[unstable.idx, 3]
    expect_true(all(is.na(unstable.r.square) | abs(unstable.r.square) < 1e-14))
    # p-values equal
    expect_equal(result[, 4, drop = FALSE], expected.pvalues)
})
