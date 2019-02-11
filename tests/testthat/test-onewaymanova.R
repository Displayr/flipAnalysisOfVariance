context("One-Way MANOVA")
data(colas, package = "flipExampleData")
z <- unclass(colas$q4a)
flipFormat::Labels(z) <- "Like Coca-Cola"
colas$like.coke <- z - 3
colas$balanced <- c(rep(1:3, rep(100,3)), rep(NA, 27))
colas$agenumeric <- car::recode(colas$d1, as.factor = FALSE, recodes = "'18 to 24' = 21; '25 to 29' = 27; '30 to 34' = 32; '35 to 39' = 37; '40 to 44' = 42; '45 to 49' = 47; '50 to 54' = 52; '55 to 64' = 60; '65 or more' = 77")
colas$d1MISSING <- colas$d1
colas$like.cokeMISSING <-  colas$like.coke
set.seed(123)
colas$d1MISSING[runif(length(colas$d1MISSING)) > .75] <- NA
colas$like.cokeMISSING[runif(length(colas$d1MISSING)) > .75] <- NA

test_that("MANOVA",{
        # Pillai's trace.
        op <- options(contrasts = c("contr.helmert", "contr.poly"))
        npk2 <- within(npk, foo <- rnorm(24))
        npk2.aov <- manova(cbind(yield, foo) ~ block, npk2)
        dog <- npk2$yield
        attr(dog, "question") <- "Soloman"
        z <- suppressWarnings(OneWayMANOVA(data.frame(dog, npk2$foo), npk2$block, show.labels = TRUE, pillai = TRUE, return.all = TRUE))
        expect_equal(summary(npk2.aov)$stats[1,6], z$manova$stats[1,6])
        # binary.
        z <- suppressWarnings(OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke, return.all = TRUE), colas$d1, binary = FALSE, show.labels = TRUE))
        z1 <- OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1, binary = TRUE, show.labels = TRUE, return.all = TRUE)
        expect_true(length(z$anovas) < length(z1$anovas))
        # show.labels
        z <- OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1, binary = TRUE, show.labels = TRUE, return.all = TRUE)
        expect_equal(z$title,"MANOVA: Age")
        expect_equal(names(z$anovas)[5], "Gender: Female")
        z <- OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1, binary = TRUE, show.labels = FALSE, return.all = TRUE)
        expect_equal(names(z$anovas)[4], "colas.q4b.5")
        # F P-Value
        z <- OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1, binary = TRUE, show.labels = TRUE, fdr = FALSE, return.all = TRUE, pillai = TRUE)
        z1 <- suppressWarnings(OneWayANOVA(colas$like.coke, colas$d1, return.all = TRUE))
        expect_equal(z$anovas[[6]]$p, z1$p)
        # Pillai with weights
        set.seed(123)
        wgt <- runif(327)
        wgt[runif(327) < .25] <- 0
        wgt[runif(327) < .1] <- NA
        expect_error(suppressWarnings(OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1, binary = TRUE, show.labels = TRUE, fdr = FALSE, return.all = TRUE, pillai = TRUE, weights = wgt)), NA)
        # Correction - FDR
        z <- OneWayMANOVA(data.frame(colas$like.coke), colas$d1, binary = TRUE, show.labels = TRUE, fdr = TRUE, return.all = TRUE)
        z1 <- OneWayANOVA(colas$like.coke, colas$d1, correction = "False Discovery Rate", compare = "To mean", return.all = TRUE)
        expect_equal(z$anovas[[1]]$coefs[2,4], z1$coefs[2, 4])
        # Robust.se
        z <- OneWayMANOVA(data.frame(colas$like.coke), colas$d1, binary = TRUE, show.labels = TRUE, fdr = TRUE, return.all = TRUE, robust.se = FALSE)
        z1 <- OneWayMANOVA(data.frame(colas$like.coke), colas$d1, binary = TRUE, show.labels = TRUE, fdr = TRUE, return.all = TRUE, robust.se = TRUE)
        expect_true(z$anovas[[1]]$coefs[2,3] != z1$anovas[[1]]$coefs[2,3])
        # Missing
        m <- "Error if missing data"
        expect_error(OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1, missing = m, binary = TRUE, return.all = TRUE, show.labels = TRUE), NA)
        expect_error(OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1MISSING, missing = m, binary = TRUE, return.all = TRUE, show.labels = TRUE))
        expect_error(OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.cokeMISSING), colas$d1, missing = m, binary = TRUE, return.all = TRUE, show.labels = TRUE))
        expect_error(OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.cokeMISSING), colas$d1MISSING, missing = m, binary = TRUE, return.all = TRUE, show.labels = TRUE))
        # p.cutoff
        z <- suppressWarnings(OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1, p.cutoff = 0.5, return.all = TRUE))
        expect_equal(z$subtitle, "Significant - Smallest p-value (after applying False Discovery Rate correction): 0.35")
        z <- suppressWarnings(OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1, p.cutoff = 0.05, return.all = TRUE, pillai = TRUE))
        expect_equal(z$subtitle, "Not significant - Pillai's Trace: 0.0764, approximate p-value: 0.41")
        # Weights
        y <- colas$like.coke
        attr(y, "name") <- "q2a"
        attr(y, "question") <- "Q2 - Liking"
        attr(y, "label") <- "Coca-Cola"
        x <- colas$d1
        attr(x, "name") <- "d1"
        attr(x, "question") <- "D1. Age"
        attr(x, "label") <- "D1. Age"
        wgt <- as.numeric(x != "25 to 29")
        expect_error(suppressWarnings(OneWayMANOVA(data.frame(y, colas$d3, colas$like.coke), colas$d1, weights = wgt, show.labels = TRUE, return.all = TRUE)), NA)
        expect_error(suppressWarnings(OneWayMANOVA(data.frame(colas$d3, colas$like.coke), colas$d1, weights = wgt, show.labels = TRUE, return.all = TRUE)), NA)
})


test_that("DS-2345 MANOVA with missing data",
{
    f1 <- system.file("tests", "testthat", "manovaSegments.rda",
                      package = "flipAnalysisOfVariance")
    f2 <- system.file("tests", "testthat", "manovaDat.rda",
                      package = "flipAnalysisOfVariance")
    load(f1)
    load(f2)
    QFilter <- TRUE
    QPopulationWeight <- NULL
    formRobust <- FALSE
    formMissing <- "Exclude cases with missing data"
    formNames <- formBinary <- FALSE

    expect_warning(manova <- OneWayMANOVA(dat,
        segmentsQMXJK,
        subset = QFilter,
        weights = QPopulationWeight,
        robust.se = formRobust,
        missing = formMissing,
        show.labels = !formNames,
        binary = formBinary,
        pillai = FALSE,
      fdr = TRUE), "Data has been automatically")
    expect_is(manova, "OneWayMANOVA")
})
