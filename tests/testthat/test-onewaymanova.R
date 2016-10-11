context("One-Way MANOVA")
data(colas, package = "flipExampleData")
z <- unclass(colas$q4a)
flipFormat::Labels(z) <- "Like Coca-Cola"
colas$like.coke <- z - 3
colas$balanced <- c(rep(1:3, rep(100,3)), rep(NA, 27))
colas$agenumeric <- car::recode(colas$d1, as.factor.result = FALSE, recodes = "'18 to 24' = 21; '25 to 29' = 27; '30 to 34' = 32; '35 to 39' = 37; '40 to 44' = 42; '45 to 49' = 47; '50 to 54' = 52; '55 to 64' = 60; '65 or more' = 77")
colas$d1MISSING <- colas$d1
colas$like.cokeMISSING <-  colas$like.coke
set.seed(123)
colas$d1MISSING[runif(length(colas$d1MISSING)) > .75] <- NA
colas$like.cokeMISSING[runif(length(colas$d1MISSING)) > .75] <- NA


library(flipAnalysisOfVariance)
manova <- OneWayMANOVA(data.frame(colas$q4a, colas$q4c, colas$q4b, colas$q4d, colas$q4e, colas$q4f),
    colas$d1)



test_that("MANOVA",{
        # Pillai's trace.
        op <- options(contrasts = c("contr.helmert", "contr.poly"))
        npk2 <- within(npk, foo <- rnorm(24))
        npk2.aov <- manova(cbind(yield, foo) ~ block, npk2)
        dog <- npk2$yield
        attr(dog, "question") <- "Soloman"
        z <- OneWayMANOVA(data.frame(dog, npk2$foo), npk2$block, show.labels = TRUE, pillai = TRUE)
        expect_equal(summary(npk2.aov)$stats[1,6], z$manova$stats[1,6])
        # binary.
        z <- suppressWarnings(OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1, binary = FALSE, show.labels = TRUE))
        z1 <- OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1, binary = TRUE, show.labels = TRUE)
        expect_true(length(z$anovas) < length(z1$anovas))
        # show.labels
        z <- OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1, binary = TRUE, show.labels = TRUE)
        expect_equal(z$title,"MANOVA: Age")
        expect_equal(names(z$anovas)[5], "Gender: Female")
        z <- OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1, binary = TRUE, show.labels = FALSE)
        expect_equal(names(z$anovas)[4], "colas.q4b.5")
        # F P-Value
        z <- OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1, binary = TRUE, show.labels = TRUE, fdr = FALSE)
        z1 <- suppressWarnings(OneWayANOVA(colas$like.coke, colas$d1))
        expect_equal(z$anovas[[6]]$p, z1$p)
        # t p-value - no correction
        z <- OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1, binary = TRUE, show.labels = TRUE, fdr = FALSE)
        z1 = OneWayANOVA(colas$like.coke, colas$d1, compare = "To mean", correction = "None")
        expect_equal(z$anovas[[6]]$coefs[2,4], z1$coefs[2, 4])
        # Correction - FDR
        z <- OneWayMANOVA(data.frame(colas$like.coke), colas$d1, binary = TRUE, show.labels = TRUE, fdr = TRUE)
        z1 <- OneWayANOVA(colas$like.coke, colas$d1, correction = "False Discovery Rate", compare = "To mean")
        #cbind(z$anovas$[[1]]$coefs[, 4], z1$coefs[, 4])
        expect_equal(z$anovas[[1]]$coefs[2,4], z1$coefs[2, 4])
        # Missing
        m <- "Error if missing data"
        expect_error(OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1, missing = m, binary = TRUE, show.labels = TRUE), NA)
        expect_error(OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1MISSING, missing = m, binary = TRUE, show.labels = TRUE))
        expect_error(OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.cokeMISSING), colas$d1, missing = m, binary = TRUE, show.labels = TRUE))
        expect_error(OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.cokeMISSING), colas$d1MISSING, missing = m, binary = TRUE, show.labels = TRUE))
        # p.cutoff
        z <- suppressWarnings(OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1, p.cutoff = 0.5))
        expect_equal(z$subtitle, "Significant - Smallest p-value (after applying False Discovery Rate correction): 0.35")
        z <- suppressWarnings(OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1, p.cutoff = 0.05, pillai = TRUE))
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
        expect_error(suppressWarnings(OneWayMANOVA(data.frame(y, colas$d3, colas$like.coke), colas$d1, weights = wgt, show.labels = TRUE)))
        expect_error(suppressWarnings(OneWayMANOVA(data.frame(colas$d3, colas$like.coke), colas$d1, weights = wgt, show.labels = TRUE)), NA)
})
