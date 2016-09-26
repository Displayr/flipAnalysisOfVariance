context("manova")
data(colas, package = "flipExampleData")
z <- unclass(colas$q4a)
#z[z == 4] <- 9
#z[z == 5] <- 4
#z[z == 9] <- 5
flipFormat::Labels(z) <- "Like Coca-Cola"
colas$like.coke <- z - 3
colas$balanced <- c(rep(1:3, rep(100,3)), rep(NA, 27))
colas$agenumeric <- car::recode(colas$d1, as.factor.result = FALSE, recodes = "'18 to 24' = 21; '25 to 29' = 27; '30 to 34' = 32; '35 to 39' = 37; '40 to 44' = 42; '45 to 49' = 47; '50 to 54' = 52; '55 to 64' = 60; '65 or more' = 77")
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
        z <- OneWayMANOVA(cbind(npk2$yield, npk2$foo), npk2$block)
        expect_equal(summary(npk2.aov)$stats[1,6], z$manova$stats[1,6])
        # binary.
        z <- OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1, binary = FALSE, show.labels = TRUE)
        z1 <- OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1, binary = TRUE, show.labels = TRUE)
        expect_true(length(z$anovas) < length(z1$anovas))
        # show.labels
        z <- OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1, binary = TRUE, show.labels = TRUE)
        expect_equal(z$title,"MANOVA: Age")
        expect_equal(names(z$anovas)[5], "Gender: Female")
        z <- OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1, binary = TRUE, show.labels = FALSE)
        expect_equal(z$title, "MANOVA: colas$d1")
        expect_equal(names(z$anovas)[4], "colas.q4b.5")
        # F P-Value
        z <- OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1, binary = TRUE, show.labels = TRUE)
        z1 = OneWayANOVA(colas$like.coke, colas$d1)
        expect_equal( z$anovas[[6]]$p, z1$p)
        # t p-value
        z <- OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1, binary = TRUE, show.labels = TRUE)
        z1 = OneWayANOVA(colas$like.coke, colas$d1, compare = "To mean")
        expect_equal(z$anovas[[6]]$coefs[2,4], z1$coefs[2, 4])
        # Correction
        z <- OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1, binary = TRUE, show.labels = TRUE, correction = "Bonferroni")
        z1 = OneWayANOVA(colas$like.coke, colas$d1, correction = "Bonferroni", compare = "To mean")
        expect_equal(z$anovas[[6]]$coefs[2,4], z1$coefs[2, 4])
        # Missing
        m <- "Error if missing data"
        expect_error(OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1, missing = m, binary = TRUE, show.labels = TRUE), NA)
        expect_error(OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1MISSING, missing = m, binary = TRUE, show.labels = TRUE))
        expect_error(OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.cokeMISSING), colas$d1, missing = m, binary = TRUE, show.labels = TRUE))
        expect_error(OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.cokeMISSING), colas$d1MISSING, missing = m, binary = TRUE, show.labels = TRUE))
        # p.cutoff
        z <- OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1, p.cutoff = 0.5)
        expect_equal(z$subtitle, "Significant: Pillai's Trace: 0.0764, approximate p-value: 0.41")
        z <- OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1, p.cutoff = 0.05)
        expect_equal(z$subtitle, "Not significant: Pillai's Trace: 0.0764, approximate p-value: 0.41")
})
