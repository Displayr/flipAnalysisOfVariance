# context("Tests that do not test properly due to confusing testthat")
# data(colas, package = "flipExampleData")
# z <- unclass(colas$q4a)
# #z[z == 4] <- 9
# #z[z == 5] <- 4
# #z[z == 9] <- 5
# flipFormat::Labels(z) <- "Like Coca-Cola"
# colas$like.coke <- z - 3
# colas$balanced <- c(rep(1:3, rep(100,3)), rep(NA, 27))
# colas$agenumeric <- car::recode(colas$d1, as.factor = FALSE, recodes = "'18 to 24' = 21; '25 to 29' = 27; '30 to 34' = 32; '35 to 39' = 37; '40 to 44' = 42; '45 to 49' = 47; '50 to 54' = 52; '55 to 64' = 60; '65 or more' = 77")
# colas$d1MISSING <- colas$d1
# colas$like.cokeMISSING <-  colas$like.coke
# set.seed(123)
# colas$d1MISSING[runif(length(colas$d1MISSING)) > .75] <- NA
# colas$like.cokeMISSING[runif(length(colas$d1MISSING)) > .75] <- NA
#
# z <- OneWayANOVA(colas$like.coke, colas$d1MISSING, show.labels = FALSE, compare = "To first")
# if (z$title !=  "One-way ANOVA: colas$like.coke by colas$d1MISSING")
#     print("Oops")
#
# z <- OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1, binary = TRUE, show.labels = FALSE)
# if (z$title !=  "MANOVA: colas$d1")
#     print("Oops")
#
