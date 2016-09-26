context("CompareMeans")
data(colas, package = "flipExampleData")
colas$like.coke <- unclass(colas$q4a)
attr(colas$like.coke, "label") <- "Like Coca-Cola"

test_that("Bug: System is computationally singular.CE-626 ",
          {
                data(colas, package = "flipExampleData")
                z <- data.frame(colas$Q5_5_1, colas$Q5_7_1)
                dep <- z[,1]
                ind <- colas$q3
                wgt <- unclass(colas$q7)
                expect_error({
                    suppressWarnings(flipRegression::Regression(dep ~ ind, weigts = wgt))
                    suppressWarnings(OneWayANOVA(dep, ind, weigts = wgt))
                }, NA)
                expect_error(CompareMultipleMeans(dep, ind, weights = wgt))

          })


test_that("Print", {
     data(cola, package = "flipExampleData")
    z <- list(cola$Q4_A, cola$Q4_B, cola$Q4_C)
    zz <- CompareMultipleMeans(z, cola$Q3, title = "Title", subtitle = "Sub title", footer = "footer")
    expect_error(print(zz), NA)

    expect_error((print(CompareMultipleMeans(list(A = cola$Q4_A, B = cola$Q4_B, C = cola$Q4_C), cola$Q3))), NA)
    # Weights.
     expect_error((print(CompareMultipleMeans(list(A = cola$Q4_A, B = cola$Q4_B, C = cola$Q4_C), cola$Q3, weights = 10 * runif(length(cola$Q3))))), NA)

    data(bank, package = "flipExampleData")
    expect_error(suppressWarnings(CompareMultipleMeans(list(bank$Fees, bank$Branch, bank$ATM), bank$Overall, compare = "Columns")), NA)


    library(flipAnalysisOfVariance)
    data(cola, package = "flipExampleData")
    CompareMultipleMeans(list(cola$Q4_A, cola$Q4_B, cola$Q4_C), cola$Q3, compare = "Columns")

})


#
# context("LDA")
# library(flipMultivariates)
# data(hbatwithsplits, package = "flipExampleData")
# hair <- hbatwithsplits
#
# hair1  <- flipTransformations::AsNumeric(hair[, paste0("x",6:18)], binary = FALSE, remove.first = TRUE)
# hair1$x1 <- hair$x1
# hair1$split60 <- hair$split60
# hair1$id <- hair$id
#
# LDA(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18, method = "moment", data = hair1, subset = split60 == "Estimation Sample", show.labels = FALSE)
#
#
#
# library(multcomp)
# mod <- lm(breaks ~ wool + tension, data = warpbreaks)
# K1 <- glht(mod, mcp(wool = "Tukey"))$linfct
# K2 <- glht(mod, mcp(tension = "Tukey"))$linfct
#
# summary(glht(mod, linfct = rbind(K1, K2)))
#
# # Two way ANOVA - paired comparisons
# mod <- flipRegression::Regression(like.coke ~ d1 + d3, data = colas)$original
# K1 <- glht(mod, mcp(d1 = "Tukey"))$linfct
# K2 <- glht(mod, mcp(d3 = "Tukey"))$linfct
# summary(glht(mod, linfct = rbind(K1, K2)))
#
# ## Two way ANOVA - within second variable - paired comparisons
# # Method 1
# mod <- flipRegression::Regression(like.coke ~ d1 * d3 , data = colas)$original
# tmp <- expand.grid(d1 = unique(colas$d1), d3 = unique(colas$d3))
# X <- model.matrix(~ d1 * d3, data = tmp)
# glht(mod, linfct = X)
#
#
# predict(mod, newdata = tmp)
#
# Tukey <- contrMat(table(colas$d1), "Tukey")
# K1 <- cbind(Tukey, matrix(0, nrow = nrow(Tukey), ncol = ncol(Tukey)))
# rownames(K1) <- paste(levels(colas$d3)[1], rownames(K1), sep = ":")
# K2 <- cbind(matrix(0, nrow = nrow(Tukey), ncol = ncol(Tukey)), Tukey)
# rownames(K2) <- paste(levels(colas$d3)[2], rownames(K2), sep = ":")
# K <- rbind(K1, K2)
# colnames(K) <- c(colnames(Tukey), colnames(Tukey))
#
#
# summary(glht(mod, linfct = K %*% X), adjusted(type = "none"))
#  K %*% predict(mod, newdata = tmp)
#
#
#
#
#
#
# colas$d1d3 <- with(colas, interaction(d1, d3))
# cell <- flipRegression::Regression(like.coke ~ d1d3 - 1, data = colas)$original
# K1 <- glht(mod, mcp(d1 = "Tukey"))$linfct
# K2 <- glht(mod, mcp(d3 = "Tukey"))$linfct
# summary(glht(mod, linfct = rbind(K1, K2)))
# summary(glht(cell, linfct = K))
#
#
#
