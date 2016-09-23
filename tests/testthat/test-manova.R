context("MANOVA")
data(colas, package = "flipExampleData")


# OneWayMANOVA(colas[, c("q4a", "q4b", "q4c", "q4d", "q4e", "q4f")], colas$d1, binary = TRUE)
#
#
# z <- flipTransformations::AsNumeric(colas[, c("q4a", "q4b", "q4c", "q4d", "q4e", "q4f")], binary = TRUE)
# head(z)
# z <- as.matrix(z)
# lm(z ~ colas$d1)
#
#
# z <- unclass(colas$q4a)
# attr(z, "label") <- "Like Coca-Cola"
# z[z == 1] <- 9
# z[z == 2] <- 1
# z[z == 9] <- 2
# colas$like.coke <- z
# colas$q4b <- unclass(colas$q4b)
# colas$q4c <- unclass(colas$q4c)
# colas$q4d <- unclass(colas$q4d)
# colas$q4e <- unclass(colas$q4e)
# colas$q4f <- unclass(colas$q4f)
#
# z <- as.matrix(cbind(colas$like.coke, colas$q4b, colas$q4c, colas$q4d, colas$q4e, colas$q4f))
# c.manova <- lm(z ~ d1,  data=colas)
# summary(manova(c.manova))
# summary(c.manova)
#
#
#
# c.manova <- lm(cbind(like.coke, q4b, q4c, q4d, q4e, q4f) ~ d1,  data=colas)
# summary(manova(c.manova))
# summary(c.manova)
#
#
# Manova(soils.mod)
# summary(Anova(soils.mod), univariate=TRUE, multivariate=FALSE,
#         p.adjust.method=TRUE
#
#
#         soils.mod <- lm(cbind(pH,N,Dens,P,Ca,Mg,K,Na,Conduc) ~ Block + Contour*Depth,
#                         data=Soils)
#
#
#         soils.mod <- lm(cbind(pH,N,Dens,P,Ca,Mg,K,Na,Conduc) ~ Block + Contour*Depth,
#                         data=Soils)
#
#
#         z = OneWayANOVA(colas$like.coke, colas$d1, compare = "Cells")
