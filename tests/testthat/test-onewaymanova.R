
library(colas, package = "flipExampleData")
z = OneWayMANOVA(colas[, c("q4a", "q4b", "q4c", "q4d", "q4e", "q4f")], colas$d1, binary = FALSE)
z

# survey::svyglm(q4a~d1, data = colas, design = flipRegression::weightedSurveyDesign(colas, runif(327)))
#
#
# data(api)
# sdesign <- svydesign(id=~1, weights=~wgt, data=colas)
# model0 <- svyglm(like.coke ~ 1, design = sdesign)
# model1 <- svyglm(like.coke ~ d2, design = sdesign)
# anova(model0, model1)
#     regTermTest(model0)
#
#
# set.seed(23)
# x <- rep(1:3, c(10, 10, 10))
# y <- rnorm(30, mean = x)
# w <- (1:30) / 10
# df <- data.frame(x, y, w)
# my.design <- svydesign(id=~1, weights= ~w, data = df)
# model.1 <- svyglm(y ~ x, design = my.design)
# regTermTest(model.1, "x")
# summary(aov(glm(y ~ x, data = df)))
#
#
#
# summary(aov(model.1))
#
#
#
#
# a
#
# model.0 <- svyglm(y ~ 1, design = my.design)
#
#
# anova(model.0, model.1)
#
#
# anova
#
# summary(lm(like.coke ~ d2, data = colas))
#
#     regTermTest(model0)
#
# data(api)
# dclus2<-svydesign(id=~1, weights=~pw, data=apiclus2)
# model0<-svyglm(I(sch.wide=="Yes")~ell, design=dclus2)
# anova(model0)
