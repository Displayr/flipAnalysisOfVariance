Y <- flipTransformations::AsNumeric(colas[, c("q4a", "q4b", "q4c", "q4d", "q4e", "q4f")], binary = FALSE)
Y <- as.matrix(Y)
Y <- matrix(rnorm(327*6), 327)
mod <- lm(as.matrix(Y) ~ colas$d1, weights = runif(327))
mod <- lm(as.matrix(Y) ~ colas$d1)#, weights = runif(327))
r <- resid(mod)
fit <- fitted(mod)

b.weights <- function(weights)
{
    n <- length(weights)
    s <- table(sample(1:n, n - 1, replace = TRUE))
    m <- rep(0.00000000001, n) # Very small value to ensure degrees of freedom are not stuffed up in other models.
    m[as.integer(names(s))] <- s
    b.w <- weights * m
    b.w / sum(b.w) * sum(weights)
}

result <- NULL
n <- nrow(Y)
wgt <- rep(1, n)
x <- colas$d1
for (i in 1:1000)
{
    b.Y <- r[sample(1:n, n, replace = TRUE), ] + fit
    g <- b.weights(wgt)
    b.mod <- lm(b.Y ~ x, weights = )
    pillai <- summary(manova(b.mod))$stats[1, 2]
    result[i] <- pillai
}
asymptotic <- summary(manova(mod))
asymptotic
sum(result > asymptotic$stats[1,2])
hist(result)


    bootstrap.pillai
b.weights(rep(1,10))
