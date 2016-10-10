#' #' BootstrappedR2
#' #'
#' #' Tests for a difference in means on one or more variables by a group variable.
#' #' @param outcomes A numeric matrix-like object.
#' #' @param predictor The factor representing the groups.
#' #' @param subset An optional vector specifying a subset of observations to be
#' #'   used in the fitting process, or, the name of a variable in \code{data}. It
#' #'   may not be an expression. \code{subset} may not
#' #' @param n.bootraps The number of bootstrap samples.
#' #' @param weights An optional vector of sampling weights, or, the name or, the
#' #'   name of a variable in \code{data}. It may not be an expression.
#' #' @param center Either a logical value or a numeric vector of length equal to the number of columns of outcome.
#' #' @param scale Either a logical value or a numeric vector of length equal to the number of columns of outcome.
#' #' @details Where there is only one outcome variable,  \code{scale} will have no efect.
#' #' With two or more variables,  scaling all variables to have the same scale will make the test be one of testin for the
#' #' average r-squared. \code{center} helps w
#' ith numerical precision (as can \code{scale}).
#' #' @importFrom flipStatistics StatisticsByGroup Mean StandardDeviation OmegaSquared
#' #' @export
#' BootstrappedR2 <- function(outcomes, predictor, subset = NULL, weights = NULL, n.boostraps = 999, center = TRUE, scale = TRUE)
#' {
#'     # Cleaning data.
#'     if (!is.data.frame(outcomes))
#'         outcome <- data.frame(outcomes)
#'     outcomes <- scale(outcomes, center, scale)
#'     if (is.null(weights))
#'         weights <- rep(1, (n <- length(predictor)))
#'     sub <- if (is.null(subset)) weights > 0 else subset & weights > 0
#'     outcomes <- subset(outcomes, sub)
#'     predictor <- subset(predictor, sub)
#'     weights <- subset(weights, sub)
#'     predictor <- predictor[sub]
#'     groups <- unclass(predictor)
#'     levels <- levels(predictor)
#'     # Statisics
#'     n <- length(predictor)
#'     grand.mean <- Mean(outcome, weights)
#'     means <- StatisticsByGroup(outcomes, predictor, weights, Mean)
#'     sds <- StatisticsByGroup(outcomes, predictor, weights, StandardDeviation)
#'     means.i <- means[groups]
#'     residuals <- sweep(outcomes, 2, grand.mean, "-")
#'     sds.i <- sds[groups]
#'     st.residuals <- residuals / sds.i
#'     o2 <- OmegaSquared(outcomes, predictor, weights)
#'     # boostrapping
#'     b.o2s <- rep(NA, n.boostraps)
#'     for (b in 1:n.boostraps)
#'     {
#'         smpl <- sample.int(n, n, replace = TRUE)#prob = weights,
#'         b.st.residuals <- st.residuals[smpl, , drop = FALSE]
#'         b.residuals <- sweep(b.st.residuals, 1, sds.i, "*")
#'         # Implicitly mean centering by using the residuals as the outcome
#'         b.o2s[b] <- OmegaSquared(b.residuals, predictor, weights)
#'
#'     }
#'     list(omega.squared = o2, p = sum(b.o2s >= o2) / n.boostraps)
#' }
#'
