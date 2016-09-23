
#' @importFrom flipTransformations Factor
tidyFactor <- function(x)
{
    x <- Factor(x)
    levels(x)[table(x) == 0] <- NA # Same basic idea as droplevels, except that it retains the attribute 'label'.
    x
}
