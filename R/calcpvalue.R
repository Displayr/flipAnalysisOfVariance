#' @importFrom stats pt qnorm pchisq
#' @importFrom survey svydesign svyranktest
#' @importFrom stats kruskal.test
calcPvaluesForOneVariable <- function(x, group, weights, is.binary = FALSE, non.parametric = FALSE)
{
    if (!is.factor(group))
        group <- factor(group)
    n.levels <- nlevels(group)
    levs <- levels(group)

    pval <- rep(NA, n.levels)
    if (n.levels < 2)
        return(pval)
    for (i in 1:n.levels)
    {
        y <- group == levs[i]
        if (non.parametric && !is.binary)
        {
            if (is.null(weights))
                pval[i] <- kruskal.test(x, y)$p.value
            else
            {
                df <- data.frame(x = x, y = y, w = weights)
                pval[i] <- svyranktest(x ~ y, svydesign(ids = ~1, weights = ~w, data = df))$p.value
            }
        }
        else 
            pval[i] <- calcPvaluesForOneVarOneLevel(x, x.is.binary = is.binary, y = y, w = weights)
    }
    return(pval)
}

calcPvaluesForOneVarOneLevel = function(x,             # A binary or numeric variable
                      x.is.binary = TRUE,       # TRUE if x is a binary variable
                      y,                        # A binary variable
                      w = rep(1, length(x)))    # weight variable (same length as x)
{
    if (length(w) <= 1)
        w <- rep(1, length(x))

    Filter = function(x, f)
    {
        if (is.null(f))
            return(NULL)
        x[f]
    }

    # Identifying missing values; these are values that are:
    # - Missing in  x (e.g., if x is Pick Any and x = Coke | Pepsi, 
    #       if either coke or Pepsi have missing values, then x is missing.
    # - Missing in y
    # - Missing or <= 0 in w
    m = is.na(x) | if(is.null(y)) FALSE else is.na(y) | is.na(w) | w <= 0
    x = Filter(x, !m)
    y = Filter(y, !m)
    w = Filter(w, !m)

    filters = list(which(y == 1), which(y == 0))
    a = computeNumericVarStats(Filter(x, filters[[1]]), Filter(w, filters[[1]]))
    b = computeNumericVarStats(Filter(x, filters[[2]]), Filter(w, filters[[2]]))

    # Here, the test.type for SegmentComparisonTable is determined based on
    # variable type but we will update this later to use the statistical
    # assumptions in QSettings
    # Need to update function signature and tests 
    test.type <- if (!x.is.binary) "tTest" else "Nonparametric"
    return(compareTwoSamples(test.type, a, b, x.is.binary,
           is.weighted = length(unique(w)) > 1, bessel = 0))
}

# a and b are lists which contain the summary statistics of each sample
compareTwoSamples <- function(test.type, a, b,
    is.binary = TRUE, is.weighted = FALSE, bessel = 0, dEff = 1)
{
    if (test.type == "tTest")
        return(tTest(a[["Average"]], b[["Average"]], a[["Standard Error"]],
            b[["Standard Error"]], a[["Base n"]], b[["Base n"]],
            is.binary, is.weighted, bessel, dEff))
    else if (test.type == "zTest")
        return(zTest(a[["Average"]], b[["Average"]], a[["Standard Error"]],
            b[["Standard Error"]], a[["Base n"]], b[["Base n"]],
            is.binary, is.weighted, bessel))
    else # Nonparametric or Chisquare
        return(raoScottSecondOrderChiSquareTest(a, b, is.weighted))
    # Non-parametric tests for numeric variables are handled before
    # getting to this function
} 
# Functions - these are all from the c# SamplingVariance class (albeit in slightly different forms)
computeVariances <- function(mean, is.binary, sum.w, sum.ww, sum.xw, sum.xww, sum.xxw, sum.xxww, n.observations)
{
    if (is.binary) # Numerical precision
    {
        if (mean < 0.00000001)
            mean = 0
        else if (mean > 0.99999999)
            mean = 1
    }
    bessel.correction = n.observations / (n.observations - 1)
    mean2 = mean * mean
    sum_of_squares = sum.xxw - 2 * mean * sum.xw + mean2 * sum.w
    sum_of_squares.w = sum.xxww - 2 * mean * sum.xww + mean2 * sum.ww

    taylor = sum_of_squares.w / (sum.w * sum.w) * bessel.correction

    naive = if (is.binary) mean * (1 - mean) else sum_of_squares / sum.w
    naive = naive * bessel.correction / n.observations
    ess = if (!is.na(taylor) && taylor < 0.000001) # Due to numeric precision issues
        sum.w * sum.w / sum.ww
        else n.observations * naive / taylor

    list(taylor = taylor,
         naive = naive,
         ess = ess,
         se = sqrt(taylor))
}

# A simplification of RaoScottSecondOrder2b2 from Q's C#
# aa and bb contain summary statistics for each sample
raoScottSecondOrderChiSquareTest <- function(aa, bb, is.weighted)
{
    if (!is.null(dim(aa[["Average"]])))
        pvals <- matrix(NA, nrow = NROW(aa[["Average"]]), ncol = NCOL(aa[["Average"]]))
    else
        pvals <- rep(NA, length = length(aa[["Average"]]))
    if (length(aa[["Average"]]) == 0)
        return(pvals)

    for (i in 1:length(aa[["Average"]]))
    {
        n.observations <- aa[["Base n"]][i] + bb[["Base n"]][i]
        if (is.weighted)
        {
            sum.w <- aa[["sumW"]][i] + bb[["sumW"]][i]
            sum.ww <- aa[["sumWW"]][i] + bb[["sumWW"]][i]
            p.a <- aa[["sumW"]][i] / sum.w
        } else
            p.a <- aa[["Base n"]][i] / n.observations
        p.b <- 1 - p.a

        proportiony = p.a
        mean.a <- aa[["Average"]][i]
        if (is.na(mean.a))
            mean.a <- 0
        mean.b <- bb[["Average"]][i]
        if (is.na(mean.b))
            mean.b <- 0

        sums.ww <- c(bb[["sumWW"]][i] - bb[["sumXWW"]][i], bb[["sumXWW"]][i], 
                     aa[["sumWW"]][i] - aa[["sumXWW"]][i], aa[["sumXWW"]][i])
        proportions <- c(p.b * (1-mean.b), p.b * mean.b,
                         p.a * (1-mean.a), p.a * mean.a)
        counts = matrix(proportions * n.observations, 2)

        # If not weighted, this reduces to a chi-square test
        group_sizes = colSums(counts)
        row.totals = rowSums(counts)
        total = sum(row.totals)

        expected = matrix(c(group_sizes[1]*row.totals[1]/total,
                            group_sizes[1]*row.totals[2]/total,
                            group_sizes[2]*row.totals[1]/total,
                            group_sizes[2]*row.totals[2]/total), 2)

        pearson.statistic = sum((counts - expected)^2/expected)
        if (!is.weighted)
            pvals[i] <- pchisq(pearson.statistic, 1, lower.tail = FALSE)
        else
        {
            variance = multinomialCovarianceMatrix(proportions, 
                    sums.ww, sum.ww, sum.w, n.observations)
            if (!is.na(pearson.statistic)) # If not a missing value
            {
                a = matrix(0, 4, 1)
                id_mat = d_mat = matrix(0, 4, 4)
                denominator = 0.0;
                for (j in 1:4)
                {
                    prop = proportions[j];
                    d_mat[j, j] = prop;
                    prop_is_0 = prop < 1e-12
                    j_prop = if (prop_is_0) 0 else 1.0 / prop;
                    if (!prop_is_0) id_mat[j, j] = j_prop;
                    a[j, 1] = j_prop / 4.0;
                    denominator = denominator + j_prop;
                }
                a[2, 1] = -a[2, 1];
                a[3, 1] = -a[3, 1];
                denominator = denominator * .0625 / n.observations;
                numerator = t(a) %*% variance %*% a
                delta = numerator / denominator;
                f = pearson.statistic / delta
            } else
                f <- NA
            pvals[i] <- (1 - pf(f, 1, n.observations - 1))
        }
    }
    return(pvals)
}

# Formulas are for binary variables (proportions):
# https://wiki.q-researchsoftware.com/wiki/Independent_Sample_Tests_-_Comparing_Two_Proportions
# And for numeric variables (means):
# https://wiki.q-researchsoftware.com/wiki/Related_Samples_Tests_-_Comparing_Two_Means
tTest <- function(mean1, mean2, se1, se2, n1, n2,
                  is.binary, is.weighted, bessel = 0, dEff = 1)
{
    if (!is.binary)
    {
        v1 <- se1 * se1
        v2 <- se2 * se2
        v <- v1 + v2
        se <- sqrt(v)
        df <- v * v / (v1 * v1 / (n1 - 1) + v2 * v2 / (n2 - 1))

    } else if (is.binary && !is.weighted)
    {
        # https://wiki.q-researchsoftware.com/wiki/Independent_Samples_T-Test_-_Comparing_Two_Proportions 
        m12 <- (n1 * mean1 + n2 * mean2)/(n1 + n2)
        se <- sqrt(m12 * (1 - m12) * (1/n1 + 1/n2))
        df <- n1 + n2 - 2

    } else if (is.binary && is.weighted)
    {
        # There seems to be some inaccuracy in the p-values
        # after around 5 or 6 decimals with this method
        se <- sqrt(dEff * (se1 * se1 + se2 * se2))
        s1c <- se1 * se1/n1
        s2c <- se2 * se2/n2
        df <- (s1c + s2c)^2 / 
            ((s1c * s1c/(n1-bessel) + (s2c * s2c)/(n2-bessel)))
    }
    t = (mean1 - mean2)/se
    p = pt(-abs(t), df) * 2
    return(p)
}

#' @importFrom stats pnorm
zTest <- function(mean1, mean2, se1, se2, n1, n2, is.binary, is.weighted, bessel = 0)
{
    if (!is.binary)
        se <- sqrt(se1 * se1 + se2 * se2)
    else if(is.binary && !is.weighted)
    {
        m12 <- (n1 * mean1 + n2 * mean2)/(n1 + n2)
        se <- sqrt(m12 * (1 - m12) * (n1 + n2) / (n1 + n2 - 2*bessel) * (1/n1 + 1/n2))
    
    } else if (is.binary && is.weighted)
    {
        se <- sqrt(se1 * se1 + se2 * se2)
    }
    z = (mean1 - mean2)/se
    p = pnorm(-abs(z)) * 2
    return(p)
}


multinomialCovarianceMatrix <- function(proportions, ww, ww_total, w_total, n)
{
    k =length(proportions)
    covariance = matrix(0, k, k)
    for (r in 1:4)
    {
        for (c in 1:4)
        {
            p1 = proportions[r];
            p2 = proportions[c];
            ww1 = ww[r];
            ww2 = ww[c];
            sc = if(r == c) computeSamplingVarianceForProportion(p1, ww1, ww_total, w_total, n)
                 else       samplingCovariance(p1, p2, ww1, ww2, ww_total, w_total, n)
            covariance[c, r] = covariance[r, c] = sc
        }
    }
    return(covariance)
}

computeSamplingVarianceForProportion <- function(input_proportion, ww, ww_total, w_total,sample_size)
{
    proportion = input_proportion
    if (proportion < 1E-8)
        proportion = 0.0;
    if (proportion > 1 - 1e-8)
        proportion = 1.0;
    sumSquaredWeights = ww_total;
    n = sample_size;
    mean = proportion;
    complement = (1.0 - proportion)
    variance = proportion * complement
    population = w_total;
    variance = variance * sample_size/(sample_size - 1.0);
    ww_sums_of_squares = complement * complement * ww + proportion * proportion * (ww_total - ww)
    return(ww_sums_of_squares / (w_total * w_total) * sample_size/(sample_size - 1.0))
}

# From the C# SamplingVariance(double proportion1, double proportion2, double ww1, double ww2, double ww_total, double w_total, int n, StatisticalAssumptions statistical_assumptions)
samplingCovariance <- function(proportion1,  proportion2,  ww1,  ww2,  ww_total,  w_total, n)
{
    ww_sums_of_squares = -proportion1 * -proportion2 * (ww_total - ww1 - ww2) + -proportion1 * (1 - proportion2) * ww2 + -proportion2 * (1 - proportion1) * ww1
    return(ww_sums_of_squares / (w_total * w_total) * (n / (n - 1)))
}

computeNumericVarStats <- function(x, w)
{
    n.observations <- length(x)
    ww = w * w
    xw = x * w
    xxw = x * xw
    xww = xw * w
    xxww = xxw * w
    sum.w = sum(w)
    sum.xw = sum(xw)
    sum.ww = sum(ww)
    sum.xxw = sum(xxw)
    sum.xww = sum(xww)
    sum.xxww = sum(xxww)
    mean.x = sum.xw / sum.w

    n.used.in.bessel.correction = n.observations
    var = computeVariances(mean.x, FALSE, sum.w, sum.ww, sum.xw, sum.xww, sum.xxw, sum.xxww, n.used.in.bessel.correction)

    return(c("Average" = mean.x, "Base n" = n.observations,
        sumW = sum.w, sumWW = sum.ww, sumXWW = sum.xww,
        "Standard Error" = var$se))
}

   
