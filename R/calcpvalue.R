#' @importFrom stats pt qnorm pchisq
#' @importFrom survey svydesign svyranktest
#' @importFrom stats kruskal.test
pvalsByGroup <- function(x, group, weights, is.binary = FALSE, non.parametric = FALSE)
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
            pval[i] <- calcPvalueForVariable(x, x.is.binary = is.binary, y = y, w = weights)
    }
    return(pval)
}

calcPvalueForVariable = function(x,             # A binary or numeric variable
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
    return(compareTwoSamples(test.type, a, b, is.binary
           is.weighted = length(unique(w)) > 1, bessel = 0)
    
    if (!x.is.binary)
        return(independentSamplesTTestMeans(a["Average"], b["Average"], a["Standard Error"], b["Standard Error"], a["Base n"], b["Base n"]))


    # Use a and b only
    n.observations <- a[["Base n"]] + b[["Base n"]]
    sum.w <- a[["sumW"]] + b[["sumW"]]
    sum.ww <- a[["sumWW"]] + b[["sumWW"]]
    p.a <- a[["sumW"]] / sum.w
    p.b <- 1 - p.a

    proportiony = p.a
    mean.a <- a[["Average"]]
    if (is.na(mean.a))
        mean.a <- 0
    mean.b <- b[["Average"]]
    if (is.na(mean.b))
        mean.b <- 0

    sums.ww <- c(b[["sumWW"]] - b[["sumXWW"]], b[["sumXWW"]], 
                 a[["sumWW"]] - a[["sumXWW"]], a[["sumXWW"]])
    proportions <- c(p.b * (1-mean.b), p.b * mean.b,
                     p.a * (1-mean.a), p.a * mean.a)
    counts = matrix(proportions * n.observations, 2)
    variance = multinomialCovarianceMatrix(proportions, 
            sums.ww, sum.ww, sum.w, n.observations)
    p = raoScottSecondOrder2b2(proportions,
                               counts,
                               variance,
                               b[["Base n"]],
                               a[["Base n"]],
                               is.weighted = length(unique(w)) > 1)
    return(p)
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
    #cat("sum.xxww:", sum.xxww, "sum.xww:", sum.xww, "sum.w:", sum.w, "\n") 

    taylor = sum_of_squares.w / (sum.w * sum.w) * bessel.correction

    naive = if (is.binary) mean * (1 - mean) else sum_of_squares / sum.w
    naive = naive * bessel.correction / n.observations
    ess = if (!is.na(taylor) && taylor < 0.000001) # Due to numeric precision issues
        sum.w * sum.w / sum.ww
        else n.observations * naive / taylor

    v1 <- sum_of_squares.w / (sum.w * sum.w)
    v2 <- mean * (1-mean) / n.observations
    #cat("v1:", v1, "v2:", v2, "sum.ww * (v1 - v2):", (v1 - v2) * sum.ww, 
     #   "sum.w:", sum.w, "sum.ww:", sqrt(sum.ww), "\n")
    list(taylor = taylor,
         naive = naive,
         ess = ess,
         se = sqrt(taylor))
}

# A simplification of RaoScottSecondOrder2b2 from Q's C#
raoScottSecondOrder2b2 <- function(proportions,
                       counts,
                       variance,
                       n0,
                       n1,
                       is.weighted)
{
    group_sizes = colSums(counts) # sample size
    row.totals = rowSums(counts)  # sum(sample size * 1 - prop), sum(sample size * prop)
    total = sum(row.totals)
    n = n0 + n1;
    expected = matrix(c(group_sizes[1]*row.totals[1]/total,
                        group_sizes[1]*row.totals[2]/total,
                        group_sizes[2]*row.totals[1]/total,
                        group_sizes[2]*row.totals[2]/total), 2)
    pearson.statistic = sum((counts - expected)^2/expected)
    if (!is.weighted)
        return(pchisq(pearson.statistic, 1, lower.tail = FALSE))

    if (!is.na(pearson.statistic)) # If not a missing value
    {
        a = matrix(0, 4, 1)
        id_mat = d_mat = matrix(0, 4, 4)
        denominator = 0.0;
        for (i in 1:4)
        {
            prop = proportions[i];
            d_mat[i, i] = prop;
            prop_is_0 = prop < 1e-12
            i_prop = if (prop_is_0) 0 else 1.0 / prop;
            if (!prop_is_0) id_mat[i, i] = i_prop;
            a[i, 1] = i_prop / 4.0;
            denominator = denominator + i_prop;
        }
        a[2, 1] = -a[2, 1];
        a[3, 1] = -a[3, 1];
        denominator = denominator * .0625 / n;
        numerator = t(a) %*% variance %*% a
        delta = numerator / denominator;
        f = pearson.statistic / delta
        1 - pf(f, 1, n - 1)
    } else
        f <- NA
    return(1 - pf(f, 1, n - 1))
}

independentSamplesTTestMeans <- function(mean1,
                                        mean2,
                                        standard_error_1,
                                        standard_error_2,
                                        n1,
                                        n2)
{
    .ComputeStandardError <- function(se_1, se_2)
    {
        var1 = se_1 * se_1;
        var2 = se_2 * se_2
        sqrt(var1 + var2)
    }
    .WelchDegreesOfFreedom <- function(se_1, se_2, n_1, n_2)
    {
        var1 = se_1 * se_1;
        var2 = se_2 * se_2;
        (var1 + var2) * (var1 + var2) / (var1 * var1 / (n_1 - 1) + var2 * var2 / (n_2 - 1));
    }
    se = .ComputeStandardError(standard_error_1,  standard_error_2)
    t = (mean1 - mean2) / se
    df = .WelchDegreesOfFreedom(standard_error_1, standard_error_2, n1, n2)
    p = pt(-abs(t), df)  * 2
    #cat(sprintf("p=%.4f, t=%.2f, se=%.3f\n", p, t, se))
    return(p)
}

tTest <- function(mean1, mean2, se1, se2, n1, n2, is.binary, is.weighted, bessel = 0)
{
    if (is.binary && !is.weighted)
    {
        m12 <- (n1 * mean1 + n2 * mean2)/(n1 + n2)
        se <- sqrt(m12 * (1 - m12) * (1/n1 + 1/n2))
        df <- n1 + n2 - 2

    } else if (is.binary && is.weighted)
    {
        se <- sqrt(se1 * se1 + se2 * se2)
        df <- (se1 * se1 / n1 + se2 * se2 / n2)^2 / 
            ((se1 * se1/n1)^2/(n1-bessel) + (se2 * se2 / n2)^2/(n2-bessel))
    }

    t = (mean1 - mean2)/se
    p = pt(-abs(t), df) * 2
    return(p)
}

zTest <- function(mean1, mean2, se1, se2, n1, n2, is.binary, is.weighted, bessel = 0)
{
    if (is.binary && !is.weighted)
    {
        m12 <- (n1 * mean1 + n2 * mean2)/(n1 + n2)
        se <- sqrt(m12 * (1 - m12) * (n1 + n2) / (n1 + n2 - 2*bessel) * (1/n1 + 1/n2))
    
    } else if (is.binary && weighted)
    {
        cat("line 234: is.weighted\n")
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

    #population.variance = sum.xxw / sum.w - mean.x * mean.x
    n.used.in.bessel.correction = n.observations
    var = computeVariances(mean.x, FALSE, sum.w, sum.ww, sum.xw, sum.xww, sum.xxw, sum.xxww, n.used.in.bessel.correction)

    #S2 <- (1 - mean.x)^2
    #S.tmp <- 1 - 2*mean.x + mean.x * mean.x
    #S2a <- 1 - 2 * sum.xw/sum.w + sum.xxww/(sum.w * sum.w)
    #cat("mean.x:", mean.x, "sum.xw/sum.w:", sum.xw/sum.w, "\n")
    #cat("mean.x^2:", mean.x * mean.x, "sum.xxww/(sum.w)^2:", sum.xxww/(sum.w * sum.w), "\n")
    #cat("S2:", S2, "S2a:", S2a, "S.tmp:", S.tmp, "\n")

    #S1 <- (var$se * var$se) * (n.observations - 1)/n.observations
    #S2 <- sum((w*(x - mean.x))^2) / (sum.w * sum.w)
    #S3 <- ((1 - mean.x)^2 * sum.xww + (mean.x * mean.x * (sum.ww - sum.xww)))/(sum.w * sum.w)
    #S4 <- mean.x * (1 - mean.x) / (sum.w - 1)
    #S5 <- (sum(x) * (1 - 2 * mean.x) + (mean.x * mean.x)) * sum.ww / (sum.w * sum.w)
    #cat("S1:", S1, "S2:", S2, "S3:", S3, "S5:", S5, "\n")

    return(c("Average" = mean.x, "Base n" = n.observations,
        sumW = sum.w, sumWW = sum.ww, sumXWW = sum.xww,
        "Standard Error" = var$se))
}

# Checks for binary variables and weights are applied beforehand
# If the test has been changed, the warning should be applied
# in the function calling this - otherwise the warning
# message is generated at every cell
compareTwoSamples <- function(test.type, mean_1, mean_2, se_1, se_2, n1, n2, 
    sumW_1 = NA, sumW_2 = NA, sumWW_1 = NA, sumWW_2 = NA, 
    is.binary = TRUE, is.weighted = FALSE, bessel = 0)
{
    if (test.type == "tTest")
        return(tTest(mean_1, mean_2, se_1, se_2, n1, n2, 
               is.binary, is.weighted))
    else if (test.type == "zTest")
        return(zTest(mean_1, mean_2, se_1, se_2, n1, n2,
               is.binary, is.weighted, bessel))
    else # Nonparametric of Chisquare
        return(raoScottSecondOrder2b2(a, b, is.weighted))
    # Non-parametric tests for numeric variables are handled before
    # getting to this function
}    
