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
        pval[i] <- if (non.parametric && !is.binary)
        {
            if (is.null(weights))
                kruskal.test(x, y)$p.value
            else
                {
                    df <- data.frame(x = x, y = y, w = weights)
                    svyranktest(x ~ y, svydesign(id= ~1, weights = ~w, data = df))$p.value
                }
        }
        else calcPvalue(x, x.is.binary = is.binary, y = y, w = weights)
    }
    return(pval)
}

calcPvalue = function(x,                        # A binary or numeric variable
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

    if (!x.is.binary)
    {
        #filters = list(y == 1 & !is.na(y), # The asymmetric is.na due to double counting of 'Missing n'
        #               y == 0 )
        filters = list(which(y == 1), # y == 1 & !is.na(y), # The asymmetric is.na due to double counting of 'Missing n'
                       which(y==0)) #y == 0 )

        a = computeNumericVarStats(Filter(x, filters[[1]]), Filter(w, filters[[1]]))
        b = computeNumericVarStats(Filter(x, filters[[2]]), Filter(w, filters[[2]]))
        return(independentSamplesTTestMeans(a["Average"], b["Average"], a["Standard Error"], b["Standard Error"], a["Base n"], b["Base n"]))
    }

    # Identifying missing values; these are values that are:
    # - Missing in  x (e.g., if x is Pick Any and x = Coke | Pepsi, if either coke or Pepsi have missing values, then x is missing.
    # - Missing in y
    # - Missing or <= 0 in w
    m = is.na(x) | if(is.null(y)) FALSE else is.na(y) | is.na(w) | w <= 0

    # Filtering data to removing missing values
    x = Filter(x, !m)
    y = Filter(y, !m)
    w = Filter(w, !m)

    # variables
    ww = w * w # This is a multiplication of each element by each other element
    xw = x * w
    xxw = x * xw
    xww = xw * w
    xxww = xxw * w
    yw = y * w # This is a multiplication of each element by each other element
    yyw = y * yw
    yww = yw * w
    yyww = yyw * w
    xy = x * y
    xyw = xy * w
    xyww = xyw * w

    # Summations of variables
    n.observations = length(x)
    n.missing = sum(m)
    sum.w = sum(w)
    sum.ww = sum(ww)
    sum.x = sum(x)
    sum.xw = sum(xw)
    sum.xxw = sum(xxw)
    sum.xww = sum(xww)
    sum.xxww = sum(xxww)
    sum.y = sum(y)
    sum.yw = sum(yw)
    sum.yyw = sum(yyw)
    sum.yww = sum(yww)
    sum.yyww = sum(yyww)
    sum.xy = sum(xy)
    sum.xyw = sum(xyw)
    sum.xyww = sum(xyww)

    proportionxy = sum.xyw / sum.w
    proportiony = sum.yw / sum.w
    proportionNotxy = proportiony - proportionxy
    proportionx = sum.xw / sum.w
    proportionyNotxy = proportionx - proportionxy
    proportionyNotxNoty = 1 - proportionxy - proportionNotxy - proportionyNotxy
    sum.Notxyww = sum.yww -sum.xyww
    sum.xNotyww = sum.xww -sum.xyww
    sum.NotxNotyww = sum.ww - sum.Notxyww - sum.xNotyww - sum.xyww
    sums.ww = c(sum.NotxNotyww, sum.xNotyww, sum.Notxyww, sum.xyww)
    proportions = c(proportionyNotxNoty, proportionyNotxy,proportionNotxy, proportionxy)
    counts = matrix(proportions * n.observations, 2)

    variance = multinomialCovarianceMatrix(proportions, sums.ww, sum.ww, sum.w, n.observations)
    p = raoScottSecondOrder2b2(proportions,
                               counts,
                               variance,
                               n.observations - sum.y,
                               sum.y,
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
raoScottSecondOrder2b2 <- function(proportions,
                       counts,
                       variance,
                       n0,
                       n1,
                       is.weighted)
{
    group_sizes = colSums(counts)
    row.totals = rowSums(counts)
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

    population.variance = sum.xxw / sum.w - mean.x * mean.x
    n.used.in.bessel.correction = n.observations
    var = computeVariances(mean.x, FALSE, sum.w, sum.ww, sum.xw, sum.xww, sum.xxw, sum.xxww, n.used.in.bessel.correction)
    return(c("Average" = mean.x, "Base n" = n.observations, "Standard Error" = var$se))
}

