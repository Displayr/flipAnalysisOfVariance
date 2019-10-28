#' @importFrom stats pt qnorm
CellStatistic = function(x, # A binary 1/0 variable representing a category (e.g., 18-24 | 25 to 29) or a numeric variable
                         x.is.binary = TRUE, # TRUE if x is a binary variable
                         y, # an optional binary or numeric varaible (same length as x)
                         y.is.binary = TRUE, #assumed FALSE if y does not exist
                         z = NULL, # an optional numeric variable; only can exist if x.is.binary and y.is.binary
                         f = rep(TRUE, length(x)),  # boolean variable with a TRUE if to be used as a filter and a FALSE otherwise (same length as x)
                         w = rep(1, length(x)),  # weight variable (same length as x)
                        not.duplicate = TRUE,
                        reproduce.error.in.bessel.correction = FALSE) # Property of the data reduction
{
    if (length(w) <= 1)
        w <- rep(1, length(x))
    
    Filter = function(x, f)
    {
        if (is.null(f))
            return(NULL)
        x[f]
    }
    
    if (!is.null(y))
    {
        # Comparing means by group
        if (x.is.binary & !y.is.binary) # Transpose
        {
            out = CellStatistic(y, y.is.binary, x, x.is.binary, z, f, w, not.duplicate)
            names(out) = sub("Column", "Row", names(out))
            return(out)
        }
        if (!x.is.binary & y.is.binary) # Largely just filtering and applying mean
        {  # Presumably this is most efficiently done via GROUPBY in SQL
           # The code below will fall over with nulls for f and w, but these
           # are trivial to fix
            filters = list(f & y == 1 & !is.na(y), # The asymmetric is.na due to double counting of 'Missing n'
                           f & y == 0 ) 
             stats = list()
             for (i in 1:2)
                  stats[[i]] = CellStatistic(x, 
                                                FALSE,
                                                NULL, 
                                                TRUE,
                                                NULL,
                                                filters[[i]], 
                                                w,
                                              not.duplicate)
            # Note that I am only using a tiny subset of hte data in stats[[2]]
            a = stats[[1]]
            b = stats[[2]]
            test = IndependentSamplesTTestMeans(a["Average"], b["Average"], a["Standard Error"], b["Standard Error"], a["Base n"], b["Base n"])
            #z = zStatistic(test$p, test$t > 0) 
            return(c(a[1:5],
              "Column n" = unname(a["Base n"]),
              "Base n" = unname(a["Base n"] + b["Base n"]),
              "Missing n" = unname(a["Missing n"] + b["Missing n"]),
              "Effective Base n" = unname(a["Effective Base n"] + b["Effective Base n"]),
              "Column Population" = unname(a["Base Population"]),
              "Base Population" = unname(a["Base Population"] + b["Base Population"]),
                     "t-Statistic" = test$t,
                     "d.f." = test$df,
              #"z-Statistic" = z,
              a["Standard Error"],
              "p" = test$p,
              a["Not Duplicate"]))
        }
    }
    # Filtering out values
    x = Filter(x, f)
    y = Filter(y, f)
    z = Filter(z, f)
    w = Filter(w, f)
    # Identifying missing values; these are values that are:
    # - Missing in  x (e.g., if x is Pick Any and x = Coke | Pepsi, if either coke or Pepsi have missing values, then x is missing.
    # - Missing in y
    # - Missing or <= 0 in w
    m = is.na(x) | if(is.null(y)) FALSE else is.na(y) | is.na(w) | w <= 0 | if(is.null(z)) FALSE else is.na(z)

    # Filtering data to removing missing values
    x = Filter(x, !m)
    y = Filter(y, !m)
    z = Filter(z, !m)
    w = Filter(w, !m)

    # variables
    ww = w * w # This is a multiplication of each element by each other element

    x.not.zero = x != 0
    xw = x * w
    xxw = x * xw
    xww = xw * w
    xxww = xxw * w
    if (!is.null(y))
    {
        yw = y * w # This is a multiplication of each element by each other element
        yyw = y * yw
        yww = yw * w
        yyww = yyw * w

        xy = x * y
        xyw = xy * w
        xyww = xyw * w
    }
    # Summations of variables
    n.observations = length(x)
    n.missing = sum(m)
    sum.w = sum(w)
    sum.ww = sum(ww)

    sum.x.not.0 = sum(x.not.zero)
    sum.x = sum(x)
    sum.xw = sum(xw)
    sum.xxw = sum(xxw)
    sum.xww = sum(xww)
    sum.xxww = sum(xxww)
    if (is.null(y))
    {
        if (!x.is.binary)
        {
            min.x = min(x)
            max.x = max(x)
        }
    } else {
        
        sum.y = sum(y)
        sum.yw = sum(yw)
        sum.yyw = sum(yyw)
        sum.yww = sum(yww)
        sum.yyww = sum(yyww)

        sum.xy = sum(xy)
        sum.xyw = sum(xyw)
        sum.xyww = sum(xyww)
    }

    if (is.null(y)) # Simple means and proportions of binary variables
    {
        mean.x = sum.xw / sum.w 
        var =   ComputeVariances(mean.x, x.is.binary, sum.w, sum.ww, sum.xw, sum.xww, sum.xxw, sum.xxww,  n.observations)
        if (x.is.binary) #Proportions
            return(c("%" = mean.x * 100,
                     n = sum.x.not.0,
                     "Base n" = n.observations,
                     "Missing n" = n.missing,
                     "Effective Base n" = var$ess,
                     Population = sum.xw,
                     "Base Population" = sum.w,
                     "Standard Error" = var$se,
                "Not Duplicate" = not.duplicate))
        else 
            {
            population.variance = sum.xxw / sum.w - mean.x * mean.x 
            n.used.in.bessel.correction = n.observations
            if(reproduce.error.in.bessel.correction)
            {
                warning("Incorrect adjustment; provided for checking purposes.")
                n.used.in.bessel.correction = n.used.in.bessel.correction + n.missing
            }
            sample.variance = population.variance * n.used.in.bessel.correction / (n.used.in.bessel.correction - 1)
            var =   ComputeVariances(mean.x, x.is.binary, sum.w, sum.ww, sum.xw, sum.xww, sum.xxw, sum.xxww,  n.used.in.bessel.correction)
            c(Average = mean.x, # Means
              "Standard Deviation" = sqrt(sample.variance),
               Minimum = min.x,
               Maximum = max.x,
                Sum = sum.xw,
                "Base n" = n.observations,
                "Missing n" = n.missing,
                     "Effective Base n" = var$ess,
                "Base Population" = sum.w,
                     "Standard Error" = var$se,
                "Not Duplicate" = not.duplicate)
            }

    } else {
        if (!x.is.binary & !y.is.binary) # Correlations of numeric variables
        {
            mean.x = sum.xw / sum.w
            mean.y = sum.yw / sum.w
            
            numerator = sum.xyw - mean.y * sum.xw - mean.x * sum.yw + mean.x * mean.y * sum.w
            x.var = sum.xxw - mean.x * mean.x * sum.w 
            y.var = sum.yyw - mean.y * mean.y * sum.w
            r = numerator / (sqrt(x.var * y.var))
            ess = sum.w * sum.w / sum.ww
            warning("SEs are inconsistent. Why?")
            se = (1 - r * r)/sqrt(ess - 1) # Cell statistic
            df = round(ess) - 2 
            se.for.test = sqrt((1 - r * r) / df) # SE used in test
            t = if (is.na(se) | abs(se) < 1E-6) 100 else r / se.for.test
            p = if (r > 0.98 & ess > 5) 0 else pt(-abs(t), df) * 2
            z = zStatistic(p , t >= 0)
            return(c("Correlation" = r,
              "Column n" = n.observations,
              "Row n" = n.observations,
              "Base n" = n.observations,
              "Missing n" = n.missing,
             "Effective Base n" = ess,
              "Column Population" = sum.w,
              "Row Population" = sum.w,
              "Base Population" = sum.w,
              "z-Statistics" = z,
              "Standard Error" = se, 
              "p" = p,
              "Not Duplicate" = not.duplicate))
        }

        if (x.is.binary & y.is.binary) # Crosstab of categoricals
        {
            total.prop = sum.xyw / sum.w
            var = ComputeVariances(total.prop, TRUE, sum.w, sum.ww, sum.xyw, sum.xyww, sum.xyw, sum.xyww, n.observations)
            column.prop = sum.xyw / sum.yw
            var.column = ComputeVariances(column.prop, TRUE, sum.yw, sum.yww, sum.xyw, sum.xyww,  sum.xyw, sum.xyww,  sum.y)

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
            proportions = c(proportionyNotxNoty, proportionyNotxy,proportionNotxy, proportionxy )
            counts = matrix(proportions * n.observations, 2)
            # The next few lines of code can be computed within Q (slightly different function names and signatures)
            variance = MultinomialCovarianceMatrix(proportions, sums.ww, sum.ww,   sum.w, n.observations)
            p = RaoScottSecondOrder2b2(proportions,
                                       counts,
                                       variance, 
                                       n.observations - sum.y, 
                                       sum.y)
            c("Column %" = column.prop * 100,
              "Row %" = sum.xyw / sum.xw * 100,
              "Total %" = total.prop * 100,
              n = sum.xy,
              "Column n" = sum.y,
              "Row n" = sum.x,
              "Base n" = n.observations,
              "Missing n" = n.missing,
              "Effective Base n" = var$ess,
              "Population" = sum.xyw,
              "Column Population" = sum.yw,
              "Row Population" = sum.xw,
              "Base Population" = sum.w,
              "z-Statistics" = zStatistic(p, column.prop > sum.xw / sum.w),
              "Standard Error" = var$se,
              "Column Standard Error" = var.column$se,
               "p" = p,
               "Not Duplicate" = not.duplicate)
        }
    }
}

    # Functions - these are all from the c# SamplingVariance class (albeit in slightly different forms)
    ComputeVariances <- function(mean, is.binary, sum.w, sum.ww, sum.xw, sum.xww, sum.xxw, sum.xxww, n.observations)
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
        ess = if (taylor < 0.000001) # Due to numeric precision issues
            sum.w * sum.w / sum.ww
            else n.observations * naive / taylor
        list(taylor = taylor,
             naive = naive,
             ess = ess,
             se = sqrt(taylor))
    }

ComputeVariance <- function(ww_sums_of_squares,
                            total_w,
                            n)
{
    ww_sums_of_squares / (total_w * total_w) * (n / (n - 1))
}

# A simplification of RaoScottSecondOrder2b2 from Q's C#
RaoScottSecondOrder2b2 <- function(proportions,
                       counts,
                       variance, 
                       n0,  
                       n1)
        {
            group_sizes = colSums(counts)
            row.totals = rowSums(counts)
            total = sum(row.totals)
            n = n0 + n1;
            expected = matrix(c(group_sizes[1]*row.totals[1]/total,
                                group_sizes[1]*row.totals[2]/total,
                                group_sizes[2]*row.totals[1]/total,
                                group_sizes[2]*row.totals[2]/total), 2)
            pearson.statistic = sum((counts - expected)^2/expected)#chisq.test(counts, correct = FALSE)$statistic
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

IndependentSamplesTTestMeans <- function(mean1,
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
    
    t = (mean1 - mean2) / .ComputeStandardError(standard_error_1,  standard_error_2)
    df = .WelchDegreesOfFreedom(standard_error_1, standard_error_2, n1, n2)
    p = pt(-abs(t), df)  * 2
    list(t = t, df = df, p = p)
}

zStatistic <- function(p, positive)
{
    z = qnorm(1 - p/2)
    if (!positive)
        z = -z
    z
}

MultinomialCovarianceMatrix <- function(proportions,ww, ww_total,  w_total, n)
        {
            #print("MultinomialCovarianceMatrix")
            #print(proportions)
            #print(ww)
            #print(ww_total)
            #print(w_total)
            #print(n)
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
                    #print("c(p1,p2,  ww1, ww2)")
                    #print(c(p1,p2,  ww1, ww2))
                    sc = if(r == c)
                        ComputeSamplingVarianceForProportion(p1, ww1, ww_total, w_total, n) #ComputeVariances(p1, ww1, ww_total, w_total, n)
                        else SamplingCovariance(p1, p2, ww1, ww2, ww_total, w_total, n)
                    covariance[c, r] = covariance[r, c] = sc
                }
            }
           covariance 
        }

ComputeSamplingVarianceForNumericVariable <- function(input_mean,
                                                      population_variance,  
                                                      ww_sums_of_squares,
                                                      w_total, 
                                                      input_n,
                                                      ww_total)
{
    sumSquaredWeights = ww_total
    n = input_n
    mean = input_mean
    variance = population_variance * n / (n - 1)
    naive_sampling_variance = variance / n
   samplingVariance = ComputeVariance(ww_sums_of_squares, w_total, n)
   effectiveSampleSize = if(samplingVariance <= 0.000001) w_total * w_total / ww_total 
            else input_n * naive_sampling_variance / samplingVariance
    #print(sqrt(samplingVariance))
    list(effective.sample.size = effectiveSampleSize,
         variance = variance, 
         standard.deviation = sqrt(variance),
         standard.error = sqrt(samplingVariance))
}

ComputeSamplingVarianceForProportion <- function(input_proportion, ww, ww_total, w_total,sample_size)
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
            ww_sums_of_squares / (w_total * w_total) * sample_size/(sample_size - 1.0);
}

# From the c# SamplingVariance(double proportion1, double proportion2, double ww1, double ww2, double ww_total, double w_total, int n, StatisticalAssumptions statistical_assumptions)
SamplingCovariance <- function(proportion1,  proportion2,  ww1,  ww2,  ww_total,  w_total, n)
{
    ww_sums_of_squares = -proportion1 * -proportion2 * (ww_total - ww1 - ww2) + -proportion1 * (1 - proportion2) * ww2 + -proportion2 * (1 - proportion1) * ww1
    ww_sums_of_squares / (w_total * w_total) * (n / (n - 1))
}
