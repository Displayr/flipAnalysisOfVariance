context("Two sample tests")
findInstDirFile <- function(file)
{
    file.path(system.file("testdata", package = "flipAnalysisOfVariance", mustWork = TRUE),
              file)
}
load(findInstDirFile("twosampletests.rda"))


test_that("Comparing Proportions (unweighted)",
{
    res <- TableOfDifferences(tb.AgeMales, tb.AgeFemales, proportions.test = "tTest")
    expect_equal(attr(res, "p-value"), c(`18 to 24` = 0.536259477922785,
        `25 to 29` = 0.927843374978388, `30 to 34` = 0.639799144128667,
        `35 to 39` = 0.0776989620280411, `40 to 44` = 0.262600176287746,
        `45 to 49` = 0.814540424544341, `50 to 54` = 0.197038245890513,
        `55 to 64` = 0.728676640369952, `65 or more` = 0.0419679190057229))

    res <- TableOfDifferences(tb.AgeMales, tb.AgeFemales, proportions.test = "zTest")
    expect_equal(attr(res, "p-value"), c(`18 to 24` = 0.536082900256781,
        `25 to 29` = 0.927820642717578, `30 to 34` = 0.639671305972617,
        `35 to 39` = 0.0773167026691302, `40 to 44` = 0.26226293246625,
        `45 to 49` = 0.81448023121581, `50 to 54` = 0.196664361018651,
        `55 to 64` = 0.728585150490911, `65 or more` = 0.0416384099417913))

    res <- TableOfDifferences(tb.AgeMales, tb.AgeFemales, proportions.test = "Nonparametric")
    expect_equal(attr(res, "p-value"), c(0.536082900256781, 0.927820642717578,
        0.639671305972617, 0.0773167026691303, 0.262262932466249, 0.81448023121581,
        0.196664361018651, 0.728585150490912, 0.0416384099417914))
})

test_that("Comparing Proportions (weighted)",
{
    expect_warning(res <- TableOfDifferences(tb.AgeMalesW, tb.AgeFemalesW,
        proportions.test = "Nonparametric"), "The tables were compared using a Z-test.")
    res <- TableOfDifferences(tb.AgeMalesW, tb.AgeFemalesW, proportions.test = "zTest")
    expect_equal(attr(res, "p-value"), c(`18 to 24` = 0.782116695108154,
        `25 to 29` = 0.517309654384073, `30 to 34` = 0.908927558498363,
        `35 to 39` = 0.423626439759687, `40 to 44` = 0.263512930438365,
        `45 to 49` = 0.877964821892863, `50 to 54` = 0.149067724568697,
        `55 to 64` = 0.782969208284808, `65 or more` = 0.0434567774432414))
    res <- TableOfDifferences(tb.AgeMalesW, tb.AgeFemalesW, proportions.test = "tTest")
    expect_equal(attr(res, "p-value"), c(`18 to 24` = 0.782188129947524,
        `25 to 29` = 0.517495506181539, `30 to 34` = 0.908956286931761,
        `35 to 39` = 0.42386398760131, `40 to 44` = 0.263855884368836,
        `45 to 49` = 0.878003756320718, `50 to 54` = 0.14946639542492,
        `55 to 64` = 0.783040428342354, `65 or more` = 0.043836773319435))
    res <- TableOfDifferences(tb.AgeMalesW, tb.AgeFemalesW, proportions.test = "tTest",
        proportions.bessel = 1, design.effect.constant = 2)
    expect_equal(attr(res, "p-value"), c(`18 to 24` = 0.845005420958658,
        `25 to 29` = 0.64718472112193, `30 to 34` = 0.935552189832649,
        `35 to 39` = 0.571696705430036, `40 to 44` = 0.429397093937519,
        `45 to 49` = 0.91356622926705, `50 to 54` = 0.307931244662508,
        `55 to 64` = 0.845619685901269, `65 or more` = 0.153779707000038))
})

test_that("Comparing Averages (unweighted)",
{
    expect_warning(res <- TableOfDifferences(tb.IdMales, tb.IdFemales,
        means.test = "Nonparametric"), "The tables were compared using a t-Test.")
    res <- TableOfDifferences(tb.IdMales, tb.IdFemales, means.test = "tTest")
    expect_equal(attr(res, "p-value"), 0.416387593497747)
    res <- TableOfDifferences(tb.IdMales, tb.IdFemales, means.test = "zTest")
    expect_equal(attr(res, "p-value"), 0.416144836438102)
})

test_that("Comparing Averages (weighted)",
{
    expect_warning(res <- TableOfDifferences(tb.IdMalesW, tb.IdFemalesW,
        means.test = "Nonparametric"), "The tables were compared using a t-Test.")
    res <- TableOfDifferences(tb.IdMalesW, tb.IdFemalesW, means.test = "tTest")
    expect_equal(attr(res, "p-value"), 0.689606500592584)
    res <- TableOfDifferences(tb.IdMalesW, tb.IdFemalesW, means.test = "zTest")
    expect_equal(attr(res, "p-value"), 0.689499274166147)
})
