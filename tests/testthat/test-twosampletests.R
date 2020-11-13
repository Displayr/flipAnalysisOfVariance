context("Two sample tests")

tb.AgeM <- structure(c(11.6455696202532, 11.6455696202532, 10.8860759493671,
    9.36708860759494, 12.9113924050633, 8.10126582278481, 10.379746835443,
    16.2025316455696, 8.86075949367089, 100, 395, 395, 395, 395,
    395, 395, 395, 395, 395, 395, 0.0161601885185669, 0.0161601885185669,
    0.0156913515686539, 0.0146790236035488, 0.0168934792534427, 0.0137462138243347,
    0.0153655585858085, 0.0185634536216532, 0.0143166057656959, 0
    ), .Dim = c(10L, 3L), .Dimnames = list(c("18 to 24", "25 to 29",
    "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
    "65 or more", "NET"), c("%", "Sample Size", "Standard Error")),
    basedescriptiontext = "sample size = 395; 51% filtered out",
    basedescription = list(Minimum = 395L, Maximum = 395L, Range = FALSE, Total = 395L,
    Missing = 0L, EffectiveSampleSize = 395L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 50.625), questiontypes = "PickOne", span = list(
    rows = structure(list(c("18 to 24", "25 to 29", "30 to 34",
    "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
    "65 or more", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
    10L))), name = "table.Age.2", questions = c("Age", "SUMMARY"))

tb.AgeF <- structure(c(13.0864197530864, 11.8518518518519, 9.87654320987654,
    13.3333333333333, 10.3703703703704, 7.65432098765432, 13.3333333333333,
    15.3086419753086, 5.18518518518519, 100, 405, 405, 405, 405,
    405, 405, 405, 405, 405, 405, 0.01677890289958, 0.0160808507427508,
    0.0148433087836342, 0.0169123801654376, 0.0151681373264829, 0.0132272920395241,
    0.0169123801654376, 0.0179141909454038, 0.0110313726010785, 0
    ), .Dim = c(10L, 3L), .Dimnames = list(c("18 to 24", "25 to 29",
    "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
    "65 or more", "NET"), c("%", "Sample Size", "Standard Error")),
    basedescriptiontext = "sample size = 405; 49% filtered out",
    basedescription = list(Minimum = 405L, Maximum = 405L, Range = FALSE, Total = 405L,
    Missing = 0L, EffectiveSampleSize = 405L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 49.375), questiontypes = "PickOne", span = list(
    rows = structure(list(c("18 to 24", "25 to 29", "30 to 34",
    "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
    "65 or more", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
    10L))), name = "table.Age.3", questions = c("Age", "SUMMARY"))

tb.AgeMW <- structure(c(11.8599157453501, 10.5586351328004, 10.7919581859254,
    9.87901109778093, 13.5201424804582, 7.96705381608331, 9.36553891166502,
    16.5338043929322, 9.52394023700455, 100, 395, 395, 395, 395,
    395, 395, 395, 395, 395, 395, 0.0199334445550185, 0.0188216385487551,
    0.0195320277743787, 0.0185371817221076, 0.0211620242498179, 0.0167653095264825,
    0.0179313597388368, 0.0227694298022236, 0.0186650928109061, 0),
    .Dim = c(10L, 3L), .Dimnames = list(c("18 to 24", "25 to 29",
    "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
    "65 or more", "NET"), c("%", "Sample Size", "Standard Error")),
    basedescriptiontext = "sample size = 395; effective sample size = 262 (66%); 51% filtered out",
    basedescription = list(Minimum = 395L, Maximum = 395L, Range = FALSE, Total = 395L,
    Missing = 0L, EffectiveSampleSize = 262L, EffectiveSampleSizeProportion = 66,
    FilteredProportion = 50.625), questiontypes = "PickOne", span = list(
    rows = structure(list(c("18 to 24", "25 to 29", "30 to 34",
    "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
    "65 or more", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
    10L))), name = "table.Age.4", questions = c("Age", "SUMMARY"),
    weight.name = "UniqueID", weight.label = "Unique Identifier")

tb.AgeFW <- structure(c(12.6447804903137, 12.3129028403221, 11.1051949289579,
    11.9981182506992, 10.3792692943591, 7.61367263310275, 13.3310744274203,
    15.6569407657389, 4.95804636908607, 100, 405, 405, 405, 405,
    405, 405, 405, 405, 405, 405, 0.0202001658065446, 0.0194878835023,
    0.0191917471002386, 0.0189149718639111, 0.0184728113701111, 0.0157662452530119,
    0.020829250038323, 0.0222471323221295, 0.0127627357591256, 0), .Dim = c(10L,
    3L), .Dimnames = list(c("18 to 24", "25 to 29", "30 to 34", "35 to 39",
    "40 to 44", "45 to 49", "50 to 54", "55 to 64", "65 or more",
    "NET"), c("%", "Sample Size", "Standard Error")),
    basedescriptiontext = "sample size = 405; effective sample size = 275 (68%); 49% filtered out",
    basedescription = list(Minimum = 405L, Maximum = 405L, Range = FALSE, Total = 405L,
    Missing = 0L, EffectiveSampleSize = 275L, EffectiveSampleSizeProportion = 68,
    FilteredProportion = 49.375), questiontypes = "PickOne", span = list(
    rows = structure(list(c("18 to 24", "25 to 29", "30 to 34",
    "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
    "65 or more", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
    10L))), name = "table.Age.5", questions = c("Age", "SUMMARY"),
    weight.name = "UniqueID", weight.label = "Unique Identifier")



test_that("Proportions (unweighted)",
{
    res <- TableOfDifferences(tb.AgeM, tb.AgeF, proportions.test = "tTest")
    expect_equal(attr(res, "p-value"), c(`18 to 24` = 0.536259477922785,
        `25 to 29` = 0.927843374978388, `30 to 34` = 0.639799144128667,
        `35 to 39` = 0.0776989620280411, `40 to 44` = 0.262600176287746,
        `45 to 49` = 0.814540424544341, `50 to 54` = 0.197038245890513,
        `55 to 64` = 0.728676640369952, `65 or more` = 0.0419679190057229))

    res <- TableOfDifferences(tb.AgeM, tb.AgeF, proportions.test = "zTest")
    expect_equal(attr(res, "p-value"), c(`18 to 24` = 0.536082900256781,
        `25 to 29` = 0.927820642717578, `30 to 34` = 0.639671305972617,
        `35 to 39` = 0.0773167026691302, `40 to 44` = 0.26226293246625,
        `45 to 49` = 0.81448023121581, `50 to 54` = 0.196664361018651,
        `55 to 64` = 0.728585150490911, `65 or more` = 0.0416384099417913))

    res <- TableOfDifferences(tb.AgeM, tb.AgeF, proportions.test = "Nonparametric")
    expect_equal(attr(res, "p-value"), c(0.536082900256781, 0.927820642717578,
        0.639671305972617, 0.0773167026691303, 0.262262932466249, 0.81448023121581,
        0.196664361018651, 0.728585150490912, 0.0416384099417914))
})

test_that("Proportions (weighted)",
{
    res <- TableOfDifferences(tb.AgeMW, tb.AgeFW, proportions.test = "zTest")
    #expect_equal(attr(res, "p-value"),

})
