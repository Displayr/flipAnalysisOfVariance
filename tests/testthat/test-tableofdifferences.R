context("Table of Differences")

tb.1num <- structure(c(1600.5, 800, 0, 39.6059004023814), .Dim = c(1L, 4L),
        .Dimnames = list("Unique Identifier", c("Average", "Sample Size",
        "Missing Count", "Standard Error")), basedescriptiontext = "sample size = 800",
        basedescription = list(Minimum = 800L, Maximum = 800L, Range = FALSE, Total = 800L,
        Missing = 0L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
        FilteredProportion = 0), questiontypes = "Number", span = list(
        rows = structure(list("Unique Identifier"), class = "data.frame", .Names = "",
        row.names = 1L)), name = "table.Unique.Identifier",
        questions = c("Unique Identifier", "SUMMARY"))

tbA.1col <- structure(c(100, 100, 93.9393939393939, 100, 79.7979797979798,
        95.959595959596, 100, 99, 99, 99, 99, 99, 99, 99, 0, 0, 0.0241028760295672,
        0, 0.0405583530232473, 0.0198904022396684, 0), .Dim = c(7L, 3L
        ), .Dimnames = list(c("Coca-Cola", "Diet Coke", "Coke Zero",
        "Pepsi", "Diet Pepsi", "Pepsi Max", "NET"), c("%", "Sample Size",
        "Standard Error")), basedescriptiontext = "sample size = 99; 88% filtered out",
        basedescription = list(Minimum = 99L, Maximum = 99L, Range = FALSE, Total = 99L,
        Missing = 0L, EffectiveSampleSize = 99L, EffectiveSampleSizeProportion = 100,
        FilteredProportion = 87.625), questiontypes = "PickAny", span = list(
        rows = structure(list(c("Coca-Cola", "Diet Coke", "Coke Zero",
        "Pepsi", "Diet Pepsi", "Pepsi Max", "NET")), class = "data.frame", .Names = "",
        row.names = c(NA, 7L))), name = "table.Awareness",
        questions = c("Awareness","SUMMARY"))

tbB.1col <- structure(c(100, 100, 90.6593406593407, 100, 82.967032967033,
        92.8571428571429, 100, 182, 182, 182, 182, 182, 182, 182, 0,
        0, 0.0216299620455664, 0, 0.0279420744290416, 0.0191427511217599,
        0), .Dim = c(7L, 3L), .Dimnames = list(c("Coca-Cola", "Diet Coke",
        "Coke Zero", "Pepsi", "Diet Pepsi", "Pepsi Max", "NET"), c("%",
        "Sample Size", "Standard Error")),
        basedescriptiontext = "sample size = 182; 77% filtered out",
        basedescription = list(Minimum = 182L, Maximum = 182L, Range = FALSE, Total = 182L,
        Missing = 0L, EffectiveSampleSize = 182L, EffectiveSampleSizeProportion = 100,
        FilteredProportion = 77.25), questiontypes = "PickAny", span = list(
        rows = structure(list(c("Coca-Cola", "Diet Coke", "Coke Zero",
        "Pepsi", "Diet Pepsi", "Pepsi Max", "NET")), class = "data.frame", .Names = "",
        row.names = c(NA, 7L))), name = "table.Awareness.2",
        questions = c("Awareness", "SUMMARY"))

tb.missingN <- structure(c(6.97674418604651, 6.97674418604651, 16.2790697674419,
        19.7674418604651, 18.6046511627907, 17.4418604651163, 0, 8.13953488372093,
        5.81395348837209, 100, 2.40963855421687, 16.8674698795181, 2.40963855421687,
        12.0481927710843, 18.0722891566265, 25.3012048192771, 12.0481927710843,
        0, 10.8433734939759, 100, 4.73372781065089, 11.8343195266272,
        9.46745562130178, 15.9763313609467, 18.3431952662722, 21.301775147929,
        5.91715976331361, 4.14201183431953, 8.28402366863905, 100, 6,
        6, 14, 17, 16, 15, 0, 7, 5, 86, 2, 14, 2, 10, 15, 21, 10, 0,
        9, 83, 8, 20, 16, 27, 31, 36, 10, 7, 14, 169, 0.0142767081606547,
        0.0142767081606547, 0.0212661327937355, 0.0232062469336523,
        0.0225873021401308,
        0.0219414156422949, 0, 0.0153732320198421, 0.01307270860486,
        0.0385697590987942, 0.00834318527443952, 0.0212661327937355,
        0.00834318527443952, 0.0182035971828893, 0.0219414156422949,
        0.0254506664705403, 0.0182035971828893, 0, 0.0173236699339976,
        0.0385697590987942, 0.0163838734968798, 0.0249210758243042, 0.0225873021401308,
        0.0282673247797227, 0.0298592602893999, 0.0315889889113352, 0.0182035971828893,
        0.0153732320198421, 0.0212661327937355, 0, 0.027632024455163,
        0.027632024455163, 0.0400426076639687, 0.0431957572581551, 0.0422086145568851,
        0.0411591966712186, 0, 0.0296588734911371, 0.0253816351782209,
        0, 0.0169345043011742, 0.0413526689470356, 0.0169345043011742,
        0.0359481389331235, 0.0424927724170274, 0.0480087583043728, 0.0359481389331235,
        0, 0.0343361894107854, 0, 0.0163838734968798, 0.0249210758243042,
        0.0225873021401308, 0.0282673247797227, 0.0298592602893999, 0.0315889889113352,
        0.0182035971828893, 0.0153732320198421, 0.0212661327937355, 0
        ), .Dim = c(10L, 3L, 4L), .Dimnames = list(c("Less than $15,000",
        "$15,001 to $30,000", "$30,001 to $45,000", "$45,001 to $60,000",
        "$60,001 to $90,000", "$90,001 to $120,000", "$120,001 to $150,000",
        "$150,001 to $200,000", "$200,001 or more", "NET"), c("Male",
        "Female", "NET"), c("Column %", "Count", "Standard Error", "Column Standard Error")),
        basedescriptiontext = "sample size = 169; total sample size = 193; 24 missing; 76% filtered out",
        basedescription = list(Minimum = 169L, Maximum = 169L, Range = FALSE, Total = 193L,
        Missing = 24L, EffectiveSampleSize = 169L, EffectiveSampleSizeProportion = 100,
        FilteredProportion = 75.875), questiontypes = c("PickOne",
        "PickOne"), span = list(rows = structure(list(c("Less than $15,000",
        "$15,001 to $30,000", "$30,001 to $45,000", "$45,001 to $60,000",
        "$60,001 to $90,000", "$90,001 to $120,000", "$120,001 to $150,000",
        "$150,001 to $200,000", "$200,001 or more", "NET")), class = "data.frame", .Names = "", row.names = c(NA, 10L)), columns = structure(list(c("Male", "Female", "NET")),
        class = "data.frame", .Names = "", row.names = c(NA,
        3L))), name = "table.Income.by.Gender", questions = c("Income",
        "Gender"))

tb1 <- structure(c(2.75482093663912, 6.06060606060606, 12.6721763085399,
        18.4573002754821, 24.7933884297521, 15.9779614325069, 6.06060606060606,
        8.26446280991736, 4.95867768595041, 100, 3.77906976744186, 15.9883720930233,
        7.84883720930233, 18.0232558139535, 19.7674418604651, 13.0813953488372,
        10.7558139534884, 4.06976744186047, 6.68604651162791, 100, 3.25318246110325,
        10.8910891089109, 10.3253182461103, 18.2461103253182, 22.3479490806223,
        14.5685997171146, 8.34512022630834, 6.22347949080622, 5.7991513437058,
        100, 363, 363, 363, 363, 363, 363, 363, 363, 363, 363, 344, 344,
        344, 344, 344, 344, 344, 344, 344, 344, 707, 707, 707, 707, 707,
        707, 707, 707, 707, 707, 0.00860253488388416, 0.0125408686623489,
        0.0174842753224801, 0.0203902404214479, 0.0226955999650883, 0.0192576374438934,
        0.0125408686623489, 0.0144717760697138, 0.0114099801739348, 0,
        0.0102962758762771, 0.0197890416941105, 0.0145213171481727, 0.0207546252617373,
        0.0215032198303599, 0.0182069143195971, 0.0167287906845875, 0.0106687978228116,
        0.0134868625549616, 0, 0.00667682824465732, 0.0117244876360376,
        0.0114520779299328, 0.0145357354437494, 0.0156780827148481, 0.0132774696490871,
        0.0104085862289755, 0.00909204000853127, 0.00879644676254966,
        0), .Dim = c(10L, 3L, 3L), .Dimnames = list(c("Less than $15,000",
        "$15,001 to $30,000", "$30,001 to $45,000", "$45,001 to $60,000",
        "$60,001 to $90,000", "$90,001 to $120,000", "$120,001 to $150,000",
        "$150,001 to $200,000", "$200,001 or more", "NET"), c("Male",
        "Female", "NET"), c("Column %", "Column Sample Size", "Column Standard Error")),
        basedescriptiontext = "sample size = 707; total sample size = 800; 93 missing",
        basedescription = list(Minimum = 707L, Maximum = 707L, Range = FALSE, Total = 800L,
        Missing = 93L, EffectiveSampleSize = 707L, EffectiveSampleSizeProportion = 100,
        FilteredProportion = 0), questiontypes = c("PickOne", "PickOne"),
        span = list(rows = structure(list(c("Less than $15,000", "$15,001 to $30,000",
        "$30,001 to $45,000", "$45,001 to $60,000", "$60,001 to $90,000",
        "$90,001 to $120,000", "$120,001 to $150,000", "$150,001 to $200,000",
        "$200,001 or more", "NET")), class = "data.frame", .Names = "",
        row.names = c(NA, 10L)), columns = structure(list(c("Male", "Female", "NET")),
        class = "data.frame", .Names = "", row.names = c(NA, 3L))),
        name = "table.Income.by.Gender", questions = c("Income", "Gender"))


tb2 <- structure(c(0, 5.65217391304348, 8.69565217391304, 16.5217391304348,
        27.3913043478261, 21.304347826087, 5.65217391304348, 10.4347826086957,
        4.34782608695652, 100, 0.884955752212389, 9.29203539823009, 6.63716814159292,
        19.9115044247788, 23.8938053097345, 15.929203539823, 11.9469026548673,
        3.53982300884956, 7.9646017699115, 100, 0.43859649122807, 7.45614035087719,
        7.67543859649123, 18.2017543859649, 25.6578947368421, 18.640350877193,
        8.7719298245614, 7.01754385964912, 6.14035087719298, 100, 230,
        230, 230, 230, 230, 230, 230, 230, 230, 230, 226, 226, 226, 226,
        226, 226, 226, 226, 226, 226, 456, 456, 456, 456, 456, 456, 456,
        456, 456, 456, 0, 0.0152600503264415, 0.0186199739053662, 0.0245412588085454,
        0.0294701898150059, 0.0270577543899362, 0.0152600503264415, 0.0202019668373047,
        0.0134761477302566, 0, 0.00624366091106688, 0.019354719412772,
        0.0165953783823424, 0.0266223167371528, 0.0284289883260336, 0.0243965464638765,
        0.0216226402174701, 0.0123189457332278, 0.0180496265967886, 0,
        0.00309793558487965, 0.0123147370497976, 0.0124797110244141,
        0.0180893483069214, 0.0204749211988841, 0.0182568512317944, 0.0132619113336571,
        0.011975326870602, 0.0112546076407926, 0), .Dim = c(10L, 3L,
        3L), .Dimnames = list(c("Less than $15,000", "$15,001 to $30,000",
        "$30,001 to $45,000", "$45,001 to $60,000", "$60,001 to $90,000",
        "$90,001 to $120,000", "$120,001 to $150,000", "$150,001 to $200,000",
        "$200,001 or more", "NET"), c("Male", "Female", "NET"), c("Column %",
        "Column Sample Size", "Column Standard Error")),
        basedescriptiontext = "sample size = 456; total sample size = 519; 63 missing; 35% filtered out",
        basedescription = list(Minimum = 456L, Maximum = 456L, Range = FALSE, Total = 519L,
        Missing = 63L, EffectiveSampleSize = 456L, EffectiveSampleSizeProportion = 100,
        FilteredProportion = 35.125), questiontypes = c("PickOne",
        "PickOne"), span = list(rows = structure(list(c("Less than $15,000",
        "$15,001 to $30,000", "$30,001 to $45,000", "$45,001 to $60,000",
        "$60,001 to $90,000", "$90,001 to $120,000", "$120,001 to $150,000",
        "$150,001 to $200,000", "$200,001 or more", "NET")),
        class = "data.frame", .Names = "", row.names = c(NA, 10L)),
        columns = structure(list(c("Male", "Female", "NET")), class = "data.frame",
        .Names = "", row.names = c(NA, 3L))), name = "table.Income.by.Gender.2",
        questions = c("Income", "Gender"))

tb.char <- structure(c("2.75482093663912", "6.06060606060606", "12.6721763085399",
        "18.4573002754821", "24.7933884297521", "15.9779614325069", "6.06060606060606",
        "8.26446280991736", "4.95867768595041", "100", "3.77906976744186",
        "15.9883720930233", "7.84883720930233", "18.0232558139535", "19.7674418604651",
        "13.0813953488372", "10.7558139534884", "4.06976744186047", "6.68604651162791",
        "100", "3.25318246110325", "10.8910891089109", "10.3253182461103",
        "18.2461103253182", "22.3479490806224", "14.5685997171146", "8.34512022630834",
        "6.22347949080622", "5.7991513437058", "100", "363", "363", "363",
        "363", "363", "363", "363", "363", "363", "363", "344", "344",
        "344", "344", "344", "344", "344", "344", "344", "344", "707",
        "707", "707", "707", "707", "707", "707", "707", "707", "707",
        "0.00860253488388416", "0.0125408686623489", "0.0174842753224801",
        "0.0203902404214479", "0.0226955999650883", "0.0192576374438934",
        "0.0125408686623489", "0.0144717760697138", "0.0114099801739348",
        "0", "0.0102962758762771", "0.0197890416941105", "0.0145213171481727",
        "0.0207546252617373", "0.0215032198303599", "0.0182069143195971",
        "0.0167287906845875", "0.0106687978228116", "0.0134868625549616",
        "0", "0.00667682824465732", "0.0117244876360376", "0.0114520779299328",
        "0.0145357354437494", "0.0156780827148481", "0.0132774696490871",
        "0.0104085862289755", "0.00909204000853127", "0.00879644676254966",
        "0", NA, NA, "b", NA, NA, NA, NA, "b", NA, "-", NA, "A", NA,
        NA, NA, NA, "a", NA, NA, "-", "-", "-", "-", "-", "-", "-", "-",
        "-", "-", "-"), .Dim = c(10L, 3L, 4L), .Dimnames = list(c("Less than $15,000",
        "$15,001 to $30,000", "$30,001 to $45,000", "$45,001 to $60,000",
        "$60,001 to $90,000", "$90,001 to $120,000", "$120,001 to $150,000",
        "$150,001 to $200,000", "$200,001 or more", "NET"), c("Male",
        "Female", "NET"), c("Column %", "Column Sample Size", "Column Standard Error",
        "Column Comparisons")),
        basedescriptiontext = "sample size = 707; total sample size = 800; 93 missing",
        basedescription = list(Minimum = 707L, Maximum = 707L, Range = FALSE, Total = 800L,
        Missing = 93L, EffectiveSampleSize = 707L, EffectiveSampleSizeProportion = 100,
        FilteredProportion = 0), questiontypes = c("PickOne", "PickOne"),
        span = list(rows = structure(list(c("Less than $15,000", "$15,001 to $30,000",
        "$30,001 to $45,000", "$45,001 to $60,000", "$60,001 to $90,000",
        "$90,001 to $120,000", "$120,001 to $150,000", "$150,001 to $200,000",
        "$200,001 or more", "NET")), class = "data.frame", .Names = "",
        row.names = c(NA, 10L)), columns = structure(list(c("Male", "Female", "NET")),
        class = "data.frame", .Names = "", row.names = c(NA, 3L))),
        name = "table.Income.by.Gender", questions = c("Income", "Gender"))


test_that("Table of differences",
{
    expect_error(TableOfDifferences(tb.1num, tb.1num), NA)
    expect_error(TableOfDifferences(tbA.1col, tbB.1col), NA)
    expect_error(TableOfDifferences(tb1, tb.missingN),
        "tables need to contain the cell statistic 'Column Standard Error' and one of 'Column Sample Size'")
    expect_error(TableOfDifferences(tb.char, tb2), NA)
    expect_error(TableOfDifferences(tb1, tb2, cond.shade = "None"), NA)
    expect_error(TableOfDifferences(tb1, tb2, cond.shade = "Arrows"), NA)
    expect_error(TableOfDifferences(tb1, tb2, cond.shade = "Boxes"), NA)
    expect_error(TableOfDifferences(tb1, tb2, cond.shade = "Cell colors"), NA)

    expect_equal(TableOfDifferences(tb1, tb2, output = "qtable"),
        structure(c(0, 5.65217391304348, 8.69565217391304, 16.5217391304348,
        27.3913043478261, 21.304347826087, 5.65217391304348, 10.4347826086957,
        4.34782608695652, 0.884955752212389, 9.29203539823009, 6.63716814159292,
        19.9115044247788, 23.8938053097345, 15.929203539823, 11.9469026548673,
        3.53982300884956, 7.9646017699115, -2.75482093663912, -0.408432147562579,
        -3.97652413462686, -1.9355611450473, 2.597915918074, 5.3263863935801,
        -0.408432147562579, 2.17031979877834, -0.61085159899389, -2.89411401522947,
        -6.69633669479321, -1.21166906770941, 1.8882486108253, 4.1263634492694,
        2.8478081909858, 1.1910887013789, -0.52994443301091, 1.27855525828359,
        0.0111279516250882, 0.837069992354198, 0.133543135976866, 0.547404380176119,
        0.481104668946767, 0.10028147045814, 0.837069992354198, 0.370727585994163,
        0.732557678283053, 0.0347279810662583, 0.0214152206853255, 0.588064335912059,
        0.572251411336666, 0.240014678282952, 0.340821586189606, 0.659498756923685,
        0.74798930260599, 0.563315541789081,
        -2.53865494142727, -0.205642736068235,
        -1.50027517987319, -0.601654143625052, 0.704526956251618, 1.64349059117384,
        -0.205642736068235, 0.895111314919973, -0.341725236441594, -2.11151603579219,
        -2.30058286158096, -0.541643185145334, 0.564738841003716, 1.17495010457587,
        0.952543163770432, 0.44060531147734, -0.321291756880541, 0.577923836049367),
        .Dim = c(9L, 2L, 4L), .Dimnames = list(c("Less than $15,000", "$15,001 to $30,000",
        "$30,001 to $45,000", "$45,001 to $60,000", "$60,001 to $90,000",
        "$90,001 to $120,000", "$120,001 to $150,000", "$150,001 to $200,000",
        "$200,001 or more"), c("Male", "Female"), c("Column %", "Differences",
        "p", "z-Statistic")),
        questiontypes = c("PickOne", "PickOne"),
        span = list(rows = structure(list(c("Less than $15,000", "$15,001 to $30,000",
        "$30,001 to $45,000", "$45,001 to $60,000", "$60,001 to $90,000",
        "$90,001 to $120,000", "$120,001 to $150,000", "$150,001 to $200,000",
        "$200,001 or more", "NET")), class = "data.frame", .Names = "",
        row.names = c(NA, 10L)), columns = structure(list(c("Male", "Female", "NET")),
        class = "data.frame", .Names = "", row.names = c(NA, 3L))),
        questions = c("Income", "Gender")))
})

test_that("DS-5117 Table of differences warns for unsuported test types", {

    expect_warning(
        TableOfDifferences(tb1, tb2, proportions.test = "ChrisTest"),
        "The selected test.type 'ChrisTest' is not supported by the Table of Differences."
    )
    expect_warning(
        TableOfDifferences(tb1, tb2, proportions.test = "Quantum"),
        "Quantum and Survey Reporter tests are not supported by the Table of Differences."
    )
    expect_warning(
        TableOfDifferences(tb.1num, tb.1num, means.test = "Quantum"),
        "Quantum and Survey Reporter tests are not supported by the Table of Differences."
    )
    expect_warning(
        TableOfDifferences(tb1, tb2, proportions.test = "SurveyReporter"),
        "Quantum and Survey Reporter tests are not supported by the Table of Differences."
    )
    expect_warning(
        TableOfDifferences(tb.1num, tb.1num, means.test = "SurveyReporter"),
        "Quantum and Survey Reporter tests are not supported by the Table of Differences."
    )

    tb1.weighted <- tb1
    attr(tb1.weighted, "weight.name") = "This is a weight"
    tb2.weighted <- tb2
    attr(tb2.weighted, "weight.name") = "This is a weight"
    expect_warning(
        TableOfDifferences(tb1.weighted, tb2.weighted, proportions.test = "Nonparametric"),
        "Non-parametric tests for weighted proportions are not supported by the Table of Differences."
    )

    tb.1num.weighted <- tb.1num
    attr(tb.1num.weighted, "weight.name") = "This is a weight"
    expect_warning(
        TableOfDifferences(tb.1num.weighted, tb.1num.weighted, means.test = "Nonparametric"),
        "Non-parametric tests for means are not supported by the Table of Differences."
    )
})

test_that("DS-5117 Table of differences warns if weighting is ambiguous", {

    tb1.no.attributes <- tb1
    attr(tb1.no.attributes, "questions") <- NULL
    expect_warning(
        TableOfDifferences(tb1.no.attributes, tb1.no.attributes),
        "Could not determine if the input tables contain weighted data"
    )
})
