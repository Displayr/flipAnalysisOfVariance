context("Table of Differences")

tb1 <- structure(c(6.97674418604651, 6.97674418604651, 16.2790697674419,
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
        "Female", "NET"), c("Column %", "Count", "Standard Error", "Column Standard Error"
        )), basedescriptiontext = "sample size = 169; total sample size = 193; 24 missing; 76% filtered out", basedescription = list(
            Minimum = 169L, Maximum = 169L, Range = FALSE, Total = 193L,
            Missing = 24L, EffectiveSampleSize = 169L, EffectiveSampleSizeProportion = 100,
            FilteredProportion = 75.875), questiontypes = c("PickOne",
        "PickOne"), span = list(rows = structure(list(c("Less than $15,000",
        "$15,001 to $30,000", "$30,001 to $45,000", "$45,001 to $60,000",
        "$60,001 to $90,000", "$90,001 to $120,000", "$120,001 to $150,000",
        "$150,001 to $200,000", "$200,001 or more", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
        10L)), columns = structure(list(c("Male", "Female", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
        3L))), name = "table.Income.by.Gender", questions = c("Income",
        "Gender"))

    tb2 <- structure(c(4.3956043956044, 5.49450549450549, 17.5824175824176,
        23.0769230769231, 20.8791208791209, 7.69230769230769, 9.89010989010989,
        4.3956043956044, 6.59340659340659, 100, 12.3287671232877, 31.5068493150685,
        13.6986301369863, 9.58904109589041, 12.3287671232877, 4.10958904109589,
        8.21917808219178, 8.21917808219178, 0, 100, 7.92682926829268,
        17.0731707317073, 15.8536585365854, 17.0731707317073, 17.0731707317073,
        6.09756097560976, 9.14634146341463, 6.09756097560976, 3.65853658536585,
        100, 4, 5, 16, 21, 19, 7, 9, 4, 6, 91, 9, 23, 10, 7, 9, 3, 6,
        6, 0, 73, 13, 28, 26, 28, 28, 10, 15, 10, 6, 164, 0.0120823757223919,
        0.0134662264725354, 0.0232409137397623, 0.0261721870712866, 0.0250682000230235,
        0.0158329265103047, 0.0178381352952906, 0.0120823757223919, 0.0147050504364147,
        0.0389264210557415, 0.0178381352952906, 0.0271979282713926, 0.0187422924478909,
        0.0158329265103047, 0.0178381352952906, 0.0104962922691683, 0.0147050504364147,
        0.0147050504364147, 0, 0.0389264210557415, 0.0211603330801224,
        0.0294720821868328, 0.0286080704716836, 0.0294720821868328, 0.0294720821868328,
        0.0187422924478909, 0.0225788133518564, 0.0187422924478909, 0.0147050504364147,
        0, 0.0216086171483555, 0.0240199213344396, 0.0401261946890232,
        0.0444115591684328, 0.0428430520650943, 0.0280883362823162, 0.0314677386006074,
        0.0216086171483555, 0.026159078492831, 0, 0.0387455770513177,
        0.0547469351976338, 0.0405210944047919, 0.0347001921314399, 0.0387455770513177,
        0.0233948647624648, 0.0323686001798124, 0.0323686001798124, 0,
        0, 0.0211603330801224, 0.0294720821868328, 0.0286080704716836,
        0.0294720821868328, 0.0294720821868328, 0.0187422924478909, 0.0225788133518564,
        0.0187422924478909, 0.0147050504364147, 0), .Dim = c(10L, 3L,
        4L), .Dimnames = list(c("Less than $15,000", "$15,001 to $30,000",
        "$30,001 to $45,000", "$45,001 to $60,000", "$60,001 to $90,000",
        "$90,001 to $120,000", "$120,001 to $150,000", "$150,001 to $200,000",
        "$200,001 or more", "NET"), c("Male", "Female", "NET"), c("Column %",
        "Count", "Standard Error", "Column Standard Error")), basedescriptiontext = "sample size = 164; total sample size = 182; 18 missing; 77% filtered out", basedescription = list(
            Minimum = 164L, Maximum = 164L, Range = FALSE, Total = 182L,
            Missing = 18L, EffectiveSampleSize = 164L, EffectiveSampleSizeProportion = 100,
            FilteredProportion = 77.25), questiontypes = c("PickOne",
        "PickOne"), span = list(rows = structure(list(c("Less than $15,000",
        "$15,001 to $30,000", "$30,001 to $45,000", "$45,001 to $60,000",
        "$60,001 to $90,000", "$90,001 to $120,000", "$120,001 to $150,000",
        "$150,001 to $200,000", "$200,001 or more", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
        10L)), columns = structure(list(c("Male", "Female", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
        3L))), name = "table.Income.by.Gender.2", questions = c("Income",
        "Gender"))

vec1dA <- structure(c(100, 100, 93.9393939393939, 100, 79.7979797979798,
95.959595959596, 100, 99, 99, 93, 99, 79, 95, 99, 0, 0, 0.0241028760295672,
0, 0.0405583530232473, 0.0198904022396684, 0), .Dim = c(7L, 3L
), .Dimnames = list(c("Coca-Cola", "Diet Coke", "Coke Zero",
"Pepsi", "Diet Pepsi", "Pepsi Max", "NET"), c("%", "Count", "Standard Error"
)), basedescriptiontext = "sample size = 99; 88% filtered out", basedescription = list(
    Minimum = 99L, Maximum = 99L, Range = FALSE, Total = 99L,
    Missing = 0L, EffectiveSampleSize = 99L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 87.625), questiontypes = "PickAny", span = list(
    rows = structure(list(c("Coca-Cola", "Diet Coke", "Coke Zero",
    "Pepsi", "Diet Pepsi", "Pepsi Max", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
    7L))), name = "table.Awareness", questions = c("Awareness",
"SUMMARY"))

vec1dB <- structure(c(100, 100, 90.6593406593407, 100, 82.967032967033,
92.8571428571429, 100, 182, 182, 165, 182, 151, 169, 182, 0,
0, 0.0216299620455664, 0, 0.0279420744290416, 0.0191427511217599,
0), .Dim = c(7L, 3L), .Dimnames = list(c("Coca-Cola", "Diet Coke",
"Coke Zero", "Pepsi", "Diet Pepsi", "Pepsi Max", "NET"), c("%",
"Count", "Standard Error")), basedescriptiontext = "sample size = 182; 77% filtered out", basedescription = list(
    Minimum = 182L, Maximum = 182L, Range = FALSE, Total = 182L,
    Missing = 0L, EffectiveSampleSize = 182L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 77.25), questiontypes = "PickAny", span = list(
    rows = structure(list(c("Coca-Cola", "Diet Coke", "Coke Zero",
    "Pepsi", "Diet Pepsi", "Pepsi Max", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
    7L))), name = "table.Awareness.2", questions = c("Awareness",
"SUMMARY"))


test_that("Table of differences",
{
    expect_error(TableOfDifferences(tb1, tb2), NA)
    expect_error(TableofDifferences(vec1dA, vec1dB))
})
