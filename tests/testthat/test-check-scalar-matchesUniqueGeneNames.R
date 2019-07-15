context("matchesUniqueGeneNames")

test_that("TRUE", {
    ok <- matchesUniqueGeneNames(x = se, genes = genes)
    expect_true(ok)
})

test_that("Not an S4 object", {
    ok <- matchesUniqueGeneNames(x = assay(se), genes = genes)
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        cause(ok),
        noquote("Not an S4 class object.")
    )
})

test_that("Invalid genes input", {
    ok <- matchesUniqueGeneNames(x = se, genes = NULL)
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        cause(ok),
        noquote("genes is not character.")
    )
})

test_that("Non-unique gene names", {
    rowData(se)[["geneName"]][[2L]] <- rowData(se)[["geneName"]][[1L]]
    genes <- rowData(se)[["geneName"]]
    ok <- matchesUniqueGeneNames(x = se, genes = genes)
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        cause(ok),
        noquote("Non-unique gene names: SYMBOL1")
    )
})

test_that("No gene names defined in object", {
    rowData(se)[["geneName"]] <- NULL
    ok <- matchesUniqueGeneNames(x = se, genes = genes)
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        cause(ok),
        noquote("Gene names are not defined in object.")
    )
})

test_that("User-requested genes that aren't defined", {
    ok <- matchesUniqueGeneNames(x = se, genes = c("XXXXXX", "YYYYYY"))
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        cause(ok),
        noquote("Genes missing: XXXXXX, YYYYYY")
    )
})
