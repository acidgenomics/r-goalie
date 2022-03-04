context("check : scalar : matchesUniqueGeneNames")

skip_if_not_installed("SummarizedExperiment")

se <- SummarizedExperiment::SummarizedExperiment(
    assays = matrix(
        data = seq_len(16L),
        nrow = 4L,
        ncol = 4L,
        dimnames = list(
            paste0("gene", seq_len(4L)),
            paste0("sample", seq_len(4L))
        )
    ),
    rowData = S4Vectors::DataFrame(
        "geneId" = paste0("ENSG0000000000", seq_len(4L)),
        "geneName" = paste0("SYMBOL", seq_len(4L))
    )
)
genes <- SummarizedExperiment::rowData(se)[["geneName"]]

test_that("TRUE", {
    ok <- matchesUniqueGeneNames(x = se, genes = genes)
    expect_true(ok)
})

test_that("Not an S4 object", {
    ok <- matchesUniqueGeneNames(
        x = SummarizedExperiment::assay(se),
        genes = genes
    )
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = paste(
            "{.var SummarizedExperiment::assay(se)}",
            "is not an S4 class object."
        )
    )
})

test_that("Invalid genes input", {
    ok <- matchesUniqueGeneNames(x = se, genes = NULL)
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "{.var genes} is not character."
    )
})

test_that("Non-unique gene names", {
    SummarizedExperiment::rowData(se)[["geneName"]][[2L]] <-
        SummarizedExperiment::rowData(se)[["geneName"]][[1L]]
    genes <- SummarizedExperiment::rowData(se)[["geneName"]]
    ok <- matchesUniqueGeneNames(x = se, genes = genes)
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_match(cause(ok), "SYMBOL1")
})

test_that("No gene names defined in object", {
    SummarizedExperiment::rowData(se)[["geneName"]] <- NULL
    ok <- matchesUniqueGeneNames(x = se, genes = genes)
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_match(cause(ok), "Gene names are not defined")
})

test_that("User-requested genes that aren't defined", {
    ok <- matchesUniqueGeneNames(x = se, genes = c("XXXXXX", "YYYYYY"))
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_match(cause(ok), "XXXXXX, YYYYYY")
})
