context("check : scalar : hasUniqueCols")

test_that("TRUE", {
    x <- matrix(data = seq_len(20L), ncol = 2L)
    expect_true(hasUniqueCols(x))
})

test_that("Duplicated columns", {
    x <- matrix(data = rep(seq_len(10L), times = 2L), ncol = 2L)
    ok <- hasUniqueCols(x)
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_match(
        as.character(cause(ok)),
        "has duplicated columns"
    )
})

test_that("1 column", {
    x <- matrix(data = rep(seq_len(10L), times = 1L), ncol = 1L)
    ok <- hasUniqueCols(x)
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "{.var x} doesn't have >= 2 columns."
    )
})

test_that("SummarizedExperiment", {
    skip_if_not_installed("SummarizedExperiment")
    object <- SummarizedExperiment::SummarizedExperiment(
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
    expect_true(hasUniqueCols(object))
})
