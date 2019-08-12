context("check : scalar : hasUniqueCols")

test_that("TRUE", {
    x <- matrix(data = seq_len(20L), ncol = 2L)
    expect_true(hasUniqueCols(x))
})

test_that("Duplicated columns", {
    x <- matrix(data = rep(seq_len(10L), times = 2L), ncol = 2L)
    ok <- hasUniqueCols(x)
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_match(
        as.character(cause(ok)),
        "has duplicated columns"
    )
})

test_that("1 column", {
    x <- matrix(data = rep(seq_len(10L), times = 1L), ncol = 1L)
    ok <- hasUniqueCols(x)
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        cause(ok),
        noquote("'x' does not have >= 2 columns.")
    )
})

test_that("SummarizedExperiment", {
    expect_true(hasUniqueCols(se))
})
