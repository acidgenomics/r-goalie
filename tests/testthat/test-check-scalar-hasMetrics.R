context("check : scalar : hasMetrics")

skip_if_not_installed(pkg = "SummarizedExperiment")

data(
    RangedSummarizedExperiment,
    package = "AcidTest",
    envir = environment()
)
x <- RangedSummarizedExperiment

test_that("FALSE", {
    ok <- hasMetrics(x)
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_match(cause(ok), "nCount, nFeature")
})

test_that("TRUE", {
    SummarizedExperiment::colData(x)[["XXX"]] <- TRUE
    ok <- hasMetrics(x, colData = "XXX")
    expect_true(ok)
})
