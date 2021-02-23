context("check : scalar : hasSubset")

skip_if_not_installed(pkg = "SummarizedExperiment")

data(
    RangedSummarizedExperiment,
    package = "AcidTest",
    envir = environment()
)
x <- RangedSummarizedExperiment

test_that("FALSE", {
    ok <- hasSubset(x)
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_match(cause(ok), "subset")
})

test_that("TRUE", {
    S4Vectors::metadata(x)[["subset"]] <- TRUE
    ok <- hasSubset(x)
    expect_true(ok)
})
