skip_if_not_installed("BiocParallel")

test_that("TRUE", {
    ok <- isBiocParallelParam(BiocParallel::bpparam())
    expect_true(ok)
})

test_that("FALSE", {
    ok <- isBiocParallelParam(list())
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = "{.var list()} is not a BiocParallel param."
    )
})
