context("isHexColorFunction")

test_that("TRUE", {
    expect_true(isHexColorFunction(viridis::viridis))
})

test_that("FALSE : not a function", {
    ok <- isHexColorFunction("viridis")
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(cause(ok), noquote('"viridis" is not a function.'))
})

test_that("FALSE : no `n` formal", {
    ok <- isHexColorFunction(ggplot2::scale_colour_manual)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        cause(ok),
        noquote("Hex color function must contain an `n` formal argument.")
    )
})

test_that("nullOK", {
    expect_false(isHexColorFunction(NULL, nullOK = FALSE))
    expect_true(isHexColorFunction(NULL, nullOK = TRUE))
})
