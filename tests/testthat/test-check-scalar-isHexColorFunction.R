context("check : scalar : isHexColorFunction")

test_that("TRUE", {
    expect_true(isHexColorFunction(viridis::viridis))
})

test_that("FALSE", {
    ok <- isHexColorFunction(ggplot2::scale_colour_manual)
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        cause(ok),
        noquote("Hex color function must contain an `n` formal argument.")
    )
})
