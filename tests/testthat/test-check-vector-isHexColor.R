context("check : vector : isHexColor")

test_that("TRUE", {
    x <- viridis::viridis(n = 2L)
    ok <- isHexColor(x)
    expect_identical(ok, c(`#440154FF` = TRUE, `#FDE725FF` = TRUE))
})

test_that("FALSE", {
    x <- ggplot2::scale_colour_manual
    ok <- isHexColor(x)
    expect_s3_class(ok, "goalie")
    expect_identical(cause(ok), noquote("'x' is not character."))
    expect_false(ok)
})
