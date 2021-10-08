context("check : vector : isHexColor")

test_that("TRUE", {
    x <- viridis::viridis(n = 2L)
    ok <- isHexColor(x)
    expect_identical(
        object = ok,
        expected = c("#440154FF" = TRUE, "#FDE725FF" = TRUE)
    )
})

test_that("FALSE", {
    x <- ggplot2::scale_color_manual
    ok <- isHexColor(x)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "{.var x} is not character."
    )
    expect_false(ok)
})
