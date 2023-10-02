test_that("TRUE", {
    skip_if_not_installed("viridis")
    x <- viridis::viridis(n = 2L)
    ok <- isHexColor(x)
    expect_identical(ok, rep(TRUE, 2L))
})

test_that("FALSE", {
    skip_if_not_installed("ggplot2")
    x <- ggplot2::scale_color_manual
    ok <- isHexColor(x)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "not character"
    )
    expect_false(ok)
})
