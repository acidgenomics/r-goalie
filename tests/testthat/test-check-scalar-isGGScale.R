context("check : scalar : isGGScale")

library(ggplot2)

## nolint start
colour_c <- scale_colour_gradient(low = "red", high = "blue")
colour_d <- scale_colour_manual(values = c("red", "blue"))
fill_c <- scale_fill_gradient(low = "red", high = "blue")
fill_d <- scale_fill_manual(values = c("red", "blue"))
## nolint end

test_that("TRUE", {
    expect_true(isGGScale(x = colour_c, scale = "continuous", aes = "colour"))
    expect_true(isGGScale(x = colour_d, scale = "discrete", aes = "colour"))
    expect_true(isGGScale(x = fill_c, scale = "continuous", aes = "fill"))
    expect_true(isGGScale(x = fill_d, scale = "discrete", aes = "fill"))
})

test_that("FALSE", {
    expect_false(isGGScale(x = colour_d, scale = "continuous", aes = "colour"))
    expect_false(isGGScale(x = colour_c, scale = "discrete", aes = "colour"))
    expect_false(isGGScale(x = fill_d, scale = "continuous", aes = "fill"))
    expect_false(isGGScale(x = fill_c, scale = "discrete", aes = "fill"))
})

test_that("nullOK", {
    expect_true(isGGScale(x = NULL, nullOK = TRUE))
    expect_false(isGGScale(x = NULL, nullOK = FALSE))
})
