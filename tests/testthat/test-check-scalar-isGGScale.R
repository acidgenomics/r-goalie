library(ggplot2)

## nolint start
color_c <- scale_color_gradient(low = "red", high = "blue")
color_d <- scale_color_manual(values = c("red", "blue"))
fill_c <- scale_fill_gradient(low = "red", high = "blue")
fill_d <- scale_fill_manual(values = c("red", "blue"))
## nolint end

test_that("TRUE", {
    expect_true(isGGScale(x = color_c, scale = "continuous", aes = "color"))
    expect_true(isGGScale(x = color_d, scale = "discrete", aes = "color"))
    expect_true(isGGScale(x = fill_c, scale = "continuous", aes = "fill"))
    expect_true(isGGScale(x = fill_d, scale = "discrete", aes = "fill"))
})

test_that("FALSE", {
    expect_false(isGGScale(x = color_d, scale = "continuous", aes = "color"))
    expect_false(isGGScale(x = color_c, scale = "discrete", aes = "color"))
    expect_false(isGGScale(x = fill_d, scale = "continuous", aes = "fill"))
    expect_false(isGGScale(x = fill_c, scale = "discrete", aes = "fill"))
})

test_that("nullOK", {
    expect_true(isGGScale(x = NULL, nullOK = TRUE))
    expect_false(isGGScale(x = NULL, nullOK = FALSE))
})
