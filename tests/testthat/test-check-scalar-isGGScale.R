skip_if_not_installed("ggplot2")

cc <- ggplot2::scale_color_gradient(low = "red", high = "blue")
cd <- ggplot2::scale_color_manual(values = c("red", "blue"))
fc <- ggplot2::scale_fill_gradient(low = "red", high = "blue")
fd <- ggplot2::scale_fill_manual(values = c("red", "blue"))

test_that("TRUE", {
    expect_true(isGGScale(x = cc, scale = "continuous", aes = "color"))
    expect_true(isGGScale(x = cd, scale = "discrete", aes = "color"))
    expect_true(isGGScale(x = fc, scale = "continuous", aes = "fill"))
    expect_true(isGGScale(x = fd, scale = "discrete", aes = "fill"))
})

test_that("FALSE", {
    expect_false(isGGScale(x = cd, scale = "continuous", aes = "color"))
    expect_false(isGGScale(x = cc, scale = "discrete", aes = "color"))
    expect_false(isGGScale(x = fd, scale = "continuous", aes = "fill"))
    expect_false(isGGScale(x = fc, scale = "discrete", aes = "fill"))
})

test_that("nullOK", {
    expect_true(isGGScale(x = NULL, nullOK = TRUE))
    expect_false(isGGScale(x = NULL, nullOK = FALSE))
})
