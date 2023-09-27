skip_if_not_installed("ggplot2")

cc <- ggplot2::scale_color_gradient(low = "red", high = "blue")
cd <- ggplot2::scale_color_manual(values = c("red", "blue"))
fc <- ggplot2::scale_fill_gradient(low = "red", high = "blue")
fd <- ggplot2::scale_fill_manual(values = c("red", "blue"))

test_that("TRUE", {
    expect_true(isGgscale(x = cc, scale = "continuous", aes = "color"))
    expect_true(isGgscale(x = cd, scale = "discrete", aes = "color"))
    expect_true(isGgscale(x = fc, scale = "continuous", aes = "fill"))
    expect_true(isGgscale(x = fd, scale = "discrete", aes = "fill"))
})

test_that("FALSE", {
    expect_false(isGgscale(x = cd, scale = "continuous", aes = "color"))
    expect_false(isGgscale(x = cc, scale = "discrete", aes = "color"))
    expect_false(isGgscale(x = fd, scale = "continuous", aes = "fill"))
    expect_false(isGgscale(x = fc, scale = "discrete", aes = "fill"))
})

test_that("nullOk", {
    expect_true(isGgscale(x = NULL, nullOk = TRUE))
    expect_false(isGgscale(x = NULL, nullOk = FALSE))
})
