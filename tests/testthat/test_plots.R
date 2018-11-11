context("Plots")

test_that("assertIsColorScaleContinuousOrNULL", {
    x <- ggplot2::scale_color_gradient(low = "red", high = "blue")
    expect_silent(assertIsColorScaleContinuousOrNULL(x))
    expect_silent(assertIsColorScaleContinuousOrNULL(NULL))
    x <- ggplot2::scale_color_manual(values = "red")
    expect_error(assertIsColorScaleContinuousOrNULL(x))
})

test_that("assertIsColorScaleDiscreteOrNULL", {
    x <- ggplot2::scale_color_manual(values = "red")
    expect_silent(assertIsColorScaleDiscreteOrNULL(x))
    expect_silent(assertIsColorScaleDiscreteOrNULL(NULL))
    x <- ggplot2::scale_color_gradient(low = "red", high = "blue")
    expect_error(assertIsColorScaleDiscreteOrNULL(x))
})

test_that("assertIsFillScaleContinuousOrNULL", {
    x <- ggplot2::scale_fill_gradient(low = "red", high = "blue")
    expect_silent(assertIsFillScaleContinuousOrNULL(x))
    expect_silent(assertIsFillScaleContinuousOrNULL(NULL))
    x <- ggplot2::scale_fill_manual(values = "red")
    expect_error(assertIsFillScaleContinuousOrNULL(x))
})

test_that("assertIsFillScaleDiscreteOrNULL", {
    x <- ggplot2::scale_fill_manual(values = "red")
    expect_silent(assertIsFillScaleDiscreteOrNULL(x))
    expect_silent(assertIsFillScaleDiscreteOrNULL(NULL))
    x <- ggplot2::scale_fill_gradient(low = "red", high = "blue")
    expect_error(assertIsFillScaleDiscreteOrNULL(x))
})

test_that("assertIsHexColorFunctionOrNULL", {
    x <- function(n) {
        colors <- c("#FFFFFF", "#000000")
        colors[seq_len(n)]
    }
    expect_silent(assertIsHexColorFunctionOrNULL(x))
    expect_error(assertIsHexColorFunctionOrNULL(x(2L)))
    # Check viridis trailing "FF" sanitization support.
    viridis <- function(n = 2L) {
        colors <- c("#440154FF", "#FDE725FF")
        colors[n]
    }
    expect_silent(assertIsHexColorFunctionOrNULL(viridis))
})
