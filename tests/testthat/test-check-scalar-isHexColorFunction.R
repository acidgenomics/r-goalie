test_that("TRUE", {
    expect_true(isHexColorFunction(viridis::viridis))
})

test_that("FALSE : not a function", {
    ok <- isHexColorFunction("viridis")
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "{.var viridis} is not a function."
    )
})

test_that("FALSE : no 'n' formal", {
    ok <- isHexColorFunction(ggplot2::scale_color_manual)
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "{.var function} doesn't contain an {.arg n} argument."
    )
})

test_that("nullOk", {
    expect_false(isHexColorFunction(NULL, nullOk = FALSE))
    expect_true(isHexColorFunction(NULL, nullOk = TRUE))
})

test_that("Function with 'n' formal that isn't supported", {
    f <- function(n) {
        invisible()
    }
    ok <- isHexColorFunction(f)
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "{.var function} function didn't return any hex colors."
    )
    f <- function(n) {
        "XXX"
    }
    ok <- isHexColorFunction(f)
    expect_false(ok)
    expect_s4_class(ok, "goalie")
})
