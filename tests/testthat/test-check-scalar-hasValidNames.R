test_that("TRUE", {
    x <- list("a" = 1L, "b" = 2L)
    expect_true(hasValidNames(x))
})

test_that("FALSE", {
    x <- list(
        `1`       = 1L, # can't start with number
        `foo bar` = 2L, # no spaces
        `foo-bar` = 3L # no hyphens
    )
    ok <- hasValidNames(x)
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_match(
        object = cause(ok),
        regexp = "valid names"
    )
})

test_that("Unset names", {
    x <- data.frame()
    ok <- hasValidNames(x)
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "{.var data.frame} doesn't have names."
    )
})



skip_if_not_installed("datasets")

test_that("TRUE", {
    x <- datasets::iris
    expect_true(hasValidDimnames(x))
})

test_that("FALSE", {
    ## Note the spaces in the row names here.
    x <- datasets::mtcars
    ok <- hasValidDimnames(x)
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_match(
        object = cause(ok),
        regexp = "valid names"
    )
    x <- data.frame(
        `1` = "a",
        `2` = "b",
        check.names = FALSE
    )
    ok <- hasValidDimnames(x)
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_match(
        object = cause(ok),
        regexp = "valid names"
    )
})
