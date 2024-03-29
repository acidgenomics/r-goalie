test_that("TRUE", {
    expect_true(hasElements("hello", n = 1L))
    expect_true(hasElements(list("a" = 1L, "b" = 2L), n = 2L))
})

test_that("FALSE", {
    expect_false(hasElements(NULL))
    expect_false(hasElements(character()))
    expect_false(hasElements(list()))
    ok <- hasElements(list(), n = 1L)
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "{.var list} has 0 elements, not 1."
    )
})

skip_if_not_installed("datasets")

test_that("TRUE", {
    expect_true(isOfDimension(datasets::mtcars, n = c(32L, 11L)))
    expect_true(isOfDimension("xxx", n = NULL))
    expect_true(isOfDimension(list(a = 1L), n = NULL))
})

test_that("FALSE : dimension mismatch", {
    ok <- isOfDimension(datasets::mtcars, n = c(1L, 1L))
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = "Dimensions 1, 2 of {.var data.frame} are incorrect."
    )
})

test_that("FALSE : expecting dim", {
    ok <- isOfDimension(datasets::mtcars, n = NULL)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = paste(
            "{.var data.frame} has dimensions {.val c(32L, 11L)},",
            "not {.val NULL}."
        )
    )
})
