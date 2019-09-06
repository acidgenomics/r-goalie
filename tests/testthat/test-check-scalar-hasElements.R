context("check : scalar : isEmpty")

test_that("TRUE", {
    expect_true(isEmpty(NULL, metric = "length"))
    expect_true(isEmpty(NULL, metric = "elements"))
    expect_true(isEmpty(character(), metric = "length"))
    expect_true(isEmpty(character(), metric = "elements"))
})

test_that("FALSE", {
    ok <- isEmpty("", metric = "length")
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = noquote("'\"\"' does not have a length of 0.")
    )

    expect_false(isEmpty(1L, metric = "length"))
    expect_false(isEmpty("", metric = "elements"))
})



context("check : scalar : isNonEmpty")

test_that("TRUE", {
    expect_true(isNonEmpty(1L, metric = "length"))
    expect_true(isNonEmpty(1L, metric = "elements"))
    ## Note that `""` returns TRUE, which may not be desirable.
    expect_true(isNonEmpty("", metric = "length"))
    expect_true(isNonEmpty("", metric = "elements"))
})

test_that("FALSE", {
    expect_false(isNonEmpty(character()))
    expect_false(isNonEmpty(NULL))
})



context("check : scalar : hasElements")

test_that("TRUE", {
    expect_true(hasElements("hello", n = 1L))
    expect_true(hasElements(list(a = 1L, b = 2L), n = 2L))
})

test_that("FALSE", {
    ok <- hasElements(list(), n = 1L)
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = noquote("'list()' has 0 elements, not 1.")
    )
})



context("check : scalar : isOfDimension")

test_that("TRUE", {
    expect_true(isOfDimension(mtcars, n = c(32L, 11L)))
    expect_true(isOfDimension("xxx", n = NULL))
    expect_true(isOfDimension(list(a = 1L), n = NULL))
})

test_that("FALSE : dimension mismatch", {
    ok <- isOfDimension(mtcars, n = c(1L, 1L))
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = noquote("Dimensions 1, 2 of 'mtcars' are incorrect.")
    )
})

test_that("FALSE : expecting dim", {
    ok <- isOfDimension(mtcars, n = NULL)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = noquote("'mtcars' has dimensions c(32L, 11L), not NULL.")
    )
})
