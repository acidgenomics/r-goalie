context("isEmpty")

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
        expected = noquote('"" has length 1, not 0.')
    )

    expect_false(isEmpty(1L, metric = "length"))
    expect_false(isEmpty("", metric = "elements"))
})



context("isNonEmpty")

test_that("TRUE", {
    expect_true(isNonEmpty(1L, metric = "length"))
    expect_true(isNonEmpty(1L, metric = "elements"))
    # Note that `""` returns TRUE, which may not be desirable.
    expect_true(isNonEmpty("", metric = "length"))
    expect_true(isNonEmpty("", metric = "elements"))
})

test_that("FALSE", {
    expect_false(isNonEmpty(character()))
    expect_false(isNonEmpty(NULL))
})



context("hasElements")

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
        expected = noquote("list() has 0 elements, not 1.")
    )
})



context("isOfDimension")

test_that("TRUE", {
    expect_true(isOfDimension(mtcars, n = c(32L, 11L)))
    expect_true(isOfDimension("xxx", n = NULL))
})

test_that("FALSE : list doesn't support dim", {
    ok <- isOfDimension(list(a = 1L), n = 1L)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = noquote("list(a = 1L) has 0 dimensions, not 1.")
    )
})

test_that("FALSE : dimension mismatch", {
    ok <- isOfDimension(mtcars, n = c(1L, 1L))
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = noquote("Dimensions 1, 2 of mtcars are incorrect.")
    )
})

test_that("FALSE : expecting dim", {
    ok <- isOfDimension(mtcars, n = NULL)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = noquote("mtcars has dimensions c(32L, 11L), not NULL.")
    )
})



context("isOfLength")

test_that("TRUE", {
    expect_true(isOfLength("xxx", n = 1L))
    expect_true(isOfLength(NA, n = 1L))
    expect_true(isOfLength(character(), n = 0L))
    expect_true(isOfLength(NULL, n = 0L))
})

test_that("FALSE", {
    expect_false(isOfLength("xxx", n = 2L))
})
