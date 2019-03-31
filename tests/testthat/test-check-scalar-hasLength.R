context("check : scalar : hasLength")

test_that("TRUE", {
    expect_true(hasLength(1L))
    expect_true(hasLength(FALSE))
    expect_true(hasLength(mtcars))
    expect_true(hasLength(""))
})

test_that("FALSE : NULL", {
    ok <- hasLength(NULL)
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        cause(ok),
        noquote("NULL has length 0.")
    )
})

test_that("FALSE : empty character", {
    ok <- hasLength(character())
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        cause(ok),
        noquote("character() has length 0.")
    )
})

test_that("FALSE : empty data frame", {
    ok <- hasLength(data.frame())
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        cause(ok),
        noquote("data.frame() has length 0.")
    )
})
