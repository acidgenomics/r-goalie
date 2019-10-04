context("check : scalar : isEmpty")

test_that("TRUE", {
    expect_true(isEmpty(NULL))
    expect_true(isEmpty(character()))
})

test_that("FALSE", {
    ok <- isEmpty("")
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = noquote("'\"\"' does not have a length of 0.")
    )
    expect_false(isEmpty(1L))
})



context("check : scalar : isNonEmpty")

test_that("TRUE", {
    expect_true(isNonEmpty(1L))
    ## Note that `""` returns TRUE, which may not be desirable.
    expect_true(isNonEmpty(""))
})

test_that("FALSE", {
    expect_false(isNonEmpty(character()))
    expect_false(isNonEmpty(NULL))
})
