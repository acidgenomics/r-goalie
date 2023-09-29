## FIXME Move this to a separate unit test.
test_that("toCauseNames complex", {
    object <- complex(length.out = 2L)
    expect_type(object, "complex")
    object <- toCauseNames(object)
    expect_identical(
        object = object,
        expected = c("0+0i", "0+0i")
    )
})

test_that("typeDescription function", {
    expect_identical(
        object = .typeDescription(class),
        expected = "class 'builtin function'"
    )
})
