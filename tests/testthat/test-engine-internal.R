context("internal")

test_that("toNames complex", {
    object <- complex(length.out = 2L)
    expect_is(object, "complex")
    object <- .toNames(object)
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
