context("internal")

test_that(".capitalize", {
    expect_null(.capitalize(x = NULL))
    expect_identical(
        .capitalize(c("hello", NA)),
        c("Hello", NA)
    )
})

test_that(".dim", {
    expect_identical(.dim(seq(2L)), 2L)
    expect_identical(.dim(mtcars), c(32L, 11L))
})

test_that(".getMetric", {
    expect_is(.getMetric("length"), "function")
    expect_error(
        object = .getMetric("XXX"),
        regexp = "not valid"
    )
})

test_that(".toNames", {
    ## Complex number.
    expect_identical(
        object = .toNames(complex(length.out = 1L)),
        expected = "0+0i"
    )
})

test_that(".typeDescription", {
    expect_identical(
        object = .typeDescription(base::save),
        expected = "class 'closure function'"
    )
})
