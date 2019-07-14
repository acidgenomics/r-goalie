context("internal")

test_that(".capitalize", {
    expect_null(.capitalize(x = NULL))
    expect_identical(
        .capitalize(c("hello", NA)),
        c("Hello", NA)
    )
})

test_that(".checkN", {
    expect_silent(.checkN(1L))
    expect_error(.checkN(1.1))
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
    # Complex number
    expect_identical(
        object = .toNames(complex(length.out = 1)),
        expected = "0+0i"
    )
})

test_that(".useFirst", {
    expect_error(
        object = .useFirst(NULL),
        regexp = "length 0"
    )
    expect_identical(.useFirst("a"), "a")
    expect_warning(
        object = .useFirst(letters[seq_len(3L)]),
        expr = "Only the first value"
    )
})
