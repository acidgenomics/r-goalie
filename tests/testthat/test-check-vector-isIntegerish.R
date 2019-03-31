context("check : vector : isIntegerish")

test_that("TRUE", {
    expect_identical(isIntegerish(seq_len(2L)), c(TRUE, TRUE))
    expect_identical(isIntegerish(c(1, 2)), c(TRUE, TRUE))  # nolint
})

test_that("FALSE", {
    ok <- isIntegerish(0.1)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        cause(ok),
        noquote("not integer")
    )
})

test_that("FALSE : NA input", {
    ok <- isIntegerish(c(1, 2, NA))  # nolint
    expect_s3_class(ok, "goalie")
    expect_identical(
        as.logical(ok),
        c(TRUE, TRUE, FALSE)
    )
    expect_identical(
        cause(ok),
        noquote(c("", "", "NA"))
    )
})
