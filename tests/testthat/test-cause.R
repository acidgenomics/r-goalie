context("cause")

test_that("scalar", {
    x <- FALSE
    cause(x) <- "xxx"
    expect_s3_class(x, "goalie")
    expect_false(x)
    expect_identical(cause(x), noquote("xxx"))
})

test_that("vector", {
    x <- c(FALSE, TRUE)
    expect_error(
        object = cause(x) <- "xxx",
        regexp = "'value' containing multiple elements must be named."
    )
    cause <- rep("XXX", length(x))
    names(cause) <- LETTERS[seq_along(cause)]
    cause(x) <- cause
    expect_s3_class(x, "goalie")
    expect_is(x, "logical")
    expect_identical(cause(x), noquote(cause))
})

test_that("cause assignment mismatch", {
    x <- c(FALSE, FALSE)
    expect_error(
        cause(x) <- c("aaa", "bbb", "ccc"),
        paste(
            "The length of 'value' should be 1 or the length of 'x' \\(2\\),",
            "but is 3."
        )
    )
})
