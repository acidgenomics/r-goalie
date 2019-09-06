context("appendToBody")

test_that("appendToBody", {
    ## Add a deprecation call into function body.
    x <- function() {
        "hello"
    }
    x <- appendToBody(
        fun = x,
        values = list(
            quote(a <- "aaa"),
            quote(b <- "bbb")
        )
    )
    expect_is(x, "function")
    ## This check approach using `subsitute()` is recommended in `body()`
    ## working example documentation.
    expect_identical(
        body(x),
        substitute({
            a <- "aaa"
            b <- "bbb"
            "hello"
        })
    )
})
