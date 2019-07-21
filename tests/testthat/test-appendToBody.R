context("appendToBody")

test_that("appendToBody", {
    ## Add a deprecation call into function body.
    x <- function() {
        "hello"
    }
    x <- appendToBody(x, quote(.Deprecated("y")))
    expect_is(x, "function")
    ## This check approach using `subsitute()` is recommended in `body()`
    ## working example documentation.
    expect_identical(
        body(x),
        substitute({
            .Deprecated("y")
            "hello"
        })
    )
})
