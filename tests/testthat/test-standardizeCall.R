context("standardizeCall")

aaa <- "AAA"
bbb <- "BBB"

test_that("Standard function", {
    testing <- function(a, b) {
        standardizeCall()
    }
    expect_identical(
        deparse(testing(aaa, bbb)),
        "testing(a = aaa, b = bbb)"
    )
})

test_that("Inside S4 method", {
    setGeneric(
        name = "testing",
        def = function(a, b, ...) {
            standardGeneric("testing")
        }
    )
    setMethod(
        f = "testing",
        signature = signature("character"),
        definition = function(a, b, ...) {
            standardizeCall()
        }
    )
    expect_identical(
        deparse(testing(aaa, bbb)),
        "testing(a = aaa, b = bbb)"
    )
})

## Note that the method contains formals that aren't in the generic here.
test_that("Inside S4 method with .local", {
    setGeneric(
        name = "testing",
        def = function(a, ...) {
            standardGeneric("testing")
        }
    )
    setMethod(
        f = "testing",
        signature = signature("character"),
        definition = function(a, b, ...) {
            standardizeCall()
        }
    )
    expect_identical(
        deparse(testing(aaa, bbb)),
        "testing(a = aaa, b = bbb)"
    )
})
