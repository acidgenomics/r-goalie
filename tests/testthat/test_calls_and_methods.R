context("calls and methods")



test_that("MethodDefinition", {
    # Use `substitute()` to put a missing argument in pairlist.
    # https://stackoverflow.com/questions/3892580
    formals <- pairlist(
        "x" = substitute(),
        row.names = NULL,
        optional = FALSE,
        "..." = substitute()
    )

    # Function
    x <- methodFunction(
        f = "as.data.frame",
        signature = "ANY",
        package = "S4Vectors"
    )
    expect_is(x, "function")
    expect_identical(formals(x), formals)

    # Formals
    x <- methodFormals(
        f = "as.data.frame",
        signature = "ANY",
        package = "S4Vectors"
    )
    expect_identical(x, formals)
})



test_that("appendToBody", {
    # Add a deprecation call into function body.
    x <- function() {
        "hello"
    }
    x <- appendToBody(x, quote(.Deprecated("y")))
    expect_is(x, "function")
    # This check approach using `subsitute()` is recommended in `body()` working
    # example documentation.
    expect_identical(
        body(x),
        substitute({
            .Deprecated("y")
            "hello"
        })
    )
})



test_that("matchArgsToDoCall", {
    # Match the arguments in call.
    fun <- function(object, xxx, ...) {
        args <- matchArgsToDoCall(
            args = list(object = object, collapse = " "),
            removeFormals = "xxx"
        )
        args
    }
    expect_identical(
        fun(c("hello", "world")),
        list(
            object = c("hello", "world"),
            collapse = " "
        )
    )

    # Pass the arguments to `do.call()`.
    fun <- function(object, xxx, ...) {
        do.call(
            what = paste,
            args = matchArgsToDoCall(
                args = list(collapse = " "),
                removeFormals = "xxx"
            )
        )
    }
    expect_identical(
        fun(c("hello", "world")),
        "hello world"
    )
})



test_that("standardizeCall", {
    aaa <- "AAA"
    bbb <- "BBB"

    # Standard function.
    testing <- function(a, b) {
        standardizeCall()
    }
    expect_identical(
        deparse(testing(aaa, bbb)),
        "testing(a = aaa, b = bbb)"
    )

    # Inside S4 method.
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
