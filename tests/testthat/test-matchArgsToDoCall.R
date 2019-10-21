context("matchArgsToDoCall")

test_that("Match the arguments in call.", {
    fun <- function(object, xxx, ...) {
        args <- matchArgsToDoCall(
            args = list(object = object, collapse = " "),
            removeFormals = "xxx",
            verbose = FALSE
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
})

test_that("Pass the arguments to 'do.call()'.", {
    fun <- function(object, xxx, ...) {
        do.call(
            what = paste,
            args = matchArgsToDoCall(
                args = list(collapse = " "),
                removeFormals = "xxx",
                verbose = FALSE
            )
        )
    }
    expect_identical(
        fun(c("hello", "world")),
        "hello world"
    )
})
