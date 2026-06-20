test_that("TRUE", {
    expect_true(hasNames(c("a" = 1L, "b" = 2L)))
    expect_true(hasNames(
        data.frame(
            "a" = c("aa", "bb"),
            "b" = c("cc", "dd"),
            stringsAsFactors = FALSE
        )
    ))
})

test_that("FALSE : matrix", {
    ok <- hasNames(matrix())
    expect_false(nocause(ok))
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "The names of {.var NA} are {.val NULL}."
    )
})

test_that("FALSE : data frame", {
    ok <- hasNames(data.frame())
    expect_false(nocause(ok))
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "The names of {.var data.frame} are all empty."
    )
})
