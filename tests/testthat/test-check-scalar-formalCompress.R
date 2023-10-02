test_that("TRUE", {
    expect_true(formalCompress("gzip"))
    expect_true(formalCompress(TRUE))
})

test_that("FALSE : unsupported string", {
    ok <- formalCompress("xxx")
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = paste0(
            "{.var xxx} has elements not in ",
            "{.var bzip2, gzip, xz}: xxx"
        )
    )
})

test_that("FALSE : not character", {
    ok <- formalCompress(NULL)
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "{.var NULL} is not any of: character, logical."
    )
})

test_that("FALSE : logical NA is not boolean", {
    ok <- formalCompress(NA)
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "{.var NA} is {.val NA}."
    )
})
