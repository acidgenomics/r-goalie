test_that("All FALSE", {
    ok <- setCause(object = FALSE, false = "xxx")
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = "xxx"
    )
})

test_that("Any FALSE", {
    ok <- setCause(
        object = c(TRUE, FALSE),
        false = "custom false"
    )
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = nocause(ok),
        expected = c(TRUE, FALSE)
    )
    expect_identical(
        object = cause(ok),
        expected = c(
            NA_character_,
            "custom false"
        )
    )
})
