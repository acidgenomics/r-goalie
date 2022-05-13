test_that("FALSE", {
    ok <- setCause(object = FALSE, false = "xxx")
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(cause(ok), "xxx")
})

test_that("NA logical (missing)", {
    ok <- setCause(
        object = c(a = TRUE, b = FALSE, c = NA),
        false = "custom false",
        missing = "custom missing"
    )
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = nocause(ok),
        expected = c("a" = TRUE, "b" = FALSE, "c" = NA)
    )
    expect_identical(
        object = cause(ok),
        expected = c(
            "a" = NA_character_,
            "b" = "custom false",
            "c" = "custom missing"
        )
    )
})
