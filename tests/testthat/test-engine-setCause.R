context("engine : setCause")

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
        nocause(ok),
        c(a = TRUE, b = FALSE, c = NA)
    )
    expect_identical(
        cause(ok),
        c(
            "a" = NA_character_,
            "b" = "custom false",
            "c" = "custom missing"
        )
    )
})
