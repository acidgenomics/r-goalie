context("engine : validateClasses")

test_that("validateClasses", {
    expect_true(
        validateClasses(
            object = list(
                "a" = character(),
                "b" = integer(),
                "c" = factor()
            ),
            expected = list(
                "a" = "character",
                "b" = "integer",
                "c" = "factor"
            )
        )
    )
    expect_identical(
        object = validateClasses(
            object = list(
                "a" = character()
            ),
            expected = list(
                "a" = "integer"
            )
        ),
        expected = paste0(
            "Class checks failed: a.\n",
            "If supported, 'updateObject()' may help resolve these issues."
        )
    )
})

test_that("Subset mode", {
    expect_true(
        validateClasses(
            object = list(
                "a" = character(),
                "b" = integer(),
                "c" = factor()
            ),
            expected = list(
                "a" = "character",
                "b" = "integer"
            ),
            subset = TRUE
        )
    )
    expect_error(
        validateClasses(
            object = list(
                "a" = character(),
                "b" = integer(),
                "c" = factor()
            ),
            expected = list(
                "a" = "character",
                "b" = "integer"
            ),
            subset = FALSE
        )
    )
})
