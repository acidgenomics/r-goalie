context("validateClasses")

test_that("validateClasses", {
    expect_true(
        validateClasses(
            object = list(
                a = character(),
                b = integer(),
                c = factor()
            ),
            expected = list(
                a = "character",
                b = "integer",
                c = "factor"
            )
        )
    )

    expect_identical(
        validateClasses(
            object = list(
                a = character()
            ),
            expected = list(
                a = "integer"
            )
        ),
        "Class checks failed:\n[1] \"a\""
    )
})

test_that("Subset mode", {
    expect_true(
        validateClasses(
            object = list(
                a = character(),
                b = integer(),
                c = factor()
            ),
            expected = list(
                a = "character",
                b = "integer"
            ),
            subset = TRUE
        )
    )
    expect_error(
        validateClasses(
            object = list(
                a = character(),
                b = integer(),
                c = factor()
            ),
            expected = list(
                a = "character",
                b = "integer"
            ),
            subset = FALSE
        )
    )
})
