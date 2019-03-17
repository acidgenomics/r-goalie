context("Validity check function")

test_that("validate", {
    expect_true(
        validate(is.integer(1L))
    )
    expect_identical(
        validate(is.character(1L)),
        "is.character(1L) is not TRUE."
    )

})

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
