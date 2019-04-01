context("hasDims")

test_that("hasDims", {
    expect_true(hasDims(mtcars))

    # Note that dims don't have to be non-zero, just not NULL.
    expect_true(hasDims(data.frame()))

    ok <- hasDims(list())
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        cause(ok),
        noquote("The dimensions of list() are NULL.")
    )
})

with_parameters_test_that(
    "hasRownames", {
        data <- fun()
        ok <- hasRownames(data)
        expect_false(ok)
        expect_s3_class(ok, "goalie")
        expect_identical(cause(ok), noquote(cause))
    },
    fun = list(
        data.frame,
        DataFrame,
        data.table,
        tibble
    ),
    cause = c(
        "data has sequence row names (soft NULL).",
        "data has NULL row names.",
        "data.table class doesn't support row names.",
        "tibble (tbl_df) class doesn't support row names."
    )
)

with_parameters_test_that(
    "hasRows, hasCols", {
        x <- mtcars
        expect_true(fun(x))

        x <- data.frame()
        expect_false(fun(x))
    },
    fun = list(hasRows, hasCols)
)
