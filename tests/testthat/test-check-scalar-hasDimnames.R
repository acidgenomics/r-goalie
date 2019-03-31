context("hasDimnames")

with_parameters_test_that(
    "hasDimnames", {
        x <- mtcars
        expect_true(fun(x))

        x <- data.frame()
        ok <- fun(x)
        expect_false(ok)
        expect_s3_class(ok, "goalie")
    },
    fun = list(
        hasDimnames,
        hasRownames,
        hasColnames
    )
)
