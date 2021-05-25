context("check : scalar : hasDimnames")

test_that("hasDimnames", {
    for (fun in list(
        hasDimnames,
        hasRownames,
        hasColnames
    )) {
        x <- mtcars
        expect_true(fun(x))

        x <- data.frame()
        ok <- fun(x)
        expect_false(ok)
        expect_s4_class(ok, "goalie")
    }
})
