context("getNameInParent")

test_that("getNameInParent", {
    fun <- function(x, .xname = getNameInParent(x)) { .xname }
    expect_identical(
        object = fun(x = "XXX"),
        expected = "\"XXX\""
    )
})
