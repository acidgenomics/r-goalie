context("goalie")

test_that("Multiple FALSE", {
    ok <- c(FALSE, FALSE)
    cause <- c("AAA", "BBB")
    object <- goalie(object = ok, cause = cause)
    expect_s4_class(object, "goalie")
    expect_identical(
        object = cause(object),
        expected = cause
    )
})

test_that("Multiple TRUE", {
    ok <- c(TRUE, TRUE)
    object <- goalie(object = ok)
    expect_s4_class(object, "goalie")
    expect_identical(
        object = cause(object),
        expected = rep(x = NA_character_, times = length(object))
    )
})
