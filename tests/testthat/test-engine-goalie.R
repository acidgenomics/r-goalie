test_that("All TRUE", {
    expect_true(goalie(TRUE))
    expect_identical(
        object = goalie(rep(TRUE, 2L)),
        expected = rep(TRUE, 2L)
    )
})

test_that("Any FALSE", {
    expect_error(
        object = goalie(
            object = c(FALSE, FALSE),
            cause = c("AAA", "BBB")
        ),
        regexp = "Cause attribute doesn't have names assigned."
    )
    object <- goalie(
        object = c(FALSE, FALSE),
        cause = c("1" = "AAA", "2" = "BBB")
    )
    expect_s4_class(object, "goalie")
    expect_identical(
        object = cause(object),
        expected = c("1" = "AAA", "2" = "BBB")
    )
})

test_that("Invalid input", {
    expect_error(
        object = goalie(logical()),
        regexp = "Invalid input."
    )
    expect_error(
        object = goalie(
            object = c(FALSE, TRUE, NA),
            cause = c("1" = "aaa", "2" = NA_character_, "3" = "ccc")
        ),
        regexp = "Object contains NA."
    )
})
