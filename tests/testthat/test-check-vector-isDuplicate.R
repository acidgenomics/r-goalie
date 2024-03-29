test_that("character", {
    expect_identical(
        object = isDuplicate(c("a", "a")),
        expected = rep(TRUE, 2L)
    )
    ok <- isDuplicate(c("a", "a", "b", "b", "c", "d", NA, NA))
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = nocause(ok),
        expected = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE)
    )
    expect_identical(
        object = cause(ok),
        expected = c(
            rep(NA, 4L),
            rep("unique", 2L),
            rep(NA, 2L)
        )
    )
    ok <- isDuplicate(c("a", "b", "c"))
    expect_s4_class(ok, "goalie")
    expect_identical(nocause(ok), rep(FALSE, 3L))
})

test_that("numeric", {
    expect_identical(
        object = isDuplicate(rep(1L, 2L)),
        expected = rep(TRUE, 2L)
    )
    ok <- isDuplicate(c(1L, 1L, 2L, 2L, 3L, 4L, NA, NA))
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = nocause(ok),
        expected = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE)
    )
    expect_identical(
        object = cause(ok),
        expected = c(
            rep(NA, 4L),
            rep("unique", 2L),
            rep(NA, 2L)
        )
    )
    ok <- isDuplicate(c(1L, 2L, 3L))
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = nocause(ok),
        expected = rep(FALSE, 3L)
    )
})

test_that("logical", {
    expect_identical(
        object = isDuplicate(c(FALSE, FALSE)),
        expected = c(TRUE, TRUE)
    )
    ok <- isDuplicate(c(FALSE, TRUE))
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = nocause(ok),
        expected = rep(FALSE, 2L)
    )
})

test_that("Invalid input", {
    expect_false(isDuplicate(NULL))
    expect_false(isDuplicate(list()))
    expect_false(isDuplicate(data.frame()))
})
