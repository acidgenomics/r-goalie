test_that("character", {
    expect_identical(
        object = isDuplicate(c("a", "a")),
        expected = c(TRUE, TRUE)
    )
    ok <- isDuplicate(c("a", "a", "b", "b", "c", "d", NA, NA))
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = as.logical(ok),
        expected = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE)
    )
    expect_identical(
        object = cause(ok),
        expected = c(NA, NA, NA, NA, "unique", "unique", NA, NA)
    )
    ok <- isDuplicate(c("a", "b", "c"))
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = as.logical(ok),
        expected = c(FALSE, FALSE, FALSE)
    )
})

test_that("numeric", {
    expect_identical(
        object = isDuplicate(c(1L, 1L)),
        expected = c(TRUE, TRUE)
    )
    ok <- isDuplicate(c(1L, 1L, 2L, 2L, 3L, 4L, NA, NA))
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = as.logical(ok),
        expected = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE)
    )
    expect_identical(
        object = cause(ok),
        expected = c(NA, NA, NA, NA, "unique", "unique", NA, NA)
    )
    ok <- isDuplicate(c(1L, 2L, 3L))
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = as.logical(ok),
        expected = c(FALSE, FALSE, FALSE)
    )

})

test_that("Invalid input", {
    expect_false(isDuplicate(NULL))
    expect_false(isDuplicate(list()))
    expect_false(isDuplicate(data.frame()))
})
