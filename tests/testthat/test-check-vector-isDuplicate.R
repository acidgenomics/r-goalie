test_that("isDuplicate", {
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
    expect_false(isDuplicate(NULL))
})
