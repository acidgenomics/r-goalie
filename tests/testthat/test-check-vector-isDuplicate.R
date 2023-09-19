test_that("isDuplicate", {
    expect_identical(
        object = isDuplicate(c("a", "a")),
        expected = c("a" = TRUE, "a" = TRUE)
    )
    ok <- isDuplicate(c("a", "a", "b", "b", "c", "d", NA, NA))
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = as.logical(ok),
        expected = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE)
    )
    ok <- isDuplicate(c("a", "b", "c"))
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = as.logical(ok),
        expected = c(FALSE, FALSE, FALSE)
    )
    expect_false(isDuplicate(NULL))
})
