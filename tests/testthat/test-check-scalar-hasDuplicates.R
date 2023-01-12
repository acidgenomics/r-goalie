test_that("hasDuplicates : TRUE", {
    expect_true(hasDuplicates(c("a", "a")))
})

test_that("hasDuplicates : FALSE", {
    ok <- hasDuplicates(c("a", "b"))
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "{.var c(\"a\", \"b\")} has no duplicates."
    )
})

test_that("hasNoDuplicates : TRUE", {
    expect_true(hasNoDuplicates(c("a", "b")))
})

test_that("hasNoDuplicates : FALSE", {
    x <- c("a", "a", "b", "b")
    ok <- hasNoDuplicates(x)
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = paste(
            "{.var x}",
            "has duplicates at positions 2, 4."
        )
    )
    y <- S4Vectors::Rle(x)
    ok <- hasNoDuplicates(y)
    expect_false(ok)
    expect_s4_class(ok, "goalie")
})
