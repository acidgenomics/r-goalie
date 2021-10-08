context("check : scalar : hasDuplicates")

test_that("TRUE", {
    expect_true(hasDuplicates(c("a", "a")))
})

test_that("FALSE", {
    ok <- hasDuplicates(c("a", "b"))
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        cause(ok),
        "{.var c(\"a\", \"b\")} has no duplicates."
    )
})



context("check : scalar : hasNoDuplicates")

test_that("TRUE", {
    expect_true(hasNoDuplicates(c("a", "b")))
})

test_that("FALSE", {
    ok <- hasNoDuplicates(c("a", "a", "b", "b"))
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        cause(ok),
        paste(
            "{.var c(\"a\", \"a\", \"b\", \"b\")}",
            "has duplicates at positions 2, 4."
        )
    )
})
