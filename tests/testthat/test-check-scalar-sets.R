context("sets")

test_that("TRUE", {
    expect_true(isSubset(x = "a", y = c("a", "b")))
    expect_true(
        isSuperset(
            x = colnames(datasets::ChickWeight),
            y = c("Time", "weight", "Diet")
        )
    )
    expect_true(areDisjointSets(x = c("a", "b"), y = c("c", "d")))
    expect_true(areIntersectingSets(x = c("a", "b"), y = c("b", "c")))
    expect_true(areSetEqual(x = c("a", "b"), y = c("b", "a")))
})

test_that("FALSE", {
    expect_false(isSubset(x = "c", y = c("a", "b")))
    expect_false(
        isSuperset(
            x = c("Time", "weight", "Diet"),
            y = colnames(datasets::ChickWeight)
        )
    )
    expect_false(areDisjointSets(x = c("a", "b"), y = c("b", "a")))
    expect_false(areIntersectingSets(x = c("a", "b"), y = c("c", "d")))
    expect_false(areSetEqual(x = c("a", "b"), y = c("b", "c")))
})
