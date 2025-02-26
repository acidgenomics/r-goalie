test_that("areDisjointSets", {
    expect_true(areDisjointSets(x = c("a", "b"), y = c("c", "d")))
    expect_false(nocause(areDisjointSets(x = c("a", "b"), y = c("b", "a"))))
})

test_that("areIntersectingSets", {
    expect_true(areIntersectingSets(x = c("a", "b"), y = c("b", "c")))
    expect_false(nocause(areIntersectingSets(x = c("a", "b"), y = c("c", "d"))))
})

test_that("areSetEqual", {
    expect_true(areSetEqual(x = c("a", "b"), y = c("b", "a")))
    expect_false(nocause(areSetEqual(x = c("a", "b"), y = c("b", "c"))))
    expect_false(nocause(areSetEqual(x = c("a", "b"), y = c("b", "c"))))
    expect_false(nocause(areSetEqual(x = c("b", "c"), y = c("a", "b"))))
    expect_false(nocause(areSetEqual(x = c("a", "b"), y = c("a", "b", "c"))))
})

test_that("isSubset", {
    expect_true(isSubset(x = "a", y = c("a", "b")))
    expect_false(nocause(isSubset(x = "c", y = c("a", "b"))))
    expect_false(nocause(isSubset(NULL, 1L)))
    expect_false(nocause(isSubset(1L, NULL)))
})

test_that("isSuperset", {
    expect_true(
        isSuperset(
            x = colnames(datasets::ChickWeight),
            y = c("Time", "weight", "Diet")
        )
    )
    expect_false(nocause(
        isSuperset(
            x = c("Time", "weight", "Diet"),
            y = colnames(datasets::ChickWeight)
        )
    ))
})
