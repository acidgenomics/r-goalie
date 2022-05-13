test_that("scalar", {
    expect_output(
        object = print(isFlag(1L)),
        regexp = "\\[1\\] FALSE"
    )
})

test_that("vector", {
    expect_output(
        object = print(isIntegerish(c(1L, NA))),
        regexp = "1    NA"
    )
})
