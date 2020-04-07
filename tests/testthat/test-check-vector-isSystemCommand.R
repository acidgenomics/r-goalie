context("check : vector : isSystemCommand")

test_that("TRUE", {
    x <- c("cd", "rm")
    expect_true(all(isSystemCommand(x)))
    expect_true(allAreSystemCommands(x))
})

test_that("FALSE", {
    x <- c("AAA", "BBB")
    expect_false(all(isSystemCommand(x)))
    expect_false(allAreSystemCommands(x))
})
