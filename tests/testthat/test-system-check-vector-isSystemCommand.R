test_that("TRUE", {
    x <- c("cp", "rm")
    expect_true(all(isSystemCommand(x)))
    expect_true(allAreSystemCommands(x))
    expect_true(isASystemCommand(x[[1L]]))
})

test_that("FALSE", {
    x <- c("AAA", "BBB")
    expect_false(all(isSystemCommand(x)))
    expect_false(nocause(allAreSystemCommands(x)))
    expect_false(nocause(isASystemCommand(x)))
    expect_false(nocause(isASystemCommand(x[[1L]])))
    expect_false(nocause(isSystemCommand(1L)))
})
