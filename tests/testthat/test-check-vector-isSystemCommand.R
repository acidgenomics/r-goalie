test_that("TRUE", {
    x <- c("cp", "rm")
    expect_true(all(isSystemCommand(x)))
    expect_true(allAreSystemCommands(x))
    expect_true(isASystemCommand(x[[1L]]))
})

test_that("FALSE", {
    x <- c("AAA", "BBB")
    expect_false(all(isSystemCommand(x)))
    expect_false(allAreSystemCommands(x))
    expect_false(isASystemCommand(x))
    expect_false(isASystemCommand(x[[1L]]))
    expect_false(isSystemCommand(1L))
})
