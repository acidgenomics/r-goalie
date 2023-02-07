test_that("Installed packages", {
    packages <- c("base", "utils")
    ok <- requireNamespaces(packages)
    expect_true(ok)
})

test_that("Missing packages", {
    packages <- c("AAA", "BBB")
    expect_error(
        object = requireNamespaces(packages),
        regexp = packages[[1L]]
    )
})
