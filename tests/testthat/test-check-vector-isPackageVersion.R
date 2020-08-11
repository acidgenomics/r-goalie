context("check : vector : isPackageVersion")

test_that("TRUE", {
    x <- isPackageVersion(
        x = c(
            "base" = packageVersion("base"),
            "utils" = packageVersion("utils")
        ),
        op = "=="
    )
    expect_true(all(x))
})

test_that("FALSE", {
    x <- isPackageVersion(
        x = c(
            "base" = packageVersion("base"),
            "utils" = packageVersion("utils"),
            "XXX" = "0.0.1"
        ),
        op = ">"
    )
    expect_false(any(x))
})
