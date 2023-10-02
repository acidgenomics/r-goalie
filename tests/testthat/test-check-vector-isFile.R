files <- file.path(tempdir(), c("example1.txt", "example2.txt"))
invisible(file.create(files))



test_that("TRUE", {
    expect_true(all(isFile(files)))
})

## Directories currently return TRUE, similar to base R `dir.exists()`.
## May want to tighten this in a future update to actual files only.

test_that("TRUE : directory input", {
    ok <- isFile(c("~", "."))
    expect_identical(nocause(ok), rep(FALSE, 2L))
})

test_that("FALSE : not file", {
    ok <- isFile(c("aaa", "bbb"))
    expect_s4_class(ok, "goalie")
    expect_identical(object = nocause(ok), rep(FALSE, 2L))
    expect_identical(
        object = cause(ok),
        expected = rep("not file", 2L)
    )
})

test_that("FALSE : not character", {
    ok <- isFile(1L)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = "not character"
    )
})



test_that("TRUE", {
    expect_true(isAFile(files[[1L]]))
})

test_that("FALSE", {
    expect_false(isAFile("~"))
    ok <- isAFile("aaa")
    expect_s4_class(ok, "goalie")
    expect_false(ok)
})

test_that("nullOk", {
    expect_true(isAFile(NULL, nullOk = TRUE))
    expect_false(isAFile(NULL, nullOk = FALSE))
})



test_that("TRUE", {
    expect_true(allAreFiles(files))
})

test_that("FALSE", {
    expect_false(allAreFiles("aaa"))
})



invisible(file.remove(files))
