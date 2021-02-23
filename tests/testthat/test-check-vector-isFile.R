files <- c("example1.txt", "example2.txt")
file.create(files)



context("check : vector : isFile")

test_that("TRUE", {
    expect_true(all(isFile(files)))
})

## Directories currently return TRUE, similar to base R `dir.exists()`.
## May want to tighten this in a future update to actual files only.
test_that("TRUE : directory input", {
    ok <- isFile(c("~", "."))
    expect_identical(nocause(ok), c(`~` = FALSE, `.` = FALSE))
})

test_that("FALSE : not file", {
    ok <- isFile(c("aaa", "bbb"))
    expect_s4_class(ok, "goalie")
    expect_identical(
        nocause(ok),
        c(aaa = FALSE, bbb = FALSE)
    )
    expect_identical(
        cause(ok),
        noquote(c(aaa = "not file", bbb = "not file"))
    )
})

test_that("FALSE : not character", {
    ok <- isFile(1L)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        cause(ok),
        noquote("'x' is not character.")
    )
})



context("check : scalar : isAFile")

test_that("TRUE", {
    expect_true(isAFile(files[[1L]]))
})

test_that("FALSE", {
    expect_false(isAFile("~"))
    ok <- isAFile("aaa")
    expect_s4_class(ok, "goalie")
    expect_false(ok)
})

test_that("nullOK", {
    expect_true(isAFile(NULL, nullOK = TRUE))
    expect_false(isAFile(NULL, nullOK = FALSE))
})



context("check : scalar : allAreFiles")

test_that("TRUE", {
    expect_true(allAreFiles(files))
})

test_that("FALSE", {
    expect_false(allAreFiles("aaa"))
})



file.remove(files)
