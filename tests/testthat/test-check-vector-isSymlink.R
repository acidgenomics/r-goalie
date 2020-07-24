skip_on_os("windows")

from <- "from.txt"
to <- "to.txt"
file.create(from)
file.symlink(from = from, to = to)



context("check : vector : isSymlink")

test_that("TRUE", {
    expect_true(all(isSymlink(to)))
})

test_that("FALSE : not symlink", {
    ok <- isSymlink(c(from, to))
    expect_s3_class(ok, "goalie")
    expect_identical(
        nocause(ok),
        c(from.txt = FALSE, to.txt = TRUE)
    )
    expect_identical(
        cause(ok),
        noquote(c(from.txt = "not symlink", to.txt = ""))
    )
})


context("check : scalar : isASymlink")

test_that("TRUE", {
    expect_true(isASymlink(to))
})

test_that("FALSE", {
    ok <- isASymlink(from)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
})



context("check : scalar : allAreSymlinks")

test_that("TRUE", {
    expect_true(allAreSymlinks(c(to, to)))
})

test_that("FALSE", {
    expect_false(allAreSymlinks(c(from, from)))
})



unlink(c(from, to))
