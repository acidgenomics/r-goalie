skip_on_os("windows")

from <- file.path(tempdir(), "from.txt")
to <- file.path(tempdir(), "to.txt")
unlink(from)
unlink(to)
invisible(file.create(from))
invisible(file.symlink(from = from, to = to))



test_that("TRUE", {
    expect_true(all(isSymlink(to)))
})

test_that("FALSE : not symlink", {
    ok <- isSymlink(c(from, to))
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = nocause(ok),
        expected = c(FALSE, TRUE)
    )
    expect_identical(
        object = unname(cause(ok)),
        expected = c("not symlink", NA_character_)
    )
})


test_that("TRUE", {
    expect_true(isASymlink(to))
})

test_that("FALSE", {
    ok <- isASymlink(from)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
})



test_that("TRUE", {
    expect_true(allAreSymlinks(c(to, to)))
})

test_that("FALSE", {
    expect_false(allAreSymlinks(c(from, from)))
})



unlink(c(from, to))
