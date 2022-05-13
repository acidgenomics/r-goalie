skip_on_os("windows")

from <- "from.txt"
to <- "to.txt"
unlink(from)
unlink(to)
file.create(from)
file.symlink(from = from, to = to)



test_that("TRUE", {
    expect_true(all(isSymlink(to)))
})

test_that("FALSE : not symlink", {
    ok <- isSymlink(c(from, to))
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = nocause(ok),
        expected = c("from.txt" = FALSE, "to.txt" = TRUE)
    )
    expect_identical(
        object = cause(ok),
        expected = c("from.txt" = "not symlink", "to.txt" = NA_character_)
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
