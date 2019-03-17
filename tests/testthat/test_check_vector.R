context("check: vector")

test_that("hasAccess", {
    ok <- hasAccess(c("~", "."))
    expect_identical(ok, c(`~` = TRUE, `.` = TRUE))

    ok <- hasAccess(c("xxx", "yyy"))
    expect_s3_class(ok, "goalie")
    expect_identical(nocause(ok), c(xxx = FALSE, yyy = FALSE))
    expect_identical(cause(ok), noquote(c("no access", "no access")))

    ok <- hasAccess(NULL)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(cause(ok), noquote("x is not character."))
})

test_that("isDirectory", {
    ok <- isDirectory(c("~", "."))
    expect_identical(ok, c(`~` = TRUE, `.` = TRUE))

    ok <- isDirectory(c("aaa", "bbb"))
    expect_s3_class(ok, "goalie")
    expect_identical(nocause(ok), c(aaa = FALSE, bbb = FALSE))
    expect_identical(cause(ok), noquote(c("not dir", "not dir")))

    ok <- isDirectory(NULL)
    expect_false(ok)
})

# nolint start
test_that("isEqual", {
    expect_true(isEqualTo(x = 1L, y = 1))
    expect_true(isNotEqualTo(x = 2, y = 1))
    expect_true(isGreaterThan(x = 1, y = 0))
    expect_true(all(isGreaterThanOrEqualTo(x = seq_len(2), y = 1)))
    expect_true(isLessThan(x = -1, y = 0))
    expect_true(all(isLessThanOrEqualTo(x = seq_len(2), y = 3)))

    ok <- isEqualTo(x = seq_len(2), y = 1)
    expect_identical(nocause(ok), c(`1` = TRUE, `2` = FALSE))
    expect_identical(cause(ok), noquote(c("", "not equal to 1; abs diff = 1")))
})
# nolint end

test_that("isExisting", {
    a <- 1L
    b <- 2L

    ok <- isExisting(c("a", "b"))
    expect_identical(ok, c(a = TRUE, b = TRUE))

    ok <- allAreNonExisting(c("x", "y"))
    expect_true(ok)

    ok <- isExisting(c("x", "y"))
    expect_s3_class(ok, "goalie")
    expect_identical(nocause(ok), c(x = FALSE, y = FALSE))
    expect_identical(cause(ok), noquote(c("non-existing", "non-existing")))

    ok <- allAreExisting(c("x", "y"))
    expect_s3_class(ok, "goalie")
    expect_false(ok)
})

test_that("isFile", {
    x <- "example.txt"
    file.create(x)
    expect_true(isAFile(x))
    unlink(x)

    ok <- isFile(1L)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(cause(ok), noquote("x is not character."))

    # Directories currently return TRUE, similar to base R `dir.exists()`.
    # May want to tighten this in a future update to actual files only.
    ok <- isFile(c("~", "."))
    expect_identical(ok, c(`~` = TRUE, `.` = TRUE))

    ok <- isFile(c("aaa", "bbb"))
    expect_s3_class(ok, "goalie")
    expect_identical(nocause(ok), c(aaa = FALSE, bbb = FALSE))
    expect_identical(cause(ok), noquote(c("not file", "not file")))
})

test_that("isHexColor", {
    x <- viridis::viridis(n = 2L)
    ok <- isHexColor(x)
    expect_identical(ok, c(`#440154FF` = TRUE, `#FDE725FF` = TRUE))

    x <- ggplot2::scale_colour_manual
    ok <- isHexColor(x)
    expect_s3_class(ok, "goalie")
    expect_identical(cause(ok), noquote("x is not character."))
    expect_false(ok)
})

test_that("isInRange", {
    expect_true(isInRange(0L, lower = 0L, upper = 1L))
    expect_true(isInRange(1L, lower = 0L, upper = 1L))
    expect_true(isInClosedRange(1L, lower = 0L, upper = 1L))

    expect_true(isInOpenRange(0.5, lower = 0L, upper = 1L))
    expect_true(isInLeftOpenRange(1L, lower = 0L, upper = 1L))
    expect_true(isInRightOpenRange(0L, lower = 0L, upper = 1L))

    expect_true(all(isNegative(c(-2L, -1L))))
    expect_true(all(isPositive(c(1L, 2L))))
    expect_true(all(isNonNegative(c(0L, 1L))))
    expect_true(all(isNonPositive(c(-1L, 0L))))

    expect_true(all(isPercentage(c(0L, 25L, 50L, 100L))))
    expect_true(all(isProportion(c(0L, 0.01, 0.1, 1L))))

    ok <- isInRange(c(2L, 3L), lower = 0L, upper = 1L)
    expect_s3_class(ok, "goalie")
    expect_false(all(as.logical(ok)), c(FALSE, FALSE))

    ok <- isInClosedRange(c(2L, 3L), lower = 0L, upper = 1L)
    expect_s3_class(ok, "goalie")
    expect_false(all(as.logical(ok)), c(FALSE, FALSE))

    ok <- isInOpenRange(c(1L, 2L), lower = 0L, upper = 1L)
    expect_s3_class(ok, "goalie")
    expect_false(all(as.logical(ok)), c(FALSE, FALSE))

    expect_false(isInLeftOpenRange(0L, lower = 0L))
    expect_false(isInRightOpenRange(1L, upper = 1L))

    expect_false(isPositive(-1L))
    expect_false(isNegative(1L))

    expect_false(isPercentage(110L))
    expect_false(isProportion(1.1))
})

test_that("isIntegerish", {
    expect_identical(isIntegerish(seq_len(2L)), c(TRUE, TRUE))
    expect_identical(isIntegerish(c(1, 2)), c(TRUE, TRUE))  # nolint

    expect_false(isIntegerish(0.1))

    ok <- isIntegerish(c(1, 2, NA))  # nolint
    expect_s3_class(ok, "goalie")
    expect_identical(
        as.logical(ok),
        c(TRUE, TRUE, FALSE)
    )
    expect_identical(
        cause(ok),
        noquote(c("", "", "NA"))
    )
})

# `isInt()` is a scalar short alias of `isIntegerish()`.
test_that("isInt", {
    ok <- isInt(seq_len(2L))
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        cause(ok),
        noquote("x does not have a length of 1.")
    )

    expect_true(isInt(1L))
    expect_true(isInt(1))  # nolint
    expect_true(isInt(1.0))  # integerish

    expect_false(isInt(0.1))
})

test_that("isMatching", {
    expect_true(isMatchingRegex(x = "foobar", pattern = "^f"))
    expect_true(isNotMatchingRegex(x = "foobar", pattern = "^b"))

    expect_true(isMatchingFixed(x = "foobar", pattern = "bar"))
    expect_true(isNotMatchingFixed(x = "foo", pattern = "bar"))
})

urls <- c("https://www.r-project.org", "ftp://r-project.org")

test_that("isURL", {
    isURL(urls)
    isAURL(urls[[1L]])
    allAreURLs(urls)

    ok <- isURL("xxx")
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        cause(ok),
        noquote("not URL")
    )

    ok <- isAURL(urls)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        cause(ok),
        noquote("urls is not a character of length 1.")
    )
})

test_that("isAURL", {
    expect_true(isAURL(urls[[1L]]))

    ok <- isAURL(urls)
    expect_false(ok)
    expect_identical(
        cause(ok),
        noquote("urls is not a character of length 1.")
    )
})

test_that("allAreURLs", {
    expect_true(allAreURLs(urls))
})
