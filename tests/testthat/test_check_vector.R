context("check: vector")

# FIXME All vectorized checks need to return cause attribute on failure.

test_that("hasAccess", {
    expect_identical(
        hasAccess(c("~", ".")),
        c(`~` = TRUE, `.` = TRUE)
    )

    # Note that we're not currently setting a cause attribute here.
    expect_identical(
        hasAccess(c("xxx", "yyy")),
        c(xxx = FALSE, yyy = FALSE)
    )

    object <- hasAccess(NULL)
    expect_s3_class(object, "goalie")
    expect_false(object)
    expect_identical(
        cause(object),
        noquote("x is not character.")
    )
})

test_that("isDirectory", {
    expect_identical(
        isDirectory(c("~", ".")),
        c(`~` = TRUE, `.` = TRUE)
    )

    # Note that this doesn't set cause attribute on failure here.
    expect_identical(
        isDirectory(c("aaa", "bbb")),
        c(aaa = FALSE, bbb = FALSE)
    )

    expect_false(isDirectory(NULL))
})

test_that("isEqual", {
    expect_true(isEqualTo(x = 1L, y = 1))
    expect_true(isNotEqualTo(x = 2, y = 1))
    expect_true(isGreaterThan(x = 1, y = 0))
    expect_true(all(isGreaterThanOrEqualTo(x = seq_len(2), y = 1)))
    expect_true(isLessThan(x = -1, y = 0))
    expect_true(all(isLessThanOrEqualTo(x = seq_len(2), y = 3)))

    object <- isEqualTo(x = seq_len(2), y = 1)
    expect_identical(as.logical(object), c(TRUE, FALSE))
    expect_identical(
        cause(object),
        noquote(c("", "not equal to 1; abs diff = 1"))
    )
})

test_that("isExisting", {
    a <- 1L
    b <- 2L

    expect_identical(
        isExisting(c("a", "b")),
        c(TRUE, TRUE)
    )
    expect_true(allAreNonExisting(c("x", "y")))

    object <- isExisting(c("x", "y"))
    expect_s3_class(object, "goalie")
    expect_identical(as.logical(object), c(FALSE, FALSE))
    expect_identical(
        cause(object),
        noquote(c("non-existing", "non-existing"))
    )

    object <- allAreExisting(c("x", "y"))
    expect_s3_class(object, "goalie")
    expect_false(object)
})

test_that("isFile", {
    x <- "example.txt"
    file.create(x)
    expect_true(isAFile(x))
    unlink(x)

    expect_false(isFile(1L))

    # Note that we're not setting a cause attribute here.
    expect_identical(
        isFile(c("~", ".")),
        c(`~` = FALSE, `.` = FALSE)
    )
})

test_that("isHexColor", {
    x <- viridis::viridis(n = 2L)
    expect_identical(isHexColor(x), c(TRUE, TRUE))

    x <- ggplot2::scale_colour_manual
    expect_false(isHexColor(x))
})

test_that("isInRange", {
    expect_true(isInRange(0L, lower = 0L, upper = 1L))
    expect_true(isInRange(1L, lower = 0L, upper = 1L))
    expect_true(isInClosedRange(1L, lower = 0L, upper = 1L))

    expect_true(isInOpenRange(0.5, lower = 0L, upper = 1L))
    expect_true(isInLeftOpenRange(1L, lower = 0L, upper = 1L))
    expect_true(isInRightOpenRange(0L, lower = 0L, upper = 1L))

    expect_identical(isNegative(c(-2L, -1L)), c(TRUE, TRUE))
    expect_identical(isPositive(c(1L, 2L)), c(TRUE, TRUE))

    expect_identical(isNonNegative(c(0L, 1L)), c(TRUE, TRUE))
    expect_identical(isNonPositive(c(-1L, 0L)), c(TRUE, TRUE))

    expect_true(all(isPercentage(c(0L, 25L, 50L, 100L))))
    expect_true(all(isProportion(c(0L, 0.01, 0.1, 1L))))

    object <- isInRange(c(2L, 3L), lower = 0, upper = 1)
    expect_s3_class(object, "goalie")
    expect_false(all(as.logical(object)), c(FALSE, FALSE))

    object <- isInClosedRange(c(2L, 3L), lower = 0L, upper = 1L)
    expect_s3_class(object, "goalie")
    expect_false(all(as.logical(object)), c(FALSE, FALSE))

    object <- isInOpenRange(c(1L, 2L), lower = 0L, upper = 1L)
    expect_s3_class(object, "goalie")
    expect_false(all(as.logical(object)), c(FALSE, FALSE))

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

    object <- isIntegerish(c(1, 2, NA))  # nolint
    expect_s3_class(object, "goalie")
    expect_identical(
        as.logical(object),
        c(TRUE, TRUE, FALSE)
    )
    expect_identical(
        cause(object),
        noquote(c("", "", "NA"))
    )
})

# `isInt()` is a scalar short alias of `isIntegerish()`.
test_that("isInt", {
    object <- isInt(seq_len(2L))
    expect_s3_class(object, "goalie")
    expect_false(object)
    expect_identical(
        cause(object),
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

    object <- isURL("xxx")
    expect_s3_class(object, "goalie")
    expect_false(object)
    expect_identical(
        cause(object),
        noquote("not URL")
    )

    object <- isAURL(urls)
    expect_s3_class(object, "goalie")
    expect_false(object)
    expect_identical(
        cause(object),
        noquote("urls is not a character of length 1.")
    )
})

test_that("isAURL", {
    expect_true(isAURL(urls[[1L]]))

    object <- isAURL(urls)
    expect_false(object)
    expect_identical(
        cause(object),
        noquote("urls is not a character of length 1.")
    )
})

test_that("allAreURLs", {
    expect_true(allAreURLs(urls))
})
