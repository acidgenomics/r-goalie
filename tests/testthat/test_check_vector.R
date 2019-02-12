context("Vectorized checks")

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
    expect_identical(
        isFile(c("~", ".")),
        c(`~` = FALSE, `.` = FALSE)
    )
})

# isHexColor
# isInRange
# isIntegerish
# isMatching
# isURL
