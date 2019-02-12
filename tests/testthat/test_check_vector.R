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

# isDirectory
# isEqual
# isExisting
# isFile
# isHexColor
# isInRange
# isIntegerish
# isMatching
# isURL
