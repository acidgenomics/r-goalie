context("base : callAndName")

test_that("callAndName", {
    expect_identical(
        callAndName(is.finite, c(1, Inf, NA)),  # nolint
        c(
            "1.000000000000000e+00" = TRUE,
            "Inf" = FALSE,
            "NA" = FALSE
        )
    )
})
