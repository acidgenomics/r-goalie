context("check : vector : isIntegerish")

test_that("TRUE", {
    expect_identical(
        isIntegerish(seq_len(2L)),
        c("1" = TRUE, "2" = TRUE)
    )
    expect_identical(
        isIntegerish(c(1, 2)),  # nolint
        c("1.000000000000000e+00" = TRUE, "2.000000000000000e+00" = TRUE)
    )
})

test_that("FALSE", {
    ok <- isIntegerish(0.1)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        cause(ok),
        c("1.000000000000000e-01" = "not integer")
    )
})

test_that("FALSE : NA input", {
    ok <- isIntegerish(c(1, 2, NA))  # nolint
    expect_s4_class(ok, "goalie")
    expect_identical(
        nocause(ok),
        c(
            "1.000000000000000e+00" = TRUE,
            "2.000000000000000e+00" = TRUE,
            "NA" = FALSE
        )
    )
    expect_identical(
        cause(ok),
        c(
            "1.000000000000000e+00" = "",
            "2.000000000000000e+00" = "",
            "NA" = "NA"
        )
    )
})

test_that("nullOK", {
    expect_false(isInt(NULL, nullOK = FALSE))
    expect_true(isInt(NULL, nullOK = TRUE))
})
