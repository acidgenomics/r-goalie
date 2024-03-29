skip_if_not_installed("AcidBase")

test_that("TRUE", {
    expect_true(hasRam(n = 1L))
})

test_that("FALSE", {
    actual <- AcidBase::ram()
    ok <- hasRam(n = Inf)
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = sprintf(
            "Not enough RAM (in GB): %s (current) < Inf (expected).",
            as.character(actual)
        )
    )
})
