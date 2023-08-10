skip_if_not_installed("AcidBase")

test_that("TRUE", {
    expect_true(hasCPU(n = 1L))
})

test_that("FALSE", {
    actual <- AcidBase::cpus()
    ok <- hasCPU(n = Inf)
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = sprintf(
            "Not enough CPU cores: %s (current) < Inf (expected).",
            as.character(actual)
        )
    )
})
