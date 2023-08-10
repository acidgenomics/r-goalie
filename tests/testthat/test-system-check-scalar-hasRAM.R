test_that("TRUE", {
    expect_true(hasRAM(n = 1L))
})

test_that("FALSE", {
    actual <- AcidBase::ram()
    ok <- hasRAM(n = Inf)
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = sprintf(
            "Not enough RAM (in GB): %s (current) < Inf (expected).",
            as.character(actual)
        )
    )
})
