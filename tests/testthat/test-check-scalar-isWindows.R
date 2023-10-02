test_that("logical", {
    ok <- isWindows()
    expect_type(ok, "logical")
})
