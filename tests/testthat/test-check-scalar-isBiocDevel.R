test_that("isBiocDevel", {
    ok <- isBiocDevel()
    expect_type(ok, "logical")
})
