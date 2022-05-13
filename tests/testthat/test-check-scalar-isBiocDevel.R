test_that("isBiocDevel", {
    ok <- isBiocDevel()
    expect_is(ok, "logical")
})
