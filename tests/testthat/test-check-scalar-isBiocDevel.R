context("check : scalar : isBiocDevel")

test_that("isBiocDevel", {
    ok <- isBiocDevel()
    expect_is(ok, "logical")
})
