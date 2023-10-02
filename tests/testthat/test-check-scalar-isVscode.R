test_that("isVscode", {
    ok <- isVscode()
    expect_type(ok, "logical")
})
