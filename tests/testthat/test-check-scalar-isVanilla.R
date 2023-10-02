test_that("isVanilla", {
    ok <- isVanilla()
    expect_type(ok, "logical")
})
