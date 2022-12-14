test_that("isDevel", {
    ok <- isDevel()
    expect_type(ok, "logical")
})
