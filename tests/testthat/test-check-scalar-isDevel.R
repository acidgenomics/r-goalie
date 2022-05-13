test_that("isDevel", {
    ok <- isDevel()
    expect_is(ok, "logical")
})
