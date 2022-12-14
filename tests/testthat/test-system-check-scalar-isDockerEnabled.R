test_that("isDockerEnabled", {
    ok <- isDockerEnabled()
    expect_type(ok, "logical")
})
