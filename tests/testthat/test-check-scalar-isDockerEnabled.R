test_that("isDockerEnabled", {
    ok <- isDockerEnabled()
    expect_is(ok, "logical")
})
