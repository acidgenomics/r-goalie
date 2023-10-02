test_that("TRUE", {
    skip_if_not_installed("tximport")
    skip_if_not_installed("tximportData")
    dir <- system.file("extdata", package = "tximportData")
    samples <- read.table(file.path(dir, "samples.txt"), header = TRUE)
    files <- file.path(dir, "salmon", samples[["run"]], "quant.sf.gz")
    names(files) <- paste0("sample", seq(from = 1L, to = length(files)))
    object <- tximport::tximport(
        files = files,
        type = "salmon",
        txIn = TRUE,
        txOut = TRUE
    )
    ok <- isTximport(object)
    expect_true(ok)
})

test_that("FALSE", {
    ok <- isTximport(list())
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = "{.var list} is not a tximport list."
    )
})
