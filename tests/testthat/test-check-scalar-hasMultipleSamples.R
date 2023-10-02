skip_if_not_installed(pkg = "SummarizedExperiment")

test_that("TRUE", {
    skip_if_not_installed(pkg = "AcidSingleCell")
    requireNamespace("AcidSingleCell", quietly = TRUE)
    data(
        SingleCellExperiment_Seurat,
        SingleCellExperiment_splatter,
        package = "AcidTest",
        envir = environment()
    )
    expect_false(hasMultipleSamples(SingleCellExperiment_Seurat))
    expect_true(hasMultipleSamples(SingleCellExperiment_splatter))
})

test_that("FALSE", {
    ok <- hasMultipleSamples(list())
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = "{.var list} is not {.cls SummarizedExperiment}."
    )
    object <- SummarizedExperiment::SummarizedExperiment(
        assay = matrix(
            data = seq_len(4L),
            nrow = 4L,
            ncol = 1L
        ),
        colData = S4Vectors::DataFrame(
            "genotype" = "wildtype",
            "treatment" = "control"
        )
    )
    ok <- hasMultipleSamples(object)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = paste(
            "{.var SummarizedExperiment} does not",
            "contain multiple samples."
        )
    )
})
