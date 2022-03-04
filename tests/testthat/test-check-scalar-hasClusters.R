context("check : scalar : hasClusters")

test_that("TRUE", {
    skip_if_not_installed(pkg = "AcidSingleCell")
    requireNamespace("AcidSingleCell", quietly = TRUE)
    data(
        SingleCellExperiment_Seurat,
        SingleCellExperiment_splatter,
        package = "AcidTest",
        envir = environment()
    )
    expect_true(hasClusters(SingleCellExperiment_Seurat))
    expect_false(hasClusters(SingleCellExperiment_splatter))
})

test_that("FALSE", {
    ok <- hasClusters(list())
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = "{.var list()} does not contain clusters."
    )
})
