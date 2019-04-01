context("matchesUniqueGeneNames")

se <- SummarizedExperiment(
    assays = matrix(
        data = seq_len(16L),
        nrow = 4L,
        ncol = 4L,
        dimnames = list(
            paste0("gene", seq_len(4L)),
            paste0("sample", seq_len(4L))
        )
    ),
    rowData = DataFrame(
        geneID = paste0("ENSG0000000000", seq_len(4L)),
        geneName = paste0("SYMBOL", seq_len(4L))
    )
)

test_that("TRUE", {
    genes <- rowData(se)[["geneName"]]
    ok <- matchesUniqueGeneNames(x = se, genes = genes)
    expect_true(ok)
})

test_that("FALSE", {
    rowData(se)[["geneName"]][[2L]] <- rowData(se)[["geneName"]][[1L]]
    genes <- rowData(se)[["geneName"]]
    ok <- matchesUniqueGeneNames(x = se, genes = genes)
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        cause(ok),
        noquote("Non-unique gene names: SYMBOL1")
    )
})
