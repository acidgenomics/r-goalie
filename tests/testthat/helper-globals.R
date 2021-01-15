options(
    "goalie.traceback" = FALSE,
    "verbose" = FALSE
)

## nolint start
DataFrame <- S4Vectors::DataFrame
SummarizedExperiment <- SummarizedExperiment::SummarizedExperiment
assay <- SummarizedExperiment::assay
data.table <- data.table::data.table
iris <- datasets::iris
mtcars <- datasets::mtcars
rowData <- SummarizedExperiment::rowData
`rowData<-` <- SummarizedExperiment::`rowData<-`
sparseMatrix <- Matrix::sparseMatrix
tibble <- tibble::tibble
## nolint end

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
        "geneId" = paste0("ENSG0000000000", seq_len(4L)),
        "geneName" = paste0("SYMBOL", seq_len(4L))
    )
)

genes <- rowData(se)[["geneName"]]
