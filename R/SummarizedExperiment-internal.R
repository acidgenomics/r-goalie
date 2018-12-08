# Using primary assay here.
.coerceSummarizedExperimentToMatrix <- function(object) {
    requireNamespace("SummarizedExperiment", quietly = TRUE)
    SummarizedExperiment::assay(object)
}
