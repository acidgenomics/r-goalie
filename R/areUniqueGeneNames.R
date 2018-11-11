#' Are Unique Gene Names?
#'
#' This assert check determines if a user-defined gene name query is using only
#' unique (non-amgibuous) symbols. It is designed to be used for gene plotting
#' particularly when performing single-cell RNA-seq marker analysis.
#'
#' @inherit params
#' @export
#'
#' @examples
#' library(SummarizedExperiment)
#' x <- SummarizedExperiment(
#'     assays = matrix(
#'         data = seq_len(16L),
#'         nrow = 4L,
#'         ncol = 4L,
#'         dimnames = list(
#'             paste0("gene", seq_len(4L)),
#'             paste0("sample", seq_len(4L))
#'         )
#'     ),
#'     rowData = DataFrame(
#'         geneID = paste0("ENSG0000000000", seq_len(4L)),
#'         geneName = paste0("SYMBOL", seq_len(4L))
#'     )
#' )
#' genes <- rowData(x)$geneName
#' areUniqueGeneNames(x = x, genes = genes)
areUniqueGeneNames <- function(x, genes) {
    if (!isS4(x)) {
        return(FALSE)
    }
    assert_that(isS4(x))
    assert_is_character(genes)
    # Get all of the gene names stashed in the x.
    if (is(x, "SummarizedExperiment")) {
        requireNamespace("SummarizedExperiment", quietly = TRUE)
        x <- SummarizedExperiment::rowData(x)
    }
    allGenes <- x[["geneName"]]
    assert_is_non_empty(allGenes)
    # Require that the user passed in gene names.
    assert_is_subset(genes, allGenes)
    # Check for no intersect with duplicate names.
    duplicatedGenes <- allGenes[which(duplicated(allGenes))]
    assert_are_disjoint_sets(genes, duplicatedGenes)
}

#' @rdname areUniqueGeneNames
#' @export
assertAreUniqueGeneNames <- function(x, genes) {
    assert_that(
        isS4(x),
        allAreUniqueGeneNames(x, genes)
    )
}
