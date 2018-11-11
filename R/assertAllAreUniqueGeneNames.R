#' Assert All Are Unique Gene Names
#'
#' This assert check determines if a user-defined gene name query is using only
#' unique (non-amgibuous) symbols. It is designed to be used for gene plotting
#' particularly when performing single-cell RNA-seq marker analysis.
#'
#' @inherit params
#' @export
#'
#' @examples
#' data(rse)
#'
#' object <- rse
#' print(object)
#'
#' genes <- object %>%
#'     SummarizedExperiment::rowData(.) %>%
#'     .[["geneName"]] %>%
#'     as.character() %>%
#'     head()
#' print(genes)
#'
#' assertAllAreUniqueGeneNames(object = object, genes = genes)
assertAllAreUniqueGeneNames <- function(object, genes) {
    assert_that(isS4(object))
    assert_is_character(genes)
    # Get all of the gene names stashed in the object.
    if (is(object, "SummarizedExperiment")) {
        requireNamespace("S4Vectors", quietly = TRUE)
        requireNamespace("SummarizedExperiment", quietly = TRUE)
        object <- object %>%
            SummarizedExperiment::rowRanges(.) %>%
            S4Vectors::mcols(.)
    }
    allGenes <- object[["geneName"]]
    assert_is_non_empty(allGenes)
    # Require that the user passed in gene names.
    assert_is_subset(genes, allGenes)
    # Check for no intersect with duplicate names.
    duplicatedGenes <- allGenes[which(duplicated(allGenes))]
    assert_are_disjoint_sets(genes, duplicatedGenes)
}
