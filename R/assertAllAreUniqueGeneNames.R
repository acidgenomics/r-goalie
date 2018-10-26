#' Assert All Are Unique Gene Names
#'
#' This assert check determines if a user-defined gene name query is using only
#' unique (non-amgibuous) symbols. It is designed to be used for gene plotting
#' particularly when performing single-cell RNA-seq marker analysis.
#'
#' @inherit assert
#' @export
#'
#' @param object `SummarizedExperiment`.
#' @param genes `character`. Input vector to check against definitions in the
#'   corresponding `SummarizedExperiment`.
#'
#' @examples
#' # FIXME
#' data(rse_small, package = "basejump")
#' object <- rse_small
#' print(object)
#' genes <- object %>%
#'     SummarizedExperiment::rowData(.) %>%
#'     .[["geneName"]] %>%
#'     as.character() %>%
#'     head()
#' print(genes)
#' assertAllAreUniqueGeneNames(object = object, genes = genes)
assertAllAreUniqueGeneNames <- function(object, genes) {
    stopifnot(isS4(object))
    assert_is_character(genes)
    # Get all of the gene names stashed in the object.
    if (is(object, "SummarizedExperiment")) {
        allGenes <- mcols(rowRanges(object))[["geneName"]]
    } else {
        allGenes <- object[["geneName"]]
    }
    assert_is_non_empty(allGenes)
    # Require that the user passed in gene names.
    assert_is_subset(genes, allGenes)
    # Check for no intersect with duplicate names.
    duplicatedGenes <- allGenes[which(duplicated(allGenes))]
    assert_are_disjoint_sets(genes, duplicatedGenes)
}
