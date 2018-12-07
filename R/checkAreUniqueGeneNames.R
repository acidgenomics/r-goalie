#' Are the Requested Gene Names Duplicated in the Corresponding Object?
#'
#' This assert check determines if a user-defined gene name query is using only
#' unique (non-amgibuous) symbols. It is designed to be used for gene plotting
#' particularly when performing single-cell RNA-seq marker analysis.
#'
#' @name checkAreUniqueGeneNames
#' @aliases areUniqueGeneNames
#' @inherit params
#'
#' @examples
#' x <- SummarizedExperiment::SummarizedExperiment(
#'     assays = matrix(
#'         data = seq_len(16L),
#'         nrow = 4L,
#'         ncol = 4L,
#'         dimnames = list(
#'             paste0("gene", seq_len(4L)),
#'             paste0("sample", seq_len(4L))
#'         )
#'     ),
#'     rowData = S4Vectors::DataFrame(
#'         geneID = paste0("ENSG0000000000", seq_len(4L)),
#'         geneName = paste0("SYMBOL", seq_len(4L))
#'     )
#' )
#' genes <- SummarizedExperiment::rowData(x)$geneName
#' checkAreUniqueGeneNames(x = x, genes = genes)
NULL



checkAreUniqueGeneNames <- function(x, genes) {
    ok <- isS4(x)
    if (!ok) {
        return("x is not an S4 class object")
    }
    ok <- is(genes, "character")
    if (!ok) {
        return("genes are not character")
    }
    # Get all of the gene names stashed in the x.
    if (is(x, "SummarizedExperiment")) {
        requireNamespace("SummarizedExperiment", quietly = TRUE)
        x <- SummarizedExperiment::rowData(x)
    }
    # Coercing to character here to handle Rle/factor matching.
    all <- as(x[["geneName"]], "character")
    # Check for gene names (symbols).
    if (!has_length(all)) {
        return("Gene names are not defined in object")
    }
    # Require that the user passed in gene names.
    ok <- all(genes %in% allGenes)
    if (!ok) {
        setdiff <- setdiff(genes, allGenes)
        return(paste("Genes missing in object:", setdiff))
    }
    # Get a vector of all duplicated gene names in the object.
    dupes <- all[which(duplicated(all))]
    # Now check for intersection with the user-defined genes vector.
    intersect <- intersect(genes, dupes)
    if (has_length(intersect)) {
        return(paste("Non-unique gene names:", toString(intersect)))
    }
    TRUE
}



#' @rdname checkAreUniqueGeneNames
#' @export
check_are_unique_gene_names <- checkAreUniqueGeneNames



#' @rdname checkAreUniqueGeneNames
#' @export
testAreUniqueGeneNames <- makeTestFunction(checkAreUniqueGeneNames)



#' @rdname checkAreUniqueGeneNames
#' @export
test_are_unique_gene_names <- testAreUniqueGeneNames



#' @rdname checkAreUniqueGeneNames
#' @export
assertAreUniqueGeneNames <- makeAssertionFunction(checkAreUniqueGeneNames)



#' @rdname checkAreUniqueGeneNames
#' @export
assert_are_unique_gene_names <- assertAreUniqueGeneNames



#' @rdname checkAreUniqueGeneNames
#' @export
expect_alpha <- makeExpectationFunction(checkAreUniqueGeneNames)
