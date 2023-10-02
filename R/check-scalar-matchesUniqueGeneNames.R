#' Do the input gene names match unique values in the corresponding object?
#'
#' This assert check determines if a user-defined gene name query is using only
#' unique (non-amgibuous) symbols. It is designed to be used for gene plotting
#' particularly when performing single-cell RNA-seq marker analysis.
#'
#' @name check-scalar-matchesUniqueGeneNames
#' @note Updated 2022-10-18.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
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
#'         "geneId" = paste0("ENSG0000000000", seq_len(4L)),
#'         "geneName" = paste0("SYMBOL", seq_len(4L))
#'     )
#' )
#' genes <- SummarizedExperiment::rowData(x)$geneName
#'
#' ## TRUE ====
#' matchesUniqueGeneNames(x = x, genes = genes)
NULL



#' @rdname check-scalar-matchesUniqueGeneNames
#' @export
matchesUniqueGeneNames <- function(x, genes) {
    ok <- isS4(x)
    if (!isTRUE(ok)) {
        return(false(
            "{.var %s} is not an S4 class object.",
            toCauseName(x)
        ))
    }
    ok <- isCharacter(genes)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ## Get all of the gene names stashed in the x.
    if (is(x, "SummarizedExperiment")) {
        requireNamespaces("SummarizedExperiment")
        x <- SummarizedExperiment::rowData(x)
    }
    ## Coercing to character here to handle Rle/factor matching.
    all <- as.character(x[["geneName"]])
    ## Check for gene names (symbols).
    if (identical(length(all), 0L)) {
        return(false(
            "Gene names are not defined in {.var %s}.",
            toCauseName(x)
        ))
    }
    ## Require that the user passed in gene names.
    ok <- isSubset(genes, all)
    if (!isTRUE(ok)) {
        setdiff <- setdiff(genes, all)
        return(false(
            "Gene names missing in {.var %s}: %s",
            toCauseName(x), toString(setdiff, width = 100L)
        ))
    }
    ## Get a vector of all duplicated gene names in the object.
    dupes <- all[which(duplicated(all))]
    ## Now check for intersection with the user-defined genes vector.
    intersect <- intersect(genes, dupes)
    if (length(intersect) > 0L) {
        return(false(
            "Non-unique gene names in {.var %s}: %s",
            toCauseName(x), toString(intersect, width = 100L)
        ))
    }
    TRUE
}
