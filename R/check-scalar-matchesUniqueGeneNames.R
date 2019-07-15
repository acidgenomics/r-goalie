#' Do the input gene names match unique values in the corresponding object?
#'
#' This assert check determines if a user-defined gene name query is using only
#' unique (non-amgibuous) symbols. It is designed to be used for gene plotting
#' particularly when performing single-cell RNA-seq marker analysis.
#'
#' @export
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
#'
#' ## TRUE ====
#' matchesUniqueGeneNames(x = x, genes = genes)

# Updated 2019-07-15.
matchesUniqueGeneNames <- function(x, genes, .xname = getNameInParent(x)) {
    ok <- isS4(x)
    if (!isTRUE(ok)) {
        return(false("Not an S4 class object."))
    }

    ok <- isCharacter(genes)
    if (!isTRUE(ok)) return(ok)

    # Get all of the gene names stashed in the x.
    if (is(x, "SummarizedExperiment")) {
        requireNamespace("SummarizedExperiment", quietly = TRUE)
        x <- SummarizedExperiment::rowData(x)
    }

    # Coercing to character here to handle Rle/factor matching.
    all <- as.character(x[["geneName"]])

    # Check for gene names (symbols).
    if (length(all) == 0L) {
        return(false("Gene names are not defined in object."))
    }

    # Require that the user passed in gene names.
    ok <- all(genes %in% all)
    if (!isTRUE(ok)) {
        setdiff <- setdiff(genes, all)
        return(false("Genes missing: %s", toString(setdiff)))
    }

    # Get a vector of all duplicated gene names in the object.
    dupes <- all[which(duplicated(all))]
    # Now check for intersection with the user-defined genes vector.
    intersect <- intersect(genes, dupes)

    if (length(intersect) > 0L) {
        return(false("Non-unique gene names: %s", toString(intersect)))
    }

    TRUE
}
