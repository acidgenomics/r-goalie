#' Assert Are Gene Annotations
#'
#' Must contain `geneID` and `geneName` columns. Does not need to contain
#' rownames, so `tibble` class is supported.
#'
#' @inherit assert
#' @export
#'
#' @param object Object that can be coerced to `data.frame`.
#'
#' @examples
#' object <- tibble::tibble(
#'     geneID = "ENSG00000000003",
#'     geneName = "TSPAN6"
#' )
#' assertAreGeneAnnotations(object)
assertAreGeneAnnotations <- function(object) {
    df <- as.data.frame(object)
    assert_is_subset(c("geneID", "geneName"), colnames(df))
    assert_is_non_empty(df)
}
