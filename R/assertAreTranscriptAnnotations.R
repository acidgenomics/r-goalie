#' Assert Are Ensembl Transcript Annotations
#'
#' Must contain `transcriptID` and `transcriptName` columns. Does not need to
#' contain rownames, so `tibble` class is supported.
#'
#' @inherit params
#' @export
#'
#' @param object Object that can be coerced to `data.frame`.
#'
#'
#' @examples
#' object <- tibble::tibble(
#'     transcriptID = "ENST00000000233",
#'     geneID = "ENSG00000004059"
#' )
#' assertAreTranscriptAnnotations(object)
assertAreTranscriptAnnotations <- function(object) {
    df <- as.data.frame(object)
    assert_is_subset(c("transcriptID", "geneID"), colnames(df))
    assert_is_non_empty(object)
}
