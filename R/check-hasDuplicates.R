#' Does the Input Have Duplicates?
#'
#' @name hasDuplicates
#' @importFrom assertive.properties has_duplicates has_no_duplicates
#' @inherit params
#'
#' @examples
#' hasDuplicates(c("a", "a"))
#' hasNoDuplicates(c("a", "b"))
NULL



#' @rdname hasDuplicates
#' @export
hasDuplicates <- has_duplicates



#' @rdname hasDuplicates
#' @export
hasNoDuplicates <- has_no_duplicates
