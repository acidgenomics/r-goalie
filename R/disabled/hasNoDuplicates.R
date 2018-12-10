#' Does the Input Have Duplicates?
#'
#' @name hasDuplicates
#' @inherit params
#'
#' @examples
#' hasDuplicates(c("a", "a"))
#' hasNoDuplicates(c("a", "b"))
NULL



#' @rdname hasDuplicates
#' @importFrom assertive.properties has_duplicates
#' @export
hasDuplicates <- has_duplicates



#' @rdname hasDuplicates
#' @importFrom assertive.properties has_no_duplicates
#' @export
hasNoDuplicates <- has_no_duplicates
