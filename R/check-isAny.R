#' Does the Object Belong to or Inherit Any of These Classes?
#'
#' @inherit params
#' @export
#'
#' @examples
#' x <- 1L
#'
#' ## Pass ====
#' isAny(x, classes = c("integer", "NULL"))
#' isAny(x, classes = c("numeric", "NULL"))
#' isAny(x, classes = c("atomic", "NULL"))
#'
#' ## Fail ====
#' isAny(x, classes = c("character", "data.frame"))
isAny <- function(x, classes) {
    any(is2(x, class = classes))
}
