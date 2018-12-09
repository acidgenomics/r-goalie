#' Does the Object Belong to or Inherit Any of These Classes?
#' @inherit params
#' @export
#' @examples
#' x <- 1L
#' isAny(x, classes = c("integer", "character"))
#' isAny(x, classes = c("numeric", "character"))
isAny <- function(x, classes) {
    any(is2(x, class = classes))
}
