#' Does the Input Belong to or Inherit All of These Classes?
#'
#' @inherit params
#' @export
#'
#' @examples
#' x <- 1L
#'
#' ## Pass ====
#' isAll(x, classes = c("integer", "numeric"))
#'
#' ## Fail ====
#' isAll(x, classes = c("integer", "NULL"))
isAll <- function(x, classes) {
    all(is2(x, class = classes))
}
