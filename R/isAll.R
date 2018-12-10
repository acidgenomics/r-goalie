#' Does the Object Belong to or Inherit All of These Classes?
#' @inherit params
#' @export
#' @examples
#' x <- 1L
#' isAll(x, classes = c("integer", "numeric"))
isAll <- function(x, classes) {
    all(is2(x, class = classes))
}
