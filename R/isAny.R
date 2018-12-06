#' Does an Object Belong to Any of These Classes?
#' @inheritParams params
#' @export
isAny <- function(x, classes) {
    any(is2(x = x, class = classes))
}
