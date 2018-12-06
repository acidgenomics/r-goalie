#' Does an Object Belong to Any of These Classes?
#' @inheritParams params
#' @export
isAny <- function(x, class) {
    any(is2(x = x, class = class))
}
