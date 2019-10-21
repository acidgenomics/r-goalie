#' File extension pattern matching
#'
#' @name extPattern
#' @note Updated 2019-10-21.
#'
#' @examples
#' extPattern
#' compressExtPattern
NULL

#' @rdname extPattern
#' @name extPattern
#' @export
NULL

#' @rdname extPattern
#' @name compressExtPattern
#' @export
NULL

compressExtPattern <- "\\.(bz2|gz|xz|zip)"
extPattern <- paste0(
    "\\.([a-zA-Z0-9]+)",
    "(", compressExtPattern, ")?$"
)
compressExtPattern <- paste0(compressExtPattern, "$")
