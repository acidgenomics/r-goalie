#' Is the Argument a String?
#'
#' @name isString
#' @importFrom checkmate testString
#' @inherit params
#' @inheritParams checkmate::testString
#' @export
#'
#' @examples
#' ## Pass ====
#' isString("a")
#'
#' ## Fail ====
#' isString(c("a", "b"))
isString <- checkmate::testString
