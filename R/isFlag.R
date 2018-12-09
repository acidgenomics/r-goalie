#' Does the Argument Contain a Boolean Flag?
#'
#' @name isFlag
#' @importFrom checkmate testString
#' @inherit params
#' @export
#'
#' @examples
#' ## Pass ====
#' isFlag(TRUE)
#' isFlag(FALSE)
#'
#' ## Fail ====
#' isFlag("xxx")
isFlag <- checkmate::testFlag
