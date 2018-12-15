#' Does R Have File System Access Rights?
#'
#' Works for either file or directory paths.
#'
#' @importFrom checkmate testAccess
#' @inheritParams checkmate::testAccess
#' @seealso `checkmate::testAccess`.
#' @export
#'
#' @examples
#' hasAccess("~")
hasAccess <- testAccess
# Default to read access check.
formals(hasAccess)[["access"]] <- "r"
