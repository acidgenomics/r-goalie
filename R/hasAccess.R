#' Does the Session Have File System Access Rights?
#'
#' Works for either file or directory paths.
#'
#' @importFrom checkmate testAccess
#' @inheritParams checkmate::testAccess
#' @seealso `checkmate::testAccess()`.
#'
#' @examples
#' hasAccess("~")
hasAccess <- testAccess
# Default to read access check.
formals(hasAccess)[["access"]] <- "r"
