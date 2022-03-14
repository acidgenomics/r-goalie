#' Is the package installed and a specific version?
#'
#' @name check-vector-isPackageVersion
#' @note Updated 2020-08-11.
#'
#' @param x `character`.
#' Named character vector.
#' Name corresponds to package name, and value corresponds to minimum version.
#'
#' @param op `character(1)`.
#' Mathematical operator.
#' Defaults to less than or equal to.
#'
#' @examples
#' ## TRUE ====
#' isPackageVersion(
#'     x = c(
#'         "base" = packageVersion("base"),
#'         "utils" = packageVersion("utils")
#'     ),
#'     op = "=="
#' )
#'
#' ## FALSE ====
#' isPackageVersion(
#'     x = c(
#'         "base" = packageVersion("base"),
#'         "utils" = packageVersion("utils")
#'     ),
#'     op = ">"
#' )
NULL



## Vector ======================================================================
#' @describeIn check-vector-isPackageVersion Vectorized.
#' @export
isPackageVersion <- function(x, op = ">=") {
    packages <- basename(names(x))
    versions <- package_version(x)
    op <- get(x = op, inherits = TRUE)
    assert(is.primitive(op))
    ok <- mapply(
        package = packages,
        version = versions,
        MoreArgs = list(op = op),
        FUN = function(package, version, op) {
            if (!isInstalled(package)) {
                return(FALSE)
            }
            op(e1 = packageVersion(package), e2 = version)
        },
        SIMPLIFY = TRUE,
        USE.NAMES = TRUE
    )
    setCause(ok, false = "version check fail")
}
