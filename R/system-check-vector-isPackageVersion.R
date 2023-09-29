#' Is the package installed and a specific version?
#'
#' @name check-vector-isPackageVersion
#' @note Updated 2022-05-13.
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



#' @describeIn check-vector-isPackageVersion Vectorized.
#' @export
isPackageVersion <- function(x, op = ">=") {
    packages <- basename(names(x))
    versions <- package_version(x)
    op <- get(x = op, inherits = TRUE)
    ok <- as.logical(Map(
        f = function(package, version, op) {
            if (!isInstalled(package)) {
                return(FALSE)
            }
            op(e1 = packageVersion(package), e2 = version)
        },
        package = packages,
        version = versions,
        MoreArgs = list("op" = op)
    ))
    names(ok) <- names(x)
    setCause(ok, false = "version check fail")
}
