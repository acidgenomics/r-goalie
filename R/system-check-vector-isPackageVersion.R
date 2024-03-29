#' Is the package installed and a specific version?
#'
#' @name check-vector-isPackageVersion
#' @note Updated 2023-10-06.
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
#'         "base" = utils::packageVersion("base"),
#'         "utils" = utils::packageVersion("utils")
#'     ),
#'     op = "=="
#' )
#'
#' ## FALSE ====
#' isPackageVersion(
#'     x = c(
#'         "base" = utils::packageVersion("base"),
#'         "utils" = utils::packageVersion("utils")
#'     ),
#'     op = ">"
#' )
NULL



#' @describeIn check-vector-isPackageVersion Vectorized.
#' @export
isPackageVersion <- function(x, op = ">=") {
    ok <- hasLength(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- isCharacter(x) || is(x, "package_version")
    if (!isTRUE(ok)) {
        ko <- rep(x = FALSE, times = length(x))
        return(setCause(ko, false = "not character or package version"))
    }
    packages <- basename(names(x))
    versions <- package_version(x)
    op <- get(x = op, inherits = TRUE)
    requireNamespaces("utils")
    ok <- unlist(Map(
        f = function(package, version, op) {
            if (!isInstalled(package)) {
                return(FALSE)
            }
            op(e1 = utils::packageVersion(package), e2 = version)
        },
        package = packages,
        version = versions,
        MoreArgs = list("op" = op),
        USE.NAMES = FALSE
    ))
    setCause(ok, false = "version check fail")
}
