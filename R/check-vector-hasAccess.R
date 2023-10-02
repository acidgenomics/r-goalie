#' Check file system access rights
#'
#' Works for either file or directory paths.
#'
#' @name check-vector-hasAccess
#' @note Updated 2023-09-29.
#'
#' @inherit check return
#'
#' @param x `character(1)`.
#' File or directory path(s).
#'
#' @param access `character(1)`.
#' String containing any of these characters, including in combination:
#'
#' - `r`: read.
#' - `w`: write.
#' - `x`: execute.
#'
#' Write and executable status cannot be checked on Windows.
#'
#' @seealso
#' - `file.access()`.
#' - `checkmate::checkAccess()`.
#'
#' @examples
#' ## TRUE ====
#' hasAccess(c("~", "."))
#'
#' ## FALSE ====
#' hasAccess("xxx")
NULL



## `file.access()` mode values:
## - 0: [-] existence
## - 1: [x] execute
## - 2: [w] write
## - 4: [r] read
## Function returns `0` on success, `-1` on failure.



## Vector ======================================================================

#' @describeIn check-vector-hasAccess Vectorized.
#' @export
## Updated 2022-12-14.
hasAccess <- function(x, access = "r") {
    ok <- hasLength(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- isCharacter(x)
    if (!isTRUE(ok)) {
        ko <- rep(x = FALSE, times = length(x))
        return(setCause(ko, false = "not character"))
    }
    ## Here we're converting the "rwx" flags to the `file.access()` modes.
    access <- strsplit(
        x = tolower(access),
        split = "",
        fixed = TRUE
    )[[1L]]
    ## String file checker that we can loop with `bapply()` below.
    checkAccess <- function(x, access) {
        if (isSubset("r", access)) {
            ok <- identical(unname(file.access(x, mode = 4L)), 0L)
            if (!isTRUE(ok)) {
                return(FALSE)
            }
        }
        ## Write/execute permissions can't be checked on Windows.
        if (!isWindows()) {
            if (isSubset("w", access)) {
                ok <- identical(unname(file.access(x, mode = 2L)), 0L)
                if (!isTRUE(ok)) {
                    return(FALSE)
                }
            }
            if (isSubset("x", access)) {
                ok <- identical(unname(file.access(x, mode = 1L)), 0L)
                if (!isTRUE(ok)) {
                    return(FALSE)
                }
            }
        }
        TRUE
    }
    ok <- bapply(X = x, FUN = checkAccess, access = access)
    setCause(ok, false = "no access")
}



## Scalar ======================================================================

#' @describeIn check-vector-hasAccess Scalar.
#' @export
## Updated 2022-05-13.
allHaveAccess <- function(x, access = "r") {
    ok <- hasAccess(x = x, access = access)
    if (!all(ok)) {
        return(falseFromVector(ok))
    }
    TRUE
}
