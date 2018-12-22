#' Check file system access rights
#'
#' Works for either file or directory paths.
#'
#' @name hasAccess
#'
#' @param x `character(1)`.
#'   File or directory path(s).
#' @param access `character(1)`.
#'   String containing any of these characters, including in combination:
#'
#'   - `r`: read.
#'   - `w`: write.
#'   - `x`: execute.
#'
#' Write and executable status cannot be checked on Windows.
#'
#' @seealso
#' - `file.access()`.
#' - `checkmate::checkAccess()`.
#'
#' @examples
#' ## Pass ====
#' hasAccess(c("~", "."))
#'
#' ## Fail ====
#' hasAccess("xxx")
NULL



#' @describeIn hasAccess Vectorized.
#' @export
hasAccess <- function(x, access = "r") {
    # `file.access()` mode values:
    # - 0: [-] existence
    # - 1: [x] execute
    # - 2: [w] write
    # - 4: [r] read
    # Note that `file.access()` will return 0 on success, -1 on failure.

    # Here we're converting the "rwx" flags to the file.access modes.
    access <- tolower(access)
    access <- strsplit(access, "")[[1L]]
    if (anyDuplicated(access) > 0L || !all(access %in% c("r", "w", "x"))) {
        return(false(
            paste0(
                "%s doesn't contain valid codes.\n",
                "Combinations of 'r', 'w' and 'x' are allowed."
            ),
            access
        ))
    }

    isWindows <- .Platform[["OS.type"]] == "windows"

    # String file checker that we can loop with `bapply()` below.
    checkAccess <- function(x, access) {
        if ("r" %in% access) {
            ok <- file.access(x, mode = 4L) == 0L
            if (!isTRUE(ok)) return(FALSE)
        }

        # Write/execute permissions can't be checked on Windows.
        if (!isTRUE(isWindows)) {
            if ("w" %in% access) {
                ok <- file.access(x, mode = 2L) == 0L
                if (!isTRUE(ok)) return(FALSE)
            }
            if ("x" %in% access) {
                ok <- file.access(x, mode = 1L) == 0L
                if (!isTRUE(ok)) return(FALSE)
            }
        }

        TRUE
    }

    bapply(X = x, FUN = checkAccess, access = access)
}



#' @describeIn hasAccess Scalar.
#' @export
allHaveAccess <- function() {
    ok <- hasAccess(x = x, access = access)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}

formals(allHaveAccess) <- formals(hasAccess)