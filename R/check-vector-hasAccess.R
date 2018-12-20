#' Check file system access rights
#'
#' Works for either file or directory paths.
#'
#' @export
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
#' Write and executable cannot be checked on Windows.
#'
#' @seealso
#' - `file.access()`.
#' - `checkmate::checkAccess()`. Note that this uses C code internally.
#'
#' @examples
#' hasAccess("~")
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

    isWindows <- .Platform[["OS.type"]] == "windows"

    if (
        anyDuplicated(access) > 0L ||
        !all(access %in% c("r", "w", "x"))
    ) {
        return(false(
            paste0(
                "%s contains an invalid access code.\n",
                "Combinations of 'r', 'w' and 'x' are allowed."
            ),
            access
        ))
    }

    if ("r" %in% access) {
        ok <- file.access(x, mode = 4L) == 0L
        if (!all(ok)) {
            return(false("%s is not readable.", x[!ok]))
        }
    }

    # Write/execute permissions can't be checked on Windows.
    if (!isTRUE(isWindows)) {
        if ("w" %in% access) {
            ok <- file.access(x, mode = 2L) == 0L
            if (!all(ok)) {
                return(false("%s is not writeable.", x[!ok]))
            }
        }
        if ("x" %in% access) {
            ok <- file.access(x, mode = 1L) == 0L
            if (!all(ok)) {
                return(false("%s is not executable.", x[!ok]))
            }
        }
    }

    TRUE
}
