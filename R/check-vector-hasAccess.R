#' Check file system access rights
#'
#' Works for either file or directory paths.
#'
#' @export
#'
#' @param x `character(1)`.
#'   File or directory path.
#' @param access `character(1)`.
#'   String containing any of these characters, including in combination:
#'
#'   - `r` read.
#'   - `w`: write.
#'   - `x`: execute.
#'
#' Write and executable cannot be checked on Windows.
#'
#' @seealso `checkmate::checkAccess()`.
#'
#' @examples
#' hasAccess("~")
hasAccess <- function(x, access = "r") {
    requireNamespace("checkmate", quietly = TRUE)
    wf <- checkmate::wf

    xname <- getNameInParent(x)

    access <- tolower(access)
    access <- strsplit(access, "")[[1L]]

    isWin <- .Platform$OS.type == "windows"
    isRoot <- (!isWin && Sys.info()["user"] == "root")

    if (
        anyDuplicated(access) > 0L ||
        !all(access %in% c("r", "w", "x"))
    ) {
        return(false(
            paste0(
                "%s contains an invalid access code.\n",
                "Combinations of ‘r’, ‘w’ and ‘x’ are allowed."
            ),
            access
        ))
    }

    if ("r" %in% access || isTRUE(isRoot)) {
        w <- wf(file.access(x, 4L) != 0L)
        if (length(w) > 0L) {
            return(false("%s is not readable.", x[w]))
        }
    }

    if (!isTRUE(isWin)) {
        if ("w" %in% access || isTRUE(isRoot)) {
            w <- wf(file.access(x, 2L) != 0L)
            if (length(w) > 0L) {
                return(false("%s is not writeable.", x[w]))
            }
        }
        if ("x" %in% access) {
            w <- wf(file.access(x, 1L) != 0L)
            if (length(w) > 0L) {
                return(false("%s is not executable.", x[w]))
            }
        }
    }

    TRUE
}
