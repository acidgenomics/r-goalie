#' Check file system access rights
#'
#' Works for either file or directory paths.
#'
#' @name hasAccess
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
NULL



.hasAccess <- function(x, access = "r") {
    access <- tolower(access)
    access <- strsplit(access, "")[[1L]]
    if (
        anyDuplicated(access) > 0L ||
        !all(access %in% c("r", "w", "x"))
    ) {
        stop("Access pattern invalid. Allowed are 'r', 'w' and 'x'.")
    }
    isWin <- .Platform$OS.type == "windows"
    isRoot <- (!isWin && Sys.info()["user"] == "root")
    if ("r" %in% access || isTRUE(isRoot)) {
        w <- wf(file.access(x, 4L) != 0L)
        if (length(w) > 0L) {
            return(sprintf("'%s' not readable", x[w]))
        }
    }
    if (!isTRUE(isWin)) {
        if ("w" %in% access || isTRUE(isRoot)) {
            w <- wf(file.access(x, 2L) != 0L)
            if (length(w) > 0L) {
                return(sprintf("'%s' not writeable", x[w]))
            }
        }
        if ("x" %in% access) {
            w <- wf(file.access(x, 1L) != 0L)
            if (length(w) > 0L) {
                return(sprintf("'%s' not executable", x[w]))
            }
        }
    }
    TRUE
}



#' @rdname hasAccess
#' @export
hasAccess <- makeTestFunction(.hasAccess)
