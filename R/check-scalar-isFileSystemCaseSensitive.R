#' Is the file system case sensitive?
#'
#' @details
#' Linux partitions in general are case sensitive by default.
#' macOS APFS and HFS partitions still default to case insensitive.
#' Windows partitions default to case insensitive.
#'
#' This function checks for case sensitivity internally by creating (touching)
#' two invisible files with names that only differ by case. If 2 files are
#' detected, the file system is case sensitive.
#'
#' @name check-scalar-isFileSystemCaseSensitive
#' @note Updated 2019-10-21.
#'
#' @inherit check
#' @inheritParams acidroxygen::params
#'
#' @seealso
#' `_koopa_is_file_system_case_sensitive` shell function in koopa.
#'
#' @examples
#' isFileSystemCaseSensitive()
NULL



#' @rdname check-scalar-isFileSystemCaseSensitive
#' @export
isFileSystemCaseSensitive <- function(dir = ".") {
    ok <- isADirectory(dir)
    if (!isTRUE(ok)) return(ok)
    files <- file.path(dir, c(".tmp.checkcase", ".tmp.checkCase"))
    unlink(files)
    file.create(files, showWarnings = FALSE)
    n <- length(list.files(
        path = dir,
        pattern = ".tmp.checkcase",
        all.files = TRUE,
        full.names = FALSE,
        recursive = FALSE,
        ignore.case = TRUE
    ))
    unlink(files)
    ok <- identical(n, 2L)
    if (!isTRUE(ok)) {
        return(false("'%s' is not case sensitive.", dir))
    }
    TRUE
}
