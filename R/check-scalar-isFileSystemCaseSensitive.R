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
#' @export
#' @note Updated 2019-10-21.
#'
#' @inheritParams acidroxygen::params
#'
#' @seealso
#' `_koopa_is_file_system_case_sensitive` shell function in koopa.
#'
#' @examples
#' isFileSystemCaseSensitive()
isFileSystemCaseSensitive <- function(dir = ".") {
    ok <- isADirectory(dir)
    if (!isTRUE(ok)) return(ok)
    files <- file.path(dir, c(".acid-checkcase", ".acid-checkCase"))
    file.create(files, showWarnings = FALSE)
    n <- length(list.files(
        path = dir,
        pattern = ".acid-checkcase",
        all.files = TRUE,
        full.names = FALSE,
        recursive = FALSE,
        ignore.case = TRUE
    ))
    ok <- identical(n, 2L)
    if (!isTRUE(ok)) {
        return(false("'%s' is not case sensitive.", dir))
    }
    unlink(files)
    TRUE
}
