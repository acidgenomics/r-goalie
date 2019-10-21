#' File extension
#'
#' @export
#' @note This function intentionally doesn't check whether a file exists.
#' @note Updated 2019-10-12.
#'
#' @param path `character`.
#'   File path(s).
#'   This function is vectorized and supports multiple files.
#'
#' @return `character`.
#' Character vector of same length as `path` input, with file extension removed.
#' Returns `NA` if no extension is detected.
#'
#' @seealso
#' - `tools::file_ext()`. Note that this returns `character()` instead of `NA`
#'   on extension match failure.
#'
#' @examples
#' fileExt(c("dir/foo.txt", "dir/bar.tar.gz", "dir/"))
fileExt <- function(path) {
    ## Note that `regexpr()` returns `-1L` on match failure.
    pos <- regexpr(pattern = extPattern, text = path)
    ifelse(
        test = pos > -1L,
        yes = substring(path, pos + 1L),
        no = NA_character_
    )
}
