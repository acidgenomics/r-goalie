#' Assert Formal Compression
#'
#' @inherit params
#' @export
#'
#' @examples
#' assertFormalCompress("xz")
assertFormalCompress <- function(x) {
    assert_is_any_of(x, classes = c("character", "logical"))
    if (is.character(x)) {
        assert_is_a_string(x)
        assert_is_subset(x, c("bzip2", "gzip", "xz"))
    }
}
