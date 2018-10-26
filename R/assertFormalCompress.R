#' Assert Formal Compression
#'
#' @inherit assert
#' @export
#'
#' @examples
#' assertFormalCompress("xz")
assertFormalCompress <- function(object) {
    assert_is_any_of(object, classes = c("character", "logical"))
    if (is.character(object)) {
        assert_is_a_string(object)
        assert_is_subset(object, c("bzip2", "gzip", "xz"))
    }
}
