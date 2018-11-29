#' Is Directory?
#'
#' @name isDir
#' @inherit params
#'
#' @examples
#' x <- "~"
#'
#' ## Requires scalar.
#' isDir(x)
#'
#' ## Parameterized.
#' areDirs(rep(x, times = 2L))
NULL



# isDir ========================================================================
#' @rdname isDir
#' @export
isDir <- R.utils::isDirectory

.msg.isDir <-  # nolint
    function(x) {
        paste(x, "is not an existing directory.")
    }

on_failure(isDir) <- function(call, env) {
    .msg.isDir(x = deparse(call[["x"]]))
}

#' @rdname isDir
#' @export
assertIsDir <- function(x) {
    assert_that(
        isDir(x),
        msg = .msg.isDir(x = deparse(substitute(x)))
    )
}



# areDirs ======================================================================
#' @rdname isDir
#' @export
areDirs <- function(x) {
    assert_that(is.character(x))
    all(dir.exists(x))
}

.msg.areDirs <- ## nolint
    function(x) {
        paste(x, "are not existing directories.")
    }

on_failure(areDirs) <- function(call, env) {
    .msg.areDirs(x = deparse(call[["x"]]))
}

#' @rdname isDir
#' @export
assertAreDirs <- function(x) {
    assert_that(
        areDirs(x),
        msg = .msg.areDirs(x = deparse(substitute(x)))
    )
}
