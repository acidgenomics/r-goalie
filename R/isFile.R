#' Is File?
#'
#' @name isFile
#' @inherit params
#'
#' @examples
#' x <- system.file("extdata/example.rds", package = "basejump")
#'
#' ## Requires scalar.
#' isFile(x)
#'
#' ## Parameterized.
#' areFiles(rep(x, times = 2L))
NULL



# isFile =======================================================================
#' @rdname isFile
#' @export
isFile <- function(x) {
    assert_that(
        is_string(x),
        !dir.exists(x)
    )
    file.exists(x)
}

.msg.isFile <- function(x) {
    paste(x, "is not an existing file.")
}

on_failure(isFile) <- function(call, env) {
    .msg.isFile(x = deparse(call[["x"]]))
}

#' @rdname isFile
#' @export
assertIsFile <- function(x) {
    assert_that(
        isFile(x),
        msg = .msg.isFile(x = deparse(substitute(x)))
    )
}



# areFiles ======================================================================
#' @rdname isFile
#' @export
areFiles <- function(x) {
    assert_that(
        is.character(x),
        !any(dir.exists(x))
    )
    all(file.exists(x))
}

.msg.areFiles <- function(x) {
    paste(x, "are not existing files.")
}

on_failure(areFiles) <- function(call, env) {
    .msg.areFiles(x = deparse(call[["x"]]))
}

#' @rdname isFile
#' @export
assertAreFiles <- function(x) {
    assert_that(
        areFiles(x),
        msg = .msg.areFiles(x = deparse(substitute(x)))
    )
}
