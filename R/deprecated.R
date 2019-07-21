## nocov start
## nolint start



#' Deprecated functions
#'
#' @name deprecated
#' @keywords internal
#' @inheritParams params
#' @seealso [Deprecated][base::Deprecated].
#' @return `.Deprecated`.
#'
#' @examples
#' a <- function(...) {
#'     .Deprecated("b")
#'     b(...)
#' }
#'
#' b <- function(x) {
#'     x + 1L
#' }
#'
#' withCallingHandlers(
#'     expr = a(1L),
#'     warning = function(w) {
#'         print(w)
#'         invokeRestart("muffleWarning")
#'     }
#' )
NULL



#' Defunct functions
#'
#' @name defunct
#' @keywords internal
#' @inheritParams params
#' @seealso [Defunct][base::Defunct].
#' @return `.Defunct`.
#'
#' @examples
#' a <- function(...) {
#'     .Defunct("b")
#' }
#'
#' withCallingHandlers(
#'     expr = tryCatch(
#'         expr = a(1L),
#'         error = function(e) {
#'             print(e)
#'             invisible()
#'         }
#'     )
#' )
NULL



## v0.2.1 =======================================================================
#' @rdname deprecated
#' @export
containsAlpha <- appendToBody(
    fun = isAlpha,
    values = quote(.Deprecated("isAlpha"))
)

#' @rdname deprecated
#' @export
containsHeaderLevel <- appendToBody(
    fun = isHeaderLevel,
    values = quote(.Deprecated("isHeaderLevel"))
)

#' @rdname deprecated
#' @export
containsHexColors <- appendToBody(
    fun = allAreHexColors,
    values = quote(.Deprecated("allAreHexColors"))
)

#' @rdname deprecated
#' @export
containsURL <- isURL

#' @rdname deprecated
#' @export
containsAURL <- isAURL





## nolint end
## nocov end
