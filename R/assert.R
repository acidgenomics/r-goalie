#' Assert that certain conditions are true
#'
#' [assert()] is a drop-in replacement for [stopifnot()] supporting more
#' informative
#' error messages.
#'
#' @note Don't use [assertive::assert_that()] as a general [stopifnot()]
#'   replacement. It doesn't currently catch everything, and isn't hardened
#'   against S4 methods.
#'
#' @inheritParams base::stopifnot
#' @export
#'
#' @param ... Any number of R expressions that return `logical(1)`, each of
#'   which should evaluate to `TRUE`.
#'
#' @seealso
#' - `stopifnot()`.
#' - `assertive.base::assert_engine()`.
#' - `assertthat::assert_that()`.
#' - `checkmate::assert()`.
#'
#' @examples
#' assert(
#'     is.atomic("example"),
#'     is.character("example")
#' )
assert <- function(..., envir = parent.frame()) {
    res <- seeIf(..., envir = envir)
    stopifnot(is.list(res))

    # Invisibly return TRUE when all checks pass.
    if (all(vapply(
        X = res,
        FUN = isTRUE,
        FUN.VALUE = logical(1L)
    ))) {
        return(invisible(TRUE))
    }

    # Otherwise display an assertion error.
    msg <- paste(
        "Assert check failure.",
        paste(
            vapply(
                X = Filter(f = Negate(isTRUE), x = res),
                FUN = cause,
                FUN.VALUE = character(1L)
            ),
            collapse = "\n"
        ),
        sep = "\n"
    )
    simpleError(message = msg, call = sys.call(-1L))
}




# When debugging code, switch back to `stopifnot()` for testing.
# assert <- stopifnot



# `base::stopifnot()` modification.
# assert <- function(...) {
#     cl <- match.call()[-1L]
#     Dparse <- function(call, cutoff = 60L) {
#         ch <- deparse(call, width.cutoff = cutoff)
#         if (length(ch) > 1L) {
#             paste(ch[1L], "....")
#         } else {
#             ch
#         }
#     }
#     head <- function(x, n = 6L) {
#         x[seq_len(if (n < 0L) max(length(x) + n, 0L) else min(n, length(x)))]
#     }
#     abbrev <- function(ae, n = 3L) {
#         paste(c(head(ae, n), if (length(ae) > n) "...."), collapse = "\n  ")
#     }
#     for (i in seq_along(cl)) {
#         cl.i <- cl[[i]]
#         r <- withCallingHandlers(
#             expr = tryCatch(
#                 expr = ...elt(i),
#                 error = function(e) {
#                     e$call <- cl.i
#                     stop(e)
#                 }
#             ),
#             warning = function(w) {
#                 w$call <- cl.i
#                 w
#             }
#         )
#         if (!(is.logical(r) && !anyNA(r) && all(r))) {
#             msg <- if (
#                 is.call(cl.i) &&
#                 identical(cl.i[[1]], quote(all.equal)) &&
#                 (
#                     is.null(ni <- names(cl.i)) ||
#                     length(cl.i) == 3L ||
#                     length(cl.i <- cl.i[!nzchar(ni)]) == 3L
#                 )
#             ) {
#                 sprintf(
#                     gettext("%s and %s are not equal:\n  %s"),
#                     Dparse(cl.i[[2]]),
#                     Dparse(cl.i[[3]]),
#                     abbrev(r)
#                 )
#             }
#             else {
#                 sprintf(
#                     ngettext(
#                         n = length(r),
#                         msg1 = "%s is not TRUE",
#                         msg2 = "%s are not all TRUE"
#                     ),
#                     Dparse(cl.i)
#                 )
#             }
#             stop(simpleError(msg, call = sys.call(-1)))
#         }
#     }
#     invisible()
# }
