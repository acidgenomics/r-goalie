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
assert <- stopifnot



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



# `assertthat::assert_that()` modification.
# assert <- function (..., env = parent.frame(), msg = NULL) {
#     res <- see_if(..., env = env, msg = msg)
#     if (res)
#         return(TRUE)
#     stop(assertError(attr(res, "msg")))
# }



# `assertive::assert_engine()` modification.
# assert <- function(
#     predicate,
#     ...,
#     msg = "The assertion failed.",
#     what = c("all", "any"),
#     na_ignore = FALSE,
#     severity = c("stop", "warning", "message", "none")
# ) {
#     handler_type <- match.arg(severity)
#     dots <- list(...)
#     return_value <- if (length(dots) > 0)
#         dots[[1]]
#     else NULL
#     if (handler_type == "none") {
#         return(invisible(return_value))
#     }
#     what <- match.fun(match.arg(what))
#     predicate_name <- get_name_in_parent(predicate)
#     ok <- predicate(...)
#     if (inherits(ok, "scalar_with_cause")) {
#         if (!isTRUE(ok)) {
#             if (missing(msg)) {
#                 msg <- cause(ok)
#             }
#             give_feedback(handler_type, msg, predicate_name)
#         }
#     }
#     else {
#         really_ok <- if (na_ignore) {
#             ok | is.na(ok)
#         }
#         else {
#             ok & !is.na(ok)
#         }
#         if (!what(really_ok)) {
#             msg <- paste(enc2utf8(msg), print_and_capture(ok),
#                          sep = "\n")
#             give_feedback(handler_type, msg, predicate_name)
#         }
#     }
#     invisible(return_value)
# }




# `checkmate::assert()` modification.
# assert <- function(..., combine = "or", .var.name = NULL) {
#     assertChoice(combine, c("or", "and"))
#     dots = match.call(expand.dots = FALSE)$...
#     env = parent.frame()
#     if (combine == "or") {
#         msgs = character(length(dots))
#         for (i in seq_along(dots)) {
#             val = eval(dots[[i]], envir = env)
#             if (identical(val, TRUE))
#                 return(invisible(TRUE))
#             msgs[i] = as.character(val)
#         }
#         if (is.null(.var.name))
#             .var.name = vapply(dots, function(x) as.character(x)[2L],
#                                FUN.VALUE = NA_character_)
#         if (length(msgs) > 1L) {
#             msgs = sprintf("%s(%s): %s", vapply(dots, function(x) as.character(x)[1L],
#                                                 FUN.VALUE = NA_character_), .var.name, msgs)
#             msgs = paste0(c("One of the following must apply:",
#                             strwrap(msgs, prefix = " * ")), collapse = "\n")
#             mstop("Assertion failed. %s", msgs)
#         }
#         else {
#             mstop("Assertion on '%s' failed. %s.", .var.name,
#                   msgs)
#         }
#     }
#     else {
#         for (i in seq_along(dots)) {
#             val = eval(dots[[i]], envir = env)
#             if (!identical(val, TRUE)) {
#                 if (is.null(.var.name))
#                     .var.name = as.character(dots[[1L]])[2L]
#                 mstop("Assertion on '%s' failed. %s.", .var.name,
#                       val)
#             }
#         }
#     }
#     invisible(TRUE)
# }
