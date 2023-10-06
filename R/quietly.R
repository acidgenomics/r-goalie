#' Perform an action quietly
#'
#' Suppress all warnings, messages, and console output.
#'
#' @export
#' @note Updated 2023-10-06.
#'
#' @param expr Expression to evaluate.
#'
#' @return Invisible `NULL`.
#'
#' @examples
#' quietly({
#'     message("hello world")
#'     object <- c("aaa", "bbb")
#' })
#' print(object)
quietly <- function(expr) {
    stopifnot(requireNamespace("utils", quietly = TRUE))
    invisible({
        utils::capture.output({
            suppressWarnings({
                suppressMessages({
                    expr
                })
            })
        })
        NULL
    })
}
