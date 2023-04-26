#' Perform an action quietly
#'
#' Suppress all warnings, messages, and console output.
#'
#' @export
#' @note Updated 2023-04-26.
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
    invisible({
        capture.output({
            suppressWarnings({
                suppressMessages({
                    expr
                })
            })
        })
        NULL
    })
}
