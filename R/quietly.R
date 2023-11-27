## FIXME This doesn't close sink on error correctly currently.



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
#' @seealso
#' - `sink()` and `nullfile()`.
#' - `utils::capture.output()`.
#' - https://stackoverflow.com/questions/2723034/
#'
#' @examples
#' quietly({
#'     message("hello world")
#'     object <- c("aaa", "bbb")
#' })
#' print(object)
quietly <- function(expr) {
    ## nolint start
    invisible({
        sink(file = nullfile(), type = "output")
        suppressWarnings({
            suppressMessages({
                expr
            })
        })
        sink()
        NULL
    })
    ## nolint end
}
