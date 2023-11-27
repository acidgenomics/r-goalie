#' Perform an action quietly
#'
#' Suppress all warnings, messages, and console output.
#'
#' @export
#' @note Updated 2023-11-27.
#'
#' @param expr Expression to evaluate.
#'
#' @return Invisible `NULL`.
#'
#' @seealso
#' - `help("conditions")`
#' - `sink()` and `nullfile()`.
#' - `utils::capture.output()`.
#' - https://stackoverflow.com/questions/2723034/
#' - https://stackoverflow.com/questions/18730491/
#' - https://stackoverflow.com/questions/25320381/
#' - https://stackoverflow.com/questions/4948361/
#'
#' @examples
#' quietly({
#'     message("hello world")
#'     object <- c("aaa", "bbb")
#' })
#' print(object)
quietly <- function(expr) {
    ## nolint start
    withCallingHandlers(
        expr = {
            sink(file = nullfile(), type = "output")
            expr
            sink()
        },
        error = function(e) {
            sink()
            simpleError(e)
        },
        message = function(m) {
            invokeRestart("muffleMessage")
        },
        warning = function(w) {
            invokeRestart("muffleWarning")
        }
    )
    invisible(NULL)
    ## nolint end
}
