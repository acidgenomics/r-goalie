#' Make test function
#'
#' @name makeTest
#'
#' @param res `TRUE` or `character(1)`.
#'   The result of a check function: `TRUE` for successful checks, or send an
#'   error message as string otherwise.
#'
#' @param fun `function`.
#'   Check function, which returns either `TRUE` or `character(1)`.
#' @param envir `environment`.
#'   The environment of the created function.
#'   Defaults to [parent.frame()][base::parent.frame].
#'
#' @seealso
#' - `checkmate::makeTest()`.
#' - `checkmate::makeTestFunction()`.
NULL



#' @rdname makeTest
#' @export
makeTest <- function(res) {
    identical(res, TRUE)
}



#' @rdname makeTest
#' @export
makeTestFunction <- function(fun, envir = parent.frame()) {
    name <- deparse(substitute(fun))
    fun <- match.fun(fun)
    args <- paste0(formalArgs(fun), collapse = ", ")

    # Prepare the body of the test function.
    text <- sprintf("{ identical(%s(%s), TRUE) }", name, args)
    body <- parse(text = text)

    # Return as new function in desired environment.
    out <- function() TRUE
    formals(out) <- formals(fun)
    body(out) <- body
    environment(out) <- envir

    out
}
