#' Match arguments to `do.call`
#'
#' @note Updated 2019-07-29.
#' @export
#'
#' @inheritParams standardizeCall
#' @inheritParams base::do.call
#' @inheritParams acidroxygen::params
#' @param removeFormals `character`.
#'   Names of formal arguments to remove from `args` list.
#'
#' @return `list`.
#' Arguments to pass to [`do.call()`][base::do.call].
#'
#' @seealso
#' - `standardizeCall()`.
#' - `do.call()`.
#'
#' @examples
#' ## Match the arguments in call.
#' fun <- function(object, xxx, ...) {
#'     args <- matchArgsToDoCall(
#'         args = list(object = object, collapse = " "),
#'         removeFormals = "xxx"
#'     )
#'     args
#' }
#' fun(c("hello", "world"))
#'
#' ## Pass the arguments to `do.call()`.
#' fun <- function(object, xxx, ...) {
#'     do.call(
#'         what = paste,
#'         args = matchArgsToDoCall(
#'             args = list(collapse = " "),
#'             removeFormals = "xxx"
#'         )
#'     )
#' }
#' fun(c("hello", "world"))
matchArgsToDoCall <- function(
    args = NULL,
    removeFormals = NULL,
    which = sys.parent(n = 1L),
    verbose = getOption("goalie.traceback", FALSE)
) {
    assert(
        isAny(args, classes = c("list", "NULL")),
        isAny(removeFormals, classes = c("character", "NULL")),
        isInt(which),
        isFlag(verbose)
    )
    if (is.list(args)) {
        assert(hasLength(args), hasNames(args))
    } else {
        args <- list()  # nocov
    }
    if (which < 1L) {
        which <- 1L  # nocov
    }
    list <- standardizeCall(
        which = which,
        defaults = TRUE,
        expandDots = TRUE,
        return = "list",
        verbose = verbose
    )
    definition <- list[["definition"]]
    call <- list[["match.call"]]
    assert(
        is.function(definition),
        is.call(call)
    )
    ## Prepare the args list.
    callArgs <- as.list(call)[-1L]
    callArgs <- callArgs[setdiff(names(callArgs), names(args))]
    args <- c(args, callArgs)
    ## This step shouldn't be necessary when using `defaults = TRUE` above.
    formalArgs <- formals(definition)
    formalArgs <- formalArgs[setdiff(names(formalArgs), "...")]
    formalArgs <- formalArgs[setdiff(names(formalArgs), names(args))]
    args <- c(args, formalArgs)
    ## Remove formals we want to exclude.
    args <- args[setdiff(names(args), removeFormals)]
    ## Show the unevaluated args, if desired.
    if (isTRUE(verbose)) {
        print(list(args = lapply(args, class)))  # nocov
    }
    ## Ensure all arguments are evaluated.
    ## Missing or NULL arguments will be stripped.
    ## https://stackoverflow.com/questions/16740307
    envir <- sys.frame(which = which)
    args <- lapply(
        X = args,
        FUN = function(expr) {
            if (is.call(expr) || is.name(expr) || is.symbol(expr)) {
                tryCatch(
                    expr = eval(expr = expr, envir = envir),
                    error = function(e) NULL
                )
            } else {
                expr
            }
        }
    )
    ## Remove any `NULL` arguments. We may want to consider changing this
    ## approach in the future, in case passing `NULL` through is important.
    args <- Filter(f = Negate(is.null), x = args)
    ## Enable verbose mode, for debugging.
    if (isTRUE(verbose)) {
        ## nocov start
        print(list(
            definition = definition,
            call = call,
            args = lapply(args, class)
        ))
        ## nocov end
    }
    assert(hasNames(args), hasNoDuplicates(names(args)))
    invisible(lapply(
        X = args,
        FUN = function(x) {
            assert(as.logical(
                !isAny(x, classes = c("call", "name", "symbol"))
            ))
        }
    ))
    args
}
