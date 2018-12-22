#' Does the input contain a boolean flag?
#'
#' @export
#' @inherit params
#'
#' @examples
#' ## Pass ====
#' isFlag(TRUE)
#' isFlag(FALSE)
#'
#' ## Fail ====
#' isFlag(c(TRUE, TRUE))
#' isFlag(1)
#' isFlag(NA)
isFlag <- function(x, .xname = getNameInParent(x)) {
    ok <- isScalarLogical(x)
    if (!isTRUE(ok)) {
        return(false("%s is not a boolean flag (TRUE/FALSE).", .xname))
    }

    # Check for NA, which is logical but not a flag.
    if (is.na(x)) {
        return(false("%s is NA", .xname))
    }

    TRUE
}