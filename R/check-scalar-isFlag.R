#' Does the input contain a boolean flag?
#'
#' @export
#' @inherit params
#'
#' @examples
#' ## TRUE ====
#' isFlag(TRUE)
#' isFlag(FALSE)
#'
#' ## FALSE ====
#' isFlag(c(TRUE, TRUE))
#' isFlag(1)
#' isFlag(NA)

# Updated 2019-07-15.
isFlag <- function(x, .xname = getNameInParent(x)) {
    ok <- isScalarLogical(x)
    if (!isTRUE(ok)) {
        return(false("%s is not a boolean flag (TRUE/FALSE).", .xname))
    }

    # Check for NA, which is logical but not a flag.
    if (is.na(x)) {
        return(false("%s is not a boolean flag (TRUE/FALSE).", .xname))
    }

    TRUE
}
