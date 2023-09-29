#' Does the input contain specific dimensions?
#'
#' @name check-scalar-isOfDimension
#' @note Updated 2021-10-08.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso
#' - `assertive.properties::is_of_dimension()`.
#'
#' @examples
#' x <- data.frame(a = c(TRUE, FALSE), b = c(FALSE, TRUE))
#' dim(x)
#'
#' ## TRUE ====
#' isOfDimension(x, n = c(2L, 2L))
#'
#' ## FALSE ====
#' isOfDimension(x, n = c(1L, 2L))
#' isOfDimension(x, n = c(2L, 1L))
NULL



#' @rdname check-scalar-isOfDimension
#' @export
isOfDimension <- function(x, n, .xname = getNameInParent(x)) {
    dimX <- dim(x)
    if (is.null(n)) {
        if (hasDims(x)) {
            return(false(
                sprintf(
                    "{.var %s} has %s {.val %s}, not {.val %s}.",
                    .xname,
                    ngettext(
                        n = length(dimX),
                        msg1 = "dimension",
                        msg2 = "dimensions"
                    ),
                    deparse(dimX),
                    "NULL"
                )
            ))
        }
        return(TRUE)
    }
    ok <- dimX == n
    if (!all(ok)) {
        notok <- which(!ok)
        return(false(
            ngettext(
                n = length(notok),
                msg1 = "Dimension %s of {.var %s} is incorrect.",
                msg2 = "Dimensions %s of {.var %s} are incorrect."
            ),
            toString(notok, width = 50L),
            .xname
        ))
    }
    TRUE
}
