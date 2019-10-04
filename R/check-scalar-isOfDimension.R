#' Does the input contain specific dimensions?
#'
#' @name check-scalar-isOfDimension
#' @note Updated 2019-10-04.
#'
#' @inherit check
#' @inheritParams acidroxygen::params
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



#' @rdname check-scalar-hasElements
#' @export
isOfDimension <- function(x, n, .xname = getNameInParent(x)) {
    assert((is.numeric(n) && length(n) == 2L) || is.null(n))
    dimX <- dim(x)
    if (is.null(n)) {
        if (hasDims(x)) {
            return(false(
                ngettext(
                    n = length(dimX),
                    msg1 = "'%s' has dimension %s, not NULL.",
                    msg2 = "'%s' has dimensions %s, not NULL."
                ),
                .xname,
                deparse(dimX)
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
                msg1 = "Dimension %s of '%s' is incorrect.",
                msg2 = "Dimensions %s of '%s' are incorrect."
            ),
            toString(notok),
            .xname
        ))
    }
    TRUE
}
