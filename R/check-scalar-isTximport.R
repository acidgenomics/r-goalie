#' Does the input contain a tximport list?
#'
#' @name check-scalar-isTximport
#' @note Updated 2023-07-21.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso `BiocParallel::bpparam`.
#'
#' @examples
#' ## TRUE ====
#'
#' ## FALSE ====
#' object <- list()
#' isTximport(object)
NULL



#' @rdname check-scalar-isTximport
#' @export
isTximport <- function(x, .xname = getNameInParent(x)) {
    ok <- is.list(x)
    if (!isTRUE(ok)) {
        return(false("{.var %s} is not a list.", .xname))
    }
    ok <- areIntersectingSets(
        x = c(
            "abundance",
            "counts",
            "countsFromAbundance",
            "infReps", # v1.9+
            "length"
        ),
        y = names(x)
    )
    if (!isTRUE(ok)) {
        return(false("{.var %s} is not a tximport list.", .xname))
    }
    ok <- identical(
        x = dimnames(x[["abundance"]]),
        y = dimnames(x[["counts"]])
    )
    if (!isTRUE(ok)) {
        return(false("{.var %s} has mismatched dimnames.", .xname))
    }
    ok <- identical(
        x = dimnames(x[["abundance"]]),
        y = dimnames(x[["length"]])
    )
    if (!isTRUE(ok)) {
        return(false("{.var %s} has mismatched dimnames.", .xname))
    }
    ok <- isString(x[["countsFromAbundance"]])
    if (!isTRUE(ok)) {
        return(false(
            "{.var %s} is missing {.var %s} metadata.",
            .xname, "countsFromAbundance"
        ))
    }
    TRUE
}
