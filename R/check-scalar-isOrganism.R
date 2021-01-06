#' Is the string input a full Latin organism name?
#'
#' @details
#' The binomial system of naming species uses Latin words. Each name has two
#' parts, the genus and the species. For example, human beings belong to the
#' genus *Homo*, and our species is *sapiens* -- so the scientific name is
#' *Homo sapiens*.
#'
#' @name check-scalar-isOrganism
#' @note Updated 2020-01-06.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @examples
#' ## TRUE ====
#' isOrganism("Homo sapiens")
#'
#' ## FALSE ====
#' isOrganism("Human")
#' isOrganism("homo_sapiens")
NULL



#' @rdname check-scalar-isOrganism
#' @export
isOrganism <- function(x, .xname = getNameInParent(x)) {
    ok <- isString(x)
    if (!isTRUE(ok)) return(ok)
    ok <- isMatchingRegex(x = x, pattern = "^[A-Z][a-z]+ [a-z]+$")
    if (!isTRUE(ok)) return(ok)
    TRUE
}
