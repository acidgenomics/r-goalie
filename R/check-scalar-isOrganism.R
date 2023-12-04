#' Is the string input a full Latin organism name?
#'
#' @details
#' The binomial system of naming species uses Latin words. Each name has two
#' parts, the genus and the species. For example, human beings belong to the
#' genus *Homo*, and our species is *sapiens* -- so the scientific name is
#' *Homo sapiens*.
#'
#' @name check-scalar-isOrganism
#' @note Updated 2023-12-04.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @examples
#' ## TRUE ====
#' isOrganism("Homo sapiens")
#' isOrganism("Canis lupus familiaris")
#'
#' ## FALSE ====
#' isOrganism("Human")
#' isOrganism("homo_sapiens")
NULL



#' @rdname check-scalar-isOrganism
#' @export
isOrganism <-
    function(x, nullOk = FALSE) {
        if (isTRUE(nullOk) && is.null(x)) {
            return(TRUE)
        }
        ok <- isString(x)
        if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- isMatchingRegex(
            x = x,
            pattern = "^[A-Z][a-z]+\\s[a-z]+(\\s[a-z]+)?$"
        )
        if (!isTRUE(ok)) {
            return(ok)
        }
        TRUE
    }
