#' Does the input contain a (non-empty) character vector?
#'
#' @section Enforced parameters:
#'
#' - Must have length.
#' - Cannot contain `NA` strings.
#' - Cannot contain empty (`""`) strings.
#'
#' @name check-scalar-isCharacter
#' @note Updated 2019-07-29.
#'
#' @inherit check
#' @inheritParams acidroxygen::params
#'
#' @seealso `assertive.strings::is_non_missing_nor_empty_character()`.
#'
#' @examples
#' ## TRUE ====
#' isCharacter("a")
#'
#' ## FALSE ====
#' isCharacter(NULL)
#' isCharacter(character())
#' isCharacter("")
#' isCharacter(NA_character_)



#' @rdname check-scalar-isCharacter
#' @export
isCharacter <- function(
    x,
    nullOK = FALSE,
    .xname = getNameInParent(x)
) {
    assert(isFlag(nullOK))

    ## Conditionally allow NULL.
    if (isTRUE(nullOK) && is.null(x)) {
        return(TRUE)
    }

    ok <- is.character(x)
    if (!isTRUE(ok)) {
        return(false("%s is not character.", .xname))
    }

    ## Don't allow `character(0)`.
    ok <- hasLength(x, .xname = .xname)
    if (!isTRUE(ok)) return(ok)

    ## Don't allow empty strings ("").
    ok <- nzchar(x)
    if (!all(ok)) {
        return(false(
            "%s has empty string at: %s.",
            .xname, toString(which(!ok))
        ))
    }

    ## Don't allow `NA_character_`.
    ok <- !is.na(x)
    if (!all(ok)) {
        return(false(
            "%s has NA at: %s.",
            .xname, toString(which(!ok))
        ))
    }

    TRUE
}
