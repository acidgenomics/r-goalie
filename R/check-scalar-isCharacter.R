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
#' @inheritParams AcidRoxygen::params
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
#'
#' @rdname check-scalar-isCharacter
#' @export
isCharacter <-
    function(x,
             nullOk = FALSE,
             .xname = getNameInParent(x)) {
        if (isTRUE(nullOk) && is.null(x)) {
            return(TRUE)
        }
        ok <- is.character(x)
        if (!isTRUE(ok)) {
            return(false("{.var %s} is not character.", .xname))
        }
        ## Don't allow `character(0)`.
        ok <- hasLength(x, .xname = .xname)
        if (!isTRUE(ok)) {
            return(ok)
        }
        ## Don't allow empty strings (`""`).
        ok <- nzchar(x)
        if (!all(ok)) {
            return(false(
                "{.var %s} has empty string at: %s.",
                .xname, toString(which(!ok), width = 50L)
            ))
        }
        ## Don't allow `NA_character_`.
        ok <- !is.na(x)
        if (!all(ok)) {
            return(false(
                "{.var %s} has {.val %s} at: %s.",
                .xname, "NA", toString(which(!ok), width = 50L)
            ))
        }
        TRUE
    }
