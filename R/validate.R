# FIXME Can we get rid of the assertthat dependency here?

#' Validate an S4 Class
#'
#' `validate` is a variant of `assert` that is specifically intended to be
#' used inside of an S4 validity method definition.
#'
#' Currently, `validate` reexports `assertthat::validate_that`.
#'
#' Like `assert`, `validate` returns `TRUE` on success. However, on failure
#' it returns a `character` instead of a `stop` call. This is the current
#' recommended practice for defining S4 validity methods inside of a
#' `setValidity` call. Refer to the documentation in the methods package,
#' specifically on `validObject` for detailed information on S4 validity
#' methods.
#'
#' @importFrom assertthat validate_that
#' @inheritParams assertthat::validate_that
#' @export
#'
#' @seealso
#' - `methods::setValidity`.
#' - `methods::validObject`.
#' - `assertthat::validate_that`.
#'
#' @examples
#' validate(
#'     is.atomic("example"),
#'     is.character("example")
#' )
validate <- validate_that
