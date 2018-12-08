# TODO Consider reworking this along with `checkAnyClass()`.



#' @describeIn checkAnyClass Less strict variant that calls `assertive::is2()`
#'   internally, and allows for inherited classes to pass.
#' @export
isAny <- function(x, classes) {
    any(is2(x, class = classes))
}



#' @rdname checkAnyClass
#' @export
is_any <- isAny
