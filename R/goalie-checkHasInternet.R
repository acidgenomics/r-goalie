#' Does the Current Session Have an Internet Connection?
#'
#' @aliases hasInternet has_internet
#' @inherit params
#' @export
#'
#' @examples
#' checkHasInternet()
checkHasInternet <- function() {
    ok <- has_internet()
    if (!ok) {
        return("Internet connection failure")
    }
    TRUE
}



#' @rdname checkHasInternet
#' @export
check_has_internet <- checkHasInternet  # nolint



#' @rdname checkHasInternet
#' @export
testHasInternet <- function() {
    isTRUE(checkHasInternet())
}



#' @rdname checkHasInternet
#' @export
test_has_internet <- testHasInternet  # nolint



#' @rdname checkHasInternet
#' @export
assertHasInternet <- function() {
    assert(checkHasInternet())
}



#' @rdname checkHasInternet
#' @export
assert_has_internet <- assertHasInternet  # nolint
