#' Does the Requested Variable Exist?
#'
#' @name checkAreNonExisting
#' @aliases areNonExisting
#' @inherit params
#'
#' @param x `character`. Variable names to check in `environment`.
#'
#' @examples
#' checkAreNonExisting(c("XXX", "YYY"))
NULL



.areNonExisting <- function(
    x,
    envir = parent.frame(),
    inherits = TRUE
) {
    all(!is_existing(x, envir = envir, inherits = inherits))
}



#' @rdname checkAreNonExisting
#' @export
checkAreNonExisting <- function(x) {
    if (isTRUE(.areNonExisting(x))) {
        TRUE
    } else {
        "Detected variables that already exist in the environment"
    }
}



#' @rdname checkAreNonExisting
#' @export
check_are_non_existing <- checkAreNonExisting



#' @rdname checkAreNonExisting
#' @export
testAreNonExisting <- makeTestFunction(checkAreNonExisting)



#' @rdname checkAreNonExisting
#' @export
test_are_non_existing <- testAreNonExisting



#' @rdname checkAreNonExisting
#' @export
assertAreNonExisting <- makeAssertionFunction(checkAreNonExisting)



#' @rdname checkAreNonExisting
#' @export
assert_are_non_existing <- assertAreNonExisting



#' @rdname checkAreNonExisting
#' @export
expect_are_non_existing <- makeExpectationFunction(checkAreNonExisting)
