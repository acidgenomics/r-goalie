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



# TODO Improve the message here.
#' @rdname checkAreNonExisting
#' @export
checkAreNonExisting <- function(
    x,
    envir = parent.frame(),
    inherits = TRUE
) {
    ok <- all(!is_existing(x, envir = envir, inherits = inherits))
    if (!ok) {
        return("Detected variables that already exist in the environment")
    }
    TRUE
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
