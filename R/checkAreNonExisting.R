#' Does the Requested Variable Exist?
#'
#' @name checkAreNonExisting
#' @aliases areNonExisting
#' @inherit params
#' @export
#'
#' @param x `character`. Variable names to check in `environment`.
#'
#' @examples
#' ## Pass ====
#' a <- NULL
#' b <- NULL
#' checkAreNonExisting(c("a", "b"))
#'
#' ## Fail ====
#' a <- 1L
#' b <- NULL
#' checkAreNonExisting(c("a", "b"))
checkAreNonExisting <- function(
    x,
    envir = parent.frame(),
    inherits = TRUE
) {
    ok <- all(!is_existing(x, envir = envir, inherits = inherits))
    # TODO Improve the message here.
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
