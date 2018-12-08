#' Does the Requested Variable Exist?
#'
#' @name checkAreNonExisting
#' @aliases areNonExisting are_non_existing
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
    if (!ok) {
        # TODO Improve the message here, indicating which.
        return("Detected variables that already exist in the environment")
    }
    TRUE
}



#' @rdname checkAreNonExisting
#' @export
check_are_non_existing <- checkAreNonExisting  # nolint



#' @rdname checkAreNonExisting
#' @export
testAreNonExisting <- makeTestFunction(checkAreNonExisting)



#' @rdname checkAreNonExisting
#' @export
test_are_non_existing <-  # nolint
    testAreNonExisting



#' @rdname checkAreNonExisting
#' @export
assertAreNonExisting <- makeAssertionFunction(checkAreNonExisting)



#' @rdname checkAreNonExisting
#' @export
assert_are_non_existing <-  # nolint
    assertAreNonExisting



#' @rdname checkAreNonExisting
#' @export
expect_are_non_existing <-  # nolint
    makeExpectationFunction(checkAreNonExisting)
