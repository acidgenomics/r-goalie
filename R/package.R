#' goalie
#'
#' Assertive check functions for defensive R programming.
#'
#' @keywords internal
#'
#' @importFrom BiocGenerics duplicated rowSums
#' @importFrom R.utils capitalize
#' @importFrom assertive.base is2
#' @importFrom assertive.code is_existing
#' @importFrom assertive.data is_hex_color
#' @importFrom assertive.properties has_colnames has_dimnames
#' @importFrom assertthat on_failure<-
#' @importFrom checkmate assert assertClass assertFlag assertLogical assertNames
#'   checkClass makeAssertion makeAssertionFunction makeExpectation
#'   makeExpectationFunction makeTest makeTestFunction testClass testFlag
#'   testScalar testString vname
#' @importFrom methods as is setGeneric setMethod signature
#' @importFrom utils globalVariables
"_PACKAGE"

globalVariables(".")

# Remove this in a future update, in favor of `assert()`.
#' @importFrom assertthat assert_that
#' @export
assertthat::assert_that

# Remove this in a future update, in favor of `validate()`.
#' @importFrom assertthat validate_that
#' @export
assertthat::validate_that

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`
