# FIXME Use checkmate variants instead of rlang where possible?
# (e.g. is_integerish, is_scalar_double, is_string)



#' goalie
#'
#' Assertive check functions for defensive R programming.
#'
#' @keywords internal
#'
#' @importFrom BiocGenerics duplicated rowSums
#' @importFrom R.utils isDirectory isFile
#' @importFrom assertive.base is2
#' @importFrom assertive.code is_existing
#' @importFrom assertive.data is_hex_color
#' @importFrom assertive.properties has_colnames has_dimnames
#' @importFrom assertthat on_failure<-
#' @importFrom checkmate assert assertClass assertLogical assertNames
#'   makeAssertion makeAssertionFunction makeExpectation makeExpectationFunction
#'   makeTest makeTestFunction testClass testFlag testScalar testString vname
#' @importFrom methods as is setGeneric setMethod signature
#' @importFrom utils globalVariables
#' @importFrom rlang is_integerish is_scalar_double
#'   is_scalar_integerish is_string
"_PACKAGE"

globalVariables(".")

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`
