#' goalie
#'
#' Assertive check functions for defensive R programming.
#'
#' @keywords internal
#'
#' @importFrom BiocGenerics anyDuplicated rowSums
#' @importFrom R.utils isDirectory isFile
#' @importFrom SummarizedExperiment assay
#' @importFrom assertive.base assert_are_identical
#' @importFrom assertive.code is_existing
#' @importFrom assertive.data assert_all_are_hex_colors is_hex_color
#' @importFrom assertive.properties assert_has_dims assert_has_dimnames
#'   assert_has_names assert_is_non_empty has_colnames has_dimnames has_rownames
#' @importFrom assertive.sets assert_are_disjoint_sets assert_is_subset
#' @importFrom assertive.types assert_is_a_bool assert_is_a_number
#'   assert_is_a_string assert_is_all_of assert_is_an_integer assert_is_any_of
#'   assert_is_character
#' @importFrom assertthat assert_that on_failure<-
#' @importFrom methods is
#' @importFrom utils globalVariables
#' @importFrom rlang is_integerish is_scalar_double is_scalar_integerish
#'   is_string
"_PACKAGE"

globalVariables(".")

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`
