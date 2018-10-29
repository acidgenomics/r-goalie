#' basejump.assertions
#' 
#' Assertive check functions.
#' 
#' @importFrom assertive.base assert_all_are_not_na assert_all_are_true
#'   assert_are_identical assert_is_identical_to_na is_not_na
#' @importFrom assertive.code assert_all_are_existing is_existing
#' @importFrom assertive.data assert_all_are_hex_colors is_hex_color
#' @importFrom assertive.files assert_all_are_dirs assert_all_are_existing_files
#'   assert_all_are_non_empty_files
#' @importFrom assertive.numbers assert_all_are_greater_than
#'   assert_all_are_greater_than_or_equal_to assert_all_are_in_closed_range
#'   assert_all_are_in_open_range assert_all_are_in_range
#'   assert_all_are_non_negative assert_all_are_positive is_positive
#' @importFrom assertive.properties assert_are_same_length assert_has_colnames
#'   assert_has_dimnames assert_has_dims assert_has_names
#'   assert_has_no_duplicates assert_has_rownames assert_has_rows
#'   assert_is_atomic assert_is_empty assert_is_non_empty assert_is_not_null
#'   assert_is_null assert_is_of_length assert_is_scalar assert_is_vector
#'   has_colnames has_dimnames has_dims has_names has_rownames has_rows
#'   is_scalar
#' @importFrom assertive.sets assert_are_disjoint_sets
#'   assert_are_intersecting_sets assert_are_set_equal assert_is_subset
#'   is_subset
#' @importFrom assertive.strings assert_all_are_matching_regex
#'   assert_all_are_non_empty_character
#'   assert_all_are_non_missing_nor_empty_character
#'   assert_any_are_matching_regex
#' @importFrom assertive.types assert_is_a_bool assert_is_a_number
#'   assert_is_a_string assert_is_all_of assert_is_an_integer assert_is_any_of
#'   assert_is_call assert_is_character assert_is_data.frame
#'   assert_is_environment assert_is_factor assert_is_function assert_is_integer
#'   assert_is_list assert_is_matrix assert_is_name assert_is_numeric
#'   assert_is_symbol assert_is_tbl_df is_a_number is_a_string
#' @importFrom assertthat assert_that
#' @importFrom methods is
#' @importFrom utils globalVariables
"_PACKAGE"

globalVariables(".")

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`