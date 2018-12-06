#' Assert Check
#' @importFrom assertthat assert_that
#' @inheritParams params
#' @export
assertthat::assert_that -> assert

#' Is a Boolean Flag?
#' @importFrom assertthat is.flag
#' @inheritParams params
#' @export
assertthat::is.flag -> isFlag

#' Validity Check
#' @importFrom assertthat validate_that
#' @inheritParams params
#' @export
assertthat::validate_that -> validate
