#' Parameters
#'
#' @name params
#' @keywords internal
#'
#' @param envir `environment`. Environment.
#' @param genes `character`. Genes.
#' @param inherits `boolean`. Should the enclosing frames of the `environment`
#'   be searched?
#' @param names `character`. Names (e.g. rownames, colnames).
#' @param x Object.
#' @param .var.name `string`. Name of the checked object to print in assertions.
#' @param add `AssertCollection`. Extra information to be included in the
#'   message for the testthat reporter. See `testthat::expect_that()`.
#' @param name XXX
#' @param classes `character`. Object classes.
#' @param ... Additional arguments.
#'
#' @seealso
#' - `checkmate::makeAssertionFunction()`.
#'
#' @return
#' - `boolean flag` for `are/has/is*()` functions.
#' - Stop on error for `assert*()` functions
#'   (see `stop()` or `stopifnot()` for details).
NULL
