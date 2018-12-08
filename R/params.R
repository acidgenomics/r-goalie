#' Parameters
#'
#' @name params
#' @keywords internal
#'
#' @param add `AssertCollection`. Extra information to be included in the
#'   message for the testthat reporter. See `testthat::expect_that()`.
#' @param classes `character`. Object classes.
#' @param envir `environment`. Environment.
#' @param genes `character`. Genes.
#' @param info `character`. Extra information to be included in the message for
#'   the testthat reporter. See `testthat::expect_that()`.
#' @param inherits `logical(1)`. Should the enclosing frames of the
#'   `environment` be searched?
#' @param label XXX
#' @param name `character`. Element name.
#' @param names `character`. Element names (e.g. row names, column names).
#' @param null.ok `logical(1)`. If set to `TRUE`, `x` may also be `NULL`.
#' @param object Object.
#' @param x Object.
#' @param y Secondary object.
#' @param .var.name `character(1)`. Name of the checked object to print in
#'   assertions.
#' @param ... Additional arguments.
#'
#' @return
#' - `boolean flag` for `are/has/is*()` functions.
#' - Stop on error for `assert*()` functions
#'   (see `stop()` or `stopifnot()` for details).
NULL
