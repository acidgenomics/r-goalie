#' Parameters
#'
#' @name params
#' @keywords internal
#'
#' @param add `AssertCollection`.
#'   Extra information to be included in the message for the testthat reporter.
#'   See `testthat::expect_that()`.
#' @param classes `character`.
#'   Object classes.
#' @param envir `environment`.
#'   Environment.
#' @param genes `character`.
#'   Genes.
#' @param info `character`.
#'   Extra information to be included in the message for the testthat reporter.
#'   See `testthat::expect_that()`.
#' @param inherits `logical(1)`.
#'   Should the enclosing frames of the `environment` be searched?
#' @param label `character(1)`.
#'   Name of the checked object to print in messages.
#' @param name `character`.
#'   Element name.
#' @param names `character`.
#'   Element names (e.g. row names, column names).
#' @param nullOK `logical(1)`.
#'   If set to `TRUE`, `x` may also be `NULL`.
#' @param object Object.
#' @param pattern `character(1)`.
#'   Pattern to use for matching.
#' @param x Object.
#' @param y Secondary object.
#' @param .xname *Not intended to be used directly.*
#' @param .yname *Not intended to be used directly.*
#' @param ... Additional arguments.
#'
#' @return
#' `TRUE` on success;
#' `FALSE` on failure, with `cause` [attribute][base::attributes] set.
NULL
