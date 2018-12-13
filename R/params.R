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
#' @param na.ok `logical(1)`.
#'   If set to `TRUE`, `x` may also be `NA`.
#' @param names `character`.
#'   Element names (e.g. row names, column names).
#' @param null.ok `logical(1)`.
#'   If set to `TRUE`, `x` may also be `NULL`.
#' @param object Object.
#' @param pattern `character(1)`.
#'   Pattern to use for matching.
#' @param x Object.
#' @param y Secondary object.
#' @param .var.name `character(1)`.
#'   Name of the checked object to print in asserions.
#' @param .xname *Not intended to be used directly.*
#' @param .yname *Not intended to be used directly.*
#' @param ... Additional arguments.
#'
#' @return
#' - `are*`/`contains*`/`has*`/`is*`: `logical(1)`.
#'   Boolean flag (`TRUE`/`FALSE`). Intended to be used in combination with
#'   `assert` function.
#' - `check*`: `TRUE` on success, `character(1)` on failure.
#'   Intended to be used in combination with `validate` function.
#' - `assert*`: `TRUE` on success, error (`stop`) on failure.
NULL
