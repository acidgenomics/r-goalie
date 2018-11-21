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
#' @param ... Additional arguments.
#'
#' @return
#' - `boolean flag` for `are/has/is*()` functions.
#' - Stop on error for `assert*()` functions
#'   (see `base::stop()` or `base::stopifnot()` for details).
NULL
