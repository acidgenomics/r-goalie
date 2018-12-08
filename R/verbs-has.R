# hasColnames ==================================================================
#' @rdname checkHasColnames
#' @export
hasColnames <- testHasColnames

#' @describeIn checkHasColnames snake alias.
#' @export
has_colnames <-  # nolint
    hasColnames



# hasCols ======================================================================
has_cols

has_dimnames

has_dims



# hasRows ======================================================================
has_rows



# hasRownames ==================================================================
has_rownames



# hasName ======================================================================
#' @importFrom rlang has_name
#' @export
rlang::has_name

#' @rdname reexports
#' @usage NULL
#' @export
hasName <- has_name



# hasNames =====================================================================
#' @describeIn reexports `checkmate::testNames`.
#' @importFrom checkmate testNames
#' @usage NULL
#' @export
hasNames <- checkmate::testNames

#' @describeIn reexports `hasNames` snake alias.
#' @usage NULL
#' @export
has_names <- hasNames
