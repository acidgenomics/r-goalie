#' Does the Input Have Dimensions?
#'
#' @name hasDims
#' @inherit params
#'
#' @examples
#' ## Pass ====
#' x <- datasets::mtcars
#' hasDims(x)
#' hasDimnames(x)
#' hasRows(x)
#' hasRownames(x)
#' hasCols(x)
#' hasColnames(x)
#'
#' ## Note that dims don't have to be non-zero, just not NULL.
#' hasDims(data.frame())
#'
#' ## Fail ====
#' x <- data.frame()
#' hasDims(list())
#' hasDimnames(x)
#' hasRows(x)
#' hasRownames(x)
#' hasCols(x)
#' hasColnames(x)
NULL



#' @rdname hasDims
#' @importFrom assertive.properties has_dims
#' @export
hasDims <- has_dims



#' @rdname hasDims
#' @importFrom assertive.properties has_dimnames
#' @export
hasDimnames <- has_dimnames



#' @rdname hasDims
#' @importFrom assertive.properties has_rows
#' @export
hasRows <- has_rows



#' @rdname hasDims
#' @importFrom assertive.properties has_cols
#' @export
hasCols <- has_cols



#' @rdname hasDims
#' @importFrom assertive.properties has_colnames
#' @export
hasColnames <- has_colnames
