#' Has Aggregate Information
#' 
#' Determine whether an object can be aggregated automatically.
#' 
#' @export
hasAggregate <- function(object, stop = FALSE) {
    logical <- "aggregate" %in% colnames(object)
    if (
        identical(logical, FALSE) &&
        identical(stop, TRUE)
    ) {
        stop("`aggregate` column is required.", call. = FALSE)
    }
    logical
}
