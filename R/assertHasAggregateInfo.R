#' Assert Has Aggregate Information
#'
#' Determine whether an object can be aggregated automatically.
#'
#' @name assertHasAggregateInfo
#' @inherit params
NULL



#' @rdname assertHasAggregateInfo
#' @export
hasAggregateInfo <- function(object) {
    "aggregate" %in% colnames(object)
}



#' @rdname assertHasAggregateInfo
#' @export
assertHasAggregateInfo <- function(object) {
    assert_that(isTRUE(hasAggregateInfo))
}
