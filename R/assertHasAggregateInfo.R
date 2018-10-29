#' Assert Has Aggregate Information
#' 
#' Determine whether an object can be aggregated automatically.
#' 
#' @name assertHasAggregateInfo
NULL



#' @rdname assertHasAggregateInfo
#' @export
assertHasAggregateInfo <- function(object) {
    assert_that(isTRUE(hasAggregateInfo))
    
}



#' @rdname assertHasAggregateInfo
#' @export
hasAggregateInfo <- function(object, stop = FALSE) {
    "aggregate" %in% colnames(object)
}



