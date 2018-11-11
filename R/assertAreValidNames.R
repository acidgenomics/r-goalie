#' Assert Are Valid Names
#'
#' @name assertAreValidNames
#' @inherit params
#'
#' @examples
#' data(rse)
#' gr <- SummarizedExperiment::rowRanges(rse)
#'
#' # Dots (periods) and underscores are valid.
#' validNames(c("sample.1", "sample_1"))
#'
#' # Can't begin with a number.
#' validNames("293cells")
#'
#' # Spaces, dashes (hyphens), and other non-alphanumerics aren't valid.
#' validNames("sample 1")
#' validNames("cell-AAAAAAAA")
#' validNames("GFP+")
#'
#' assertAreValidNames(c("sample1", "sample2"))
#' assertHasValidNames(gr)
#'
#' # Check rows and columns (dimnames) in a single call.
#' validDimnames(datasets::iris)    # TRUE
#' validDimnames(datasets::mtcars)  # FALSE
#' assertHasValidDimnames(rse)
NULL



#' @rdname assertAreValidNames
#' @export
validNames <- function(names) {
    if (!is.character(names)) {
        FALSE
    } else {
        identical(names, make.names(names, unique = TRUE))
    }
}



#' @rdname assertAreValidNames
#' @export
assertAreValidNames <- function(names) {
    assert_that(validNames(names))
}



#' @rdname assertAreValidNames
#' @export
assertHasValidNames <- function(object) {
    assert_has_names(object)
    invisible(lapply(names(object), assertAreValidNames))
}



#' @rdname assertAreValidNames
#' @export
validDimnames <- function(object) {
    # Error if object doesn't support dim.
    assert_that(!is.null(dim(object)))

    # Return TRUE if there are no names.
    if (!has_dimnames(object)) {
        return(TRUE)
    }

    # Check rows.
    if (hasRownames(object)) {
        validRows <- validNames(rownames(object))
    } else {
        validRows <- TRUE
    }
    assert_is_a_bool(validRows)

    # Check columns.
    if (has_colnames(object)) {
        validCols <- validNames(colnames(object))
    } else {
        validCols <- TRUE
    }
    assert_is_a_bool(validCols)

    all(c(validRows, validCols))
}



#' @rdname assertAreValidNames
#' @export
assertHasValidDimnames <- function(object) {
    assert_has_dimnames(object)
    assert_all_are_true(validDimnames(object))
}
