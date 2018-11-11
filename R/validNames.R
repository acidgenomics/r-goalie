#' Valid Names
#'
#' @name validNames
#' @inherit params
#'
#' @examples
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
#'
#' list <- list(example = 1L)
#' names(list)
#' assertHasValidNames(list)
#'
#' # Check rows and columns (dimnames) in a single call.
#' validDimnames(datasets::iris)    # TRUE
#' validDimnames(datasets::mtcars)  # FALSE
NULL



# validNames ===================================================================
#' @rdname validNames
#' @export
validNames <- function(names) {
    if (!is.character(names)) {
        FALSE
    } else {
        identical(names, make.names(names, unique = TRUE))
    }
}

#' @rdname validNames
#' @export
assertAreValidNames <- function(names) {
    assert_that(validNames(names))
}

#' @rdname validNames
#' @export
assertHasValidNames <- function(x) {
    assert_has_names(x)
    invisible(lapply(names(x), assertAreValidNames))
}



# validDimnames ================================================================
#' @rdname validNames
#' @export
validDimnames <- function(x) {
    # Error if x doesn't support dim.
    assert_that(!is.null(dim(x)))

    # Return TRUE if there are no names.
    if (!has_dimnames(x)) {
        return(TRUE)
    }

    # Check rows.
    if (hasRownames(x)) {
        validRows <- validNames(rownames(x))
    } else {
        validRows <- TRUE
    }
    assert_is_a_bool(validRows)

    # Check columns.
    if (has_colnames(x)) {
        validCols <- validNames(colnames(x))
    } else {
        validCols <- TRUE
    }
    assert_is_a_bool(validCols)

    all(c(validRows, validCols))
}

#' @rdname validNames
#' @export
assertHasValidDimnames <- function(x) {
    assert_has_dimnames(x)
    assert_all_are_true(validDimnames(x))
}
