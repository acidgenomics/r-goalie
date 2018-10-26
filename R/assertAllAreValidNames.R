#' Assert All Are Valid Names
#'
#' @inherit assert
#' @export
#'
#' @examples
#' assertAllAreValidNames(c("sample1", "sample2"))
assertAllAreValidNames <- function(object) {
    assert_is_character(object)
    assert_is_non_empty(object)
    assert_all_are_non_missing_nor_empty_character(object)
    assert_has_no_duplicates(object)
    assert_all_are_true(validNames(object))
}



#' @rdname assertAllAreValidNames
#' @export
assertHasValidDimnames <- function(object) {
    assert_has_dimnames(object)
    assert_all_are_true(validDimnames(object))
}



#' @rdname assertAllAreValidNames
#' @export
assertHasValidNames <- function(object) {
    assert_has_names(object)
    invisible(lapply(names(object), assertAllAreValidNames))
}



#' @rdname assertAllAreValidNames
#' @export
#' @examples
#' validDimnames(datasets::mtcars)
validDimnames <- function(object) {
    if (is.null(dim(object))) {
        stop("Object does not support `dim()`.")
    } else if (!has_dimnames(object)) {
        c(TRUE, TRUE)
    } else {
        vapply(
            X = lapply(
                X = dimnames(object),
                FUN = function(x) {
                    # Return `FALSE` on duplicates.
                    if (any(duplicated(x))) {
                        FALSE
                    } else {
                        validNames(x)
                    }
                }
            ),
            FUN = all,
            FUN.VALUE = logical(1L)
        )
    }
}



#' @rdname assertAllAreValidNames
#' @export
#' @examples
#' validNames(c(
#'     "sample_1",
#'     "gene_1",
#'     "293cells",
#'     "cell-AAAAAAAA",
#'     "GFP+ sort"
#' ))
validNames <- function(object) {
    if (!is.atomic(object)) {
        FALSE
    } else if (is.null(object)) {
        TRUE
    } else {
        vapply(
            X = object,
            FUN = function(object) {
                # Note that we're enforcing unique values here.
                identical(
                    x = as.character(object),
                    y = make.names(object, unique = TRUE)
                )
            },
            FUN.VALUE = logical(1L),
            USE.NAMES = FALSE
        )
    }
}
