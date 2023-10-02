#' Sanitize vector input to cause names
#'
#' @name toCauseNames
#' @note Updated 2023-10-02.
#'
#' @details
#' Names resulting from this function do not necessarily return valid, and will
#' not be identical to output from [`make.names()`][base::make.names()].
#'
#' @param x `atomic`.
#'
#' @return `character`.
#'
#' @seealso
#' - `assertive.base:::to_names()`.
#' - https://stackoverflow.com/questions/26183735
#'
#' @examples
#' ## Non-character vectors are supported.
#' toCauseNames(1)
#' toCauseNames(complex(1L))
#' toCauseNames(NA)
#' toCauseNames(TRUE)
#'
#' ## Doesn't use 'make.names()' to sanitize.
#' toCauseNames(c("sample-1", "hello world"))
NULL



## Vector ======================================================================

#' @rdname toCauseNames
#' @export
toCauseNames <- function(x) {
    if (is.null(x)) {
        return("NULL")
    }
    if (!is.atomic(x)) {
        return(as.character(class(x))[[1L]])
    }
    if (is.double(x)) {
        x <- ifelse(
            test = is.na(x),
            yes = "NA", # NA_real_
            no = sprintf("%.15e", x)
        )
    } else if (is.complex(x)) {
        x <- ifelse(
            test = is.na(x),
            yes = "NA", # NA_complex_
            no = sprintf("%.15g+%.15gi", Re(x), Im(x))
        )
    } else {
        x <- as.character(x)
        x <- ifelse(
            test = is.na(x),
            yes = "NA", # NA_character_
            no = sprintf("%s", x)
        )
    }
    x
}



#' @describeIn toCauseNames Scalar variant.
#' @export
toCauseName <- function(x) {
    if (is.null(x)) {
        return("NULL")
    }
    cls <- as.character(class(x))[[1L]]
    if (!is.atomic(x)) {
        return(cls)
    }
    if (identical(length(x), 1L)) {
        return(as.character(x))
    }
    cls
}
