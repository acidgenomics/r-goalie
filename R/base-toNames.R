#' Sanitize vector input to names
#'
#' @export
#' @note Names resulting from this function do not necessarily return valid, and
#'   will not be identical to output from [`make.names()`][base::make.names()].
#'   The output is intended for downstream use with the [cause()] function.
#' @note Updated 2019-10-09.
#'
#' @inheritParams acidroxygen::params
#'
#' @return `character`.
#'
#' @seealso
#' - `assertive.base:::to_names()`.
#' - https://stackoverflow.com/questions/26183735
#'
#' @examples
#' ## Non-character vectors are supported.
#' toNames(1)
#' toNames(complex(1L))
#' toNames(NA)
#' toNames(TRUE)
#'
#' ## Doesn't use `make.names()` to sanitize.
#' toNames(c("sample-1", "hello world"))
toNames <- function(x) {
    ## Assert check for `is.vector()` instead of `is.atomic()` here will error
    ## out for `na.omit()` return.
    stopifnot(is.atomic(x))
    if (is.double(x)) {
        ifelse(
            test = is.na(x),
            yes = "NA",  # NA_real_
            no = sprintf("%.15e", x)
        )
    } else if (is.complex(x)) {
        ifelse(
            test = is.na(x),
            yes = "NA",  # NA_complex_
            no = sprintf("%.15g+%.15gi", Re(x), Im(x))
        )
    } else {
        x <- as.character(x)
        ifelse(
            test = is.na(x),
            yes = "NA",  # NA_character_
            no = sprintf("%s", x)
        )
    }
}
