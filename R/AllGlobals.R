## NOTE Enclosing in `\\b` word boundary here doesn't work.
## Updated 2021-10-07.
.cliPattern <- paste0(
    "\\{",
    "\\.[a-z]+\\s",
    "([^\\}]+)",
    "\\}"
)



## extPattern ==================================================================
#' File extension pattern matching
#'
#' @name extPattern
#' @note Updated 2020-01-04.
#' @keywords internal
#'
#' @examples
#' extPattern
#' compressExtPattern
NULL

.compressExtPattern <- "\\.(bz2|gz|xz|zip)"

#' @rdname extPattern
#' @export
extPattern <- paste0(
    "\\.([a-zA-Z0-9]+)",
    "(", .compressExtPattern, ")?$"
)

#' @rdname extPattern
#' @export
compressExtPattern <- paste0(.compressExtPattern, "$")



## tolerance ===================================================================
#' Set the tolerance limit
#'
#' @note Updated 2019-08-10.
#' @noRd
.tolerance <- 100L * .Machine[["double.eps"]]
