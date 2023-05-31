#' CLI extraction pattern
#'
#' @note Updated 2022-05-13.
#' @noRd
#'
#' @details
#' Enclosing in `\\b` word boundary here doesn't work.
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
#' @note Updated 2023-05-31.
#' @keywords internal
#'
#' @examples
#' extPattern
#' compressExtPattern
NULL

.compressExts <-
    c("7z", "br", "bz2", "gz", "lz", "lz4", "lzma", "xz", "z", "zip", "zst")

.compressExtPattern <-
    paste0("\\.(", paste(.compressExts, collapse = "|"), ")")

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
