#' Does the current session have an Internet connection?
#'
#' @name check-scalar-hasInternet
#' @note Updated 2023-08-24.
#'
#' @inherit check return
#'
#' @seealso
#' - `isAnExistingUrl()`.
#' - `Biobase::testBioCConnection()`.
#'
#' @examples
#' hasInternet()
NULL



#' @rdname check-scalar-hasInternet
#' @export
hasInternet <- function() {
    isAnExistingUrl("https://www.bioconductor.org/")
}
