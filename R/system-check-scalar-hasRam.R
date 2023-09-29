#' Does the current machine have a sufficient RAM?
#'
#' @name check-scalar-hasRam
#' @note Updated 2023-08-10.
#'
#' @inherit check return
#'
#' @param n `integer(1)`.
#' Minimum amount of RAM in gigabytes (GB).
#'
#' @examples
#' ## TRUE ====
#' hasRam(n = 1L)
#'
#' ## FALSE ====
#' hasRam(n = Inf)
NULL



#' @rdname check-scalar-hasRam
#' @export
hasRam <- function(n) {
    requireNamespaces("AcidBase")
    ram <- AcidBase::ram()
    ok <- ram >= n
    if (!isTRUE(ok)) {
        return(false(
            "Not enough RAM (in GB): %s (%s) < %s (%s).",
            as.character(ram), "current",
            as.character(n), "expected"
        ))
    }
    TRUE
}
