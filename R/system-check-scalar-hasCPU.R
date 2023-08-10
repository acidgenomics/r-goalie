#' Does the current machine have a sufficient number of CPU cores?
#'
#' @name check-scalar-hasCPU
#' @note Updated 2023-08-10.
#'
#' @inherit check return
#'
#' @param n `integer(1)`.
#' Minimum number of CPU cores.
#'
#' @examples
#' ## TRUE ====
#' hasCPU(n = 1L)
#'
#' ## FALSE ====
#' hasCPU(n = Inf)
NULL



#' @rdname check-scalar-hasCPU
#' @export
hasCPU <- function(n) {
    assert(
        isInt(n),
        requireNamespaces("AcidBase")
    )
    cpus <- AcidBase::cpus()
    ok <- cpus >= n
    if (!isTRUE(ok)) {
        return(false(
            "Not enough CPU cores: %s (%s) < %s (%s).",
            as.character(cpus), "current",
            as.character(n), "expected"
        ))
    }
    TRUE
}
