#' Does the input contain a tximport list?
#'
#' @name check-scalar-isTximport
#' @note Updated 2023-07-21.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso `tximport::tximport`.
#'
#' @examples
#' ## TRUE ====
#' if (allAreInstalled(c("tximport", "tximportData"))) {
#'     dir <- system.file("extdata", package = "tximportData")
#'     samples <- read.table(file.path(dir, "samples.txt"), header = TRUE)
#'     files <- file.path(dir, "salmon", samples[["run"]], "quant.sf.gz")
#'     names(files) <- paste0("sample", seq(from = 1L, to = length(files)))
#'     object <- tximport::tximport(
#'         files = files,
#'         type = "salmon",
#'         txIn = TRUE,
#'         txOut = TRUE
#'     )
#'     isTximport(object)
#' }
#'
#' ## FALSE ====
#' object <- list()
#' isTximport(object)
NULL



#' @rdname check-scalar-isTximport
#' @export
isTximport <- function(x) {
    ok <- is.list(x)
    if (!isTRUE(ok)) {
        return(false(
            "{.var %s} is not a list.",
            toCauseName(x)
        ))
    }
    ok <- areIntersectingSets(
        x = c(
            "abundance",
            "counts",
            "countsFromAbundance",
            "infReps", # v1.9+
            "length"
        ),
        y = names(x)
    )
    if (!isTRUE(ok)) {
        return(false(
            "{.var %s} is not a tximport list.",
            toCauseName(x)
        ))
    }
    ok <- identical(
        x = dimnames(x[["abundance"]]),
        y = dimnames(x[["counts"]])
    )
    if (!isTRUE(ok)) {
        return(false(
            "{.var %s} has mismatched dimnames.",
            toCauseName(x)
        ))
    }
    ok <- identical(
        x = dimnames(x[["abundance"]]),
        y = dimnames(x[["length"]])
    )
    if (!isTRUE(ok)) {
        return(false(
            "{.var %s} has mismatched dimnames.",
            toCauseName(x)
        ))
    }
    ok <- isString(x[["countsFromAbundance"]])
    if (!isTRUE(ok)) {
        return(false(
            "{.var %s} is missing {.var %s} metadata.",
            toCauseName(x), "countsFromAbundance"
        ))
    }
    TRUE
}
