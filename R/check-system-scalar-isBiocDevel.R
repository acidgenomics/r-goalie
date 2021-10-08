#' Is the current Bioconductor installation under development?
#'
#' @name check-scalar-isBiocDevel
#' @note Updated 2021-01-04.
#'
#' @inherit check return
#'
#' @examples
#' isBiocDevel()
NULL



#' @rdname check-scalar-isBiocDevel
#' @export
isBiocDevel <- function() {
    assert(hasInternet())
    ok <- isInstalled("BiocManager")
    if (!isTRUE(ok)) {
        return(ok)
    }
    assert(
        requireNamespace("BiocManager", quietly = TRUE),
        requireNamespace("yaml", quietly = TRUE)
    )
    version <- BiocManager::version()
    yaml <- yaml::read_yaml("https://bioconductor.org/config.yaml")
    assert(isSubset("devel_version", names(yaml)))
    develVersion <- yaml[["devel_version"]]
    ok <- identical(x = as.character(version), y = as.character(develVersion))
    if (!isTRUE(ok)) {
        return(false(
            "Not Bioc Devel: {.val %s}.",
            as.character(version)
        ))
    }
    TRUE
}
