## nocov start



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
    ## FIXME Return FALSE on internet failure.
    assert(hasInternet())
    ok <- isInstalled("BiocManager")
    if (!isTRUE(ok)) {
        return(ok)
    }
    requireNamespaces(c("BiocManager", "yaml"))
    version <- BiocManager::version()
    yaml <- yaml::read_yaml("https://bioconductor.org/config.yaml")
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



## nocov end
