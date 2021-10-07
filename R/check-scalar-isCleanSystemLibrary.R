#' Is the R system library clean?
#'
#' @name check-scalar-isCleanSystemLibrary
#' @note Updated 2020-08-11.
#'
#' @inherit check return
#'
#' @examples
#' isCleanSystemLibrary()
NULL



#' @rdname check-scalar-isCleanSystemLibrary
#' @export
isCleanSystemLibrary <- function() {
    x <- utils::installed.packages()
    ## Subset information on base packages.
    base <- x[which(x[, "Priority"] == "base"), , drop = FALSE]
    ## Expect a single system library.
    syslib <- unique(base[, "LibPath"])
    if (!identical(length(syslib), 1L)) {
        return(false("Detected multiple system libraries."))
    }
    ## Subset packages in the system library.
    system <- x[which(x[, "LibPath"] == syslib), ]
    if (any(is.na(system[, "Priority"]))) {
        return(false("Detected user-installed packages in system library."))
    }
    ## Check for packages built against a different point release.
    version <- getRversion()
    stopifnot(all(grepl("^\\d\\.\\d\\.\\d$", version)))
    version <- gsub("\\.\\d$", "", version)
    if (!all(grepl(
        pattern = paste0("^", version),
        x = system[, "Built"]
    ))) {
        ## e.g. Ubuntu CRAN binary install now fails this.
        return(false("Detected packages built against a different release."))
    }
    TRUE
}
