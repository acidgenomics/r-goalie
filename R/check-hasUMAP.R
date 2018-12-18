#' Is the Python UMAP module available in the current session?
#' @export
#' @seealso
#' - [reticulate](https://cran.r-project.org/package=reticulate)
#' - [umap-learn](https://github.com/lmcinnes/umap)
#' @examples
#' hasUMAP()
hasUMAP <- function() {
    requireNamespace("reticulate", quietly = TRUE)
    reticulate::py_module_available(module = "umap")
}
