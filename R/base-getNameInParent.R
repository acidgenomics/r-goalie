safeDeparse <- function(expr, ...) {
    paste0(deparse(expr, width.cutoff = 500L, ...), collapse = "")
}



getNameInParent <- function(x, escapePercent = TRUE) {
    xname <- safeDeparse(do.call(substitute, list(substitute(x), parent.frame())))
    if (isTRUE(escapePercent)) {
        xname <- gsub("%", "%%", xname)
    }
    xname
}
