#' Is the R session running inside Docker?
#'
#' @name check-scalar-isDocker
#' @inherit params
#'
#' @examples
#' isDocker()
NULL



#' @rdname check-scalar-isDocker
#' @export
isDocker <- function() {
    if (
        identical(Sys.getenv("DOCKER"), "True") ||
        identical(Sys.getenv("HOME"), "/root") ||
        identical(Sys.info()[["user"]], "root")
    ) {
        TRUE
    } else {
        FALSE
    }
}



# nocov start

#' @describeIn check-scalar-isDocker Utility function for testthat.
#' @export
skip_on_docker <- function() {
    requireNamespace("testthat", quietly = TRUE)
    if (!isTRUE(isDocker())) {
        return()
    }
    testthat::skip("On Docker")
}

# nocov end
