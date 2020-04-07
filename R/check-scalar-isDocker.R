#' Is the R session running inside Docker?
#'
#' @name check-scalar-isDocker
#' @note Updated 2020-04-07.
#'
#' @inherit check return
#'
#' @examples
#' isDocker()
NULL



#' @rdname check-scalar-isDocker
#' @export
isDocker <- function() {
    file <- file.path("", "proc", "1", "cgroup")
    ok <- isFile(file)
    if (!isTRUE(ok)) return(ok)
    ## nocov start
    x <- readLines(file)
    ok <- any(grepl(pattern = ":/docker/", x = x))
    if (!isTRUE(ok)) {
        return(false("Docker not detected."))
    }
    TRUE
    ## nocov end
}



## nocov start

#' @describeIn check-scalar-isDocker Utility function for testthat.
#' @export
skip_on_docker <-  # nolint
    function() {
        assert(requireNamespace("testthat", quietly = TRUE))
        if (!isTRUE(isDocker())) {
            return()
        }
        testthat::skip("On Docker")
    }

## nocov end
