## nocov start



#' Is the R session running inside Docker?
#'
#' @name check-scalar-isDocker
#' @note Updated 2020-04-08.
#'
#' @inherit check return
#'
#' @examples
#' isDocker()
NULL



#' @rdname check-scalar-isDocker
#' @export
isDocker <-
    function() {
        file <- file.path("", "proc", "1", "cgroup")
        ok <- isFile(file)
        if (!isTRUE(ok)) return(ok)
        x <- readLines(file)
        ok <- any(grepl(pattern = ":/docker/", x = x))
        if (!isTRUE(ok)) {
            return(false("Docker not detected."))
        }
        TRUE
    }



#' @describeIn check-scalar-isDocker Utility function for testthat.
#' @export
skip_on_docker <-  # nolint
    function() {
        requireNamespaces("testthat")
        if (!isTRUE(isDocker())) return(invisible())
        testthat::skip("On Docker")
    }



## nocov end
