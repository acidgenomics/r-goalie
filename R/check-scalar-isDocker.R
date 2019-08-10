## nocov start



#' Is the R session running inside Docker?
#'
#' @name check-scalar-isDocker
#' @note Updated 2019-08-10.
#'
#' @inherit check
#' @inheritParams acidroxygen::params
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
        false("Docker image not detected.")
    }
}



#' @describeIn check-scalar-isDocker Utility function for testthat.
#' @export
skip_on_docker <-  # nolint
    function() {
        requireNamespace("testthat", quietly = TRUE)
        if (!isTRUE(isDocker())) {
            return()
        }
        testthat::skip("On Docker")
    }



## nocov end
