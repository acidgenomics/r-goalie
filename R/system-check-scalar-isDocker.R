## nocov start



#' Is the R session running inside Docker?
#'
#' @name check-scalar-isDocker
#' @note Updated 2021-01-04.
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
        if (!isTRUE(ok)) {
            return(ok)
        }
        x <- readLines(file)
        ok <- any(grepl(pattern = ":/docker/", x = x))
        if (!isTRUE(ok)) {
            return(false("Docker not detected."))
        }
        TRUE
    }



## nocov end
