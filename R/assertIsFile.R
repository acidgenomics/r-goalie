#' Assert Is File
#'
#' @inherit params
#' @inheritParams params
#' @export
#'
#' @examples
#' assertIsFile(system.file("extdata/example.rds", package = "basejump"))
assertIsFile <- function(file) {
    assert_is_a_string(file)
    assert_that(file.exists(file))
}
