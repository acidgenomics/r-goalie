#' Does an Object Have an Element with This Name?
#'
#' @name hasName
#' @inheritParams params
#'
#' @export
NULL



#' @rdname hasName
#' @export
hasName <- has_name

#' @importFrom rlang has_name
#' @usage NULL
#' @export
rlang::has_name



#' @rdname hasName
#' @export
assertHasName <- makeAssertionFunction(hasName)

#' @rdname hasName
#' @export
assert_has_name <- assertHasName
