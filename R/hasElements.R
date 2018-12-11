#' Does the Input Have Elements?
#'
#' @name hasElements
#' @inherit params
#' @inheritParams assertive.properties::is_empty
#'
#' @examples
#' ## Pass ====
#' hasElements("hello")
#' hasElements(TRUE)
#' hasElements(list(a = 1, b = 2))
#'
#' ## Fail ====
#' hasElements(list())
NULL



#' @rdname hasElements
#' @importFrom assertive.properties is_empty
#' @export
isEmpty <- is_empty



#' @rdname hasElements
#' @importFrom assertive.properties is_non_empty
#' @export
isNonEmpty <- is_non_empty



#' @rdname hasElements
#' @importFrom assertive.properties has_elements
#' @export
hasElements <- has_elements



#' @rdname hasElements
#' @importFrom assertive.properties is_of_dimension
#' @export
isOfDimension <- is_of_dimension



# TODO Somewhat redundant with `hasLength()`. Work on resolution.
#' @rdname hasElements
#' @importFrom assertive.properties is_of_length
#' @export
isOfLength <- is_of_length



#' @rdname hasElements
#' @importFrom assertive.properties is_non_empty
#' @export
isNonEmpty <- is_non_empty
