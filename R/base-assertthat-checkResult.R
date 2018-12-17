# @seealso `assertthat:::check_result`.
checkResult <- function(x) {
    if (!is.logical(x))
        stop(
            "assert: assertion must return a logical value.",
            call. = FALSE
        )
    if (any(is.na(x)))
        stop(
            "assert: missing values present in assertion.",
            call. = FALSE
        )
    if (length(x) != 1) {
        stop(
            "assert: length of assertion is not 1.",
            call. = FALSE
        )
    }
    TRUE
}
