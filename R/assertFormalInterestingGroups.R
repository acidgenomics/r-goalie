#' Interesting Groups Formal Assert Check
#'
#' Prevent unwanted downstream behavior when a missing interesting group
#' is requested by the user.
#'
#' @inherit assert
#' @export
#'
#' @param object S4 class object.
#' @param interestingGroups `character`. Interesting groups.
#'
#' @examples
#' library(basejump.data)
#' data(rse, package = "basejump.data")
#' assertFormalInterestingGroups(rse, "treatment")
#' assertFormalInterestingGroups(rse, NULL)
assertFormalInterestingGroups <- function(object, interestingGroups) {
    assert_that(isS4(object))
    data <- sampleData(object)
    
    # Check `interestingGroups` argument.
    if (is.null(interestingGroups)) {
        # Early return clean on `NULL` value (e.g. DESeqDataSet).
        return(invisible())
    } else {
        # Otherwise, require that `interestingGroups` is a character.
        assert_is_character(interestingGroups)
    }
    
    # Check intersection with sample data.
    assert_is_subset(interestingGroups, colnames(data))
    
    # Check that interesting groups columns are factors.
    invisible(lapply(
        X = data[, interestingGroups, drop = FALSE],
        FUN = assert_is_factor
    ))
}
