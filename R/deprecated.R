# nocov start
# nolint start



#' Deprecated Functions
#' @name deprecated
#' @keywords internal
#' @inheritParams params
#' @return `.Deprecated()`.
NULL



#' Defunct Functions
#' @name defunct
#' @keywords internal
#' @inheritParams params
#' @return `.Defunct()`.
NULL



# basejump v0.4.0 ==============================================================
#' @rdname defunct
#' @export
assertIsAnnotable <- function(...) {
    .Defunct()
}



# basejump v0.5.4 ==============================================================
#' @rdname defunct
#' @export
assertIsGFF <- function(...) {
    .Defunct()
}



# v0.1.0 =======================================================================
#' @rdname deprecated
#' @export
assertAllAreNonExisting <- function(...) {
    .Deprecated("assertAreNonExisting")
    assertAreNonExisting(...)
}

#' @rdname deprecated
#' @export
assertAllAreValidNames <- function(...) {
    .Deprecated("assertAreValidNames")
    assertAreValidNames(...)
}

#' @rdname defunct
#' @export
assertAreGeneAnnotations <- function(...) {
    .Defunct(msg = "Create `Gene2Symbol` class object with basejump.")
}

#' @rdname defunct
#' @export
assertAreTranscriptAnnotations <- function(...) {
    .Defunct(msg = "Create `Tx2Gene` class object with basejump.")
}

#' @rdname deprecated
#' @export
assertIsAHeaderLevel <- function(...) {
    .Deprecated("assertIsHeaderLevel")
    assertIsHeaderLevel(...)
}

#' @rdname deprecated
#' @export
assertIsAStringOrNULL <- function(...) {
    # Soft deprecated: assertIsStringOrNULL
    assertIsStringOrNULL(...)
}

#' @rdname defunct
#' @export
assertIsCharacterOrNULL <- function(...) {
    .Defunct("assertive::assert_is_any_of")
}

#' @rdname defunct
#' @export
assertIsDataFrameOrNULL <- function(...) {
    .Defunct("assertive::assert_is_any_of")
}

#' @rdname defunct
#' @export
assertIsGene2symbol <- function(...) {
    .Defunct(msg = "Create `Gene2Symbol` class object with basejump.")
}

#' @rdname defunct
#' @export
assertIsTx2gene <- function(...) {
    .Defunct(msg = "Create `Tx2Gene` class object with basejump.")
}



# v0.1.3 =======================================================================
#' @rdname deprecated
#' @export
areSamplesUnique <- function(...) {
    .Deprecated("hasUniqueCols")
    hasUniqueCols(...)
}



# nolint end
# nocov end
