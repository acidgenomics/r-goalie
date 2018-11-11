# nocov start
# nolint start



#' Deprecated Functions
#'
#' @name deprecated
#' @keywords internal
#'
#' @inheritParams params
#'
#' @return [.Deprecated()].
NULL



#' Defunct Functions
#'
#' @name defunct
#' @keywords internal
#'
#' @inheritParams params
#'
#' @return [.Defunct()].
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
assertAllAreValidNames <- function(...) {
    .Deprecated("assertAreValidNames")
    assertAreValidNames(...)
}

#' @rdname defunct
#' @export
assertAreGeneAnnotations <- function(...) {
    .Defunct(msg = "Create `Gene2Symbol` class object.")
}

#' @rdname defunct
#' @export
assertAreTranscriptAnnotations <- function(...) {
    .Defunct(msg = "Create `Tx2Gene` class object.")
}

#' @rdname deprecated
#' @export
assertFormalGene2symbol <- function(...) {
    .Deprecated("assertFormalGene2Symbol")
    assertFormalGene2Symbol(...)
}

#' @rdname deprecated
#' @export
assertIsAHeaderLevel <- function(...) {
    .Deprecated("assertIsHeaderLevel")
    assertIsHeaderLevel(...)
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
    .Defunct(msg = "Create `Gene2Symbol` class object.")
}

#' @rdname defunct
#' @export
assertIsTx2gene <- function(...) {
    .Defunct(msg = "Create `Tx2Gene` class object.")
}



# nolint end
# nocov end
