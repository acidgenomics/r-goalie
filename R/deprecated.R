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
    .Defunct("assertHasValidNames or validNames")
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
    .Deprecated("assertHeaderLevel")
    assertHeaderLevel(...)
}

#' @rdname deprecated
#' @export
assertIsAStringOrNULL <- function(x) {
    .Deprecated("assertString")
    assertString(x = x, null.ok = TRUE)
}

#' @rdname defunct
#' @export
assertIsCharacterOrNULL <- function(...) {
    .Defunct("checkMultiClass")
}

#' @rdname defunct
#' @export
assertIsDataFrameOrNULL <- function(...) {
    .Defunct("checkMultiClass")
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
    .Deprecated("testHasUniqueCols")
    testHasUniqueCols(...)
}



# v0.2.0 =======================================================================
# areDirs : checkDirectoryExists
# areFiles : checkFileExists
# areNonExisting
# areSamplesUnique
# areURLs
# areUniqueGeneNames
# assertAllAreNonExisting
# assertAllAreValidNames
# assertAreDirs
# assertAreFiles
# assertAreGeneAnnotations
# assertAreNonExisting
# assertAreTranscriptAnnotations
# assertAreURLs
# assertAreUniqueGeneNames
# assertAreValidNames
# assertFormalCompress
# assertHasRownames
# assertHasUniqueCols
# assertHasValidDimnames
# assertHasValidNames
# assertIsAHeaderLevel
# assertIsANumberOrNULL
# assertIsAStringOrNULL
# assertIsAlpha
# assertIsAnImplicitInteger
# assertIsAnImplicitIntegerOrNULL
# assertIsAnIntegerOrNULL
# assertIsAnnotable
# assertIsCharacterOrNULL
# assertIsColorScaleContinuousOrNULL
# assertIsColorScaleDiscreteOrNULL
# assertIsDataFrameOrNULL
# assertIsDir
# assertIsFile
# assertIsFillScaleContinuousOrNULL
# assertIsFillScaleDiscreteOrNULL
# assertIsGFF
# assertIsGene2symbol
# assertIsHeaderLevel
# assertIsHexColorFunctionOrNULL
# assertIsImplicitInteger
# assertIsImplicitIntegerOrNULL
# assertIsStringOrNULL
# assertIsTx2gene
# assertIsURL
# hasRownames
# hasUniqueCols
# isAlpha
# isAnImplicitInteger
# isDir
# isFile
# isHeaderLevel
# isImplicitInteger
# isURL
# validDimnames
# validNames



# nolint end
# nocov end
