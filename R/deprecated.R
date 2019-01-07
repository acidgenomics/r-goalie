# nocov start
# nolint start



#' Deprecated functions
#' @name deprecated
#' @keywords internal
#' @inheritParams params
#' @return `.Deprecated`.
NULL



#' Defunct functions
#' @name defunct
#' @keywords internal
#' @inheritParams params
#' @return `.Defunct`.
NULL



# v0.2.0 =======================================================================
# areDirs/checkDirectoryExists
# areFiles/checkFileExists
# areSamplesUnique
# areURLs
# areUniqueGeneNames
# isAnImplicitInteger (isInt)
# isImplicitInteger (isIntegerish)
# validDimnames (hasValidDimnames)



# v0.2.1 =======================================================================
#' @rdname deprecated
#' @export
containsAlpha <- appendToBody(
    fun = isAlpha,
    values = quote(.Deprecated("isAlpha"))
)

#' @rdname deprecated
#' @export
containsHeaderLevel <- appendToBody(
    fun = isHeaderLevel,
    values = quote(.Deprecated("isHeaderLevel"))
)

#' @rdname deprecated
#' @export
containsHexColors <- appendToBody(
    fun = allAreHexColors,
    values = quote(.Deprecated("allAreHexColors"))
)

#' @rdname deprecated
#' @export
containsURL <- isURL

#' @rdname deprecated
#' @export
containsAURL <- isAURL





# nolint end
# nocov end
