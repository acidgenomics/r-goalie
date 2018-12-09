# assert =======================================================================
#' @importFrom checkmate assert
#' @export
checkmate::assert
# NOTE Using a stricter default for `combine` argument here.
formals(assert)[["combine"]] <- "and"



# checkAtomic ==================================================================
#' @importFrom checkmate assertAtomic
#' @export
checkmate::assertAtomic

#' @importFrom checkmate checkAtomic
#' @export
checkmate::checkAtomic

#' @importFrom checkmate testAtomic
#' @export
checkmate::testAtomic



# checkCharacter ===============================================================
#' @importFrom checkmate assertCharacter
#' @export
checkmate::assertCharacter

#' @importFrom checkmate checkCharacter
#' @export
checkmate::checkCharacter

#' @importFrom checkmate testCharacter
#' @export
checkmate::testCharacter



# checkClass ===================================================================
#' @importFrom checkmate assertClass
#' @export
checkmate::assertClass

#' @importFrom checkmate checkClass
#' @export
checkmate::checkClass

#' @importFrom checkmate testClass
#' @export
checkmate::testClass



# checkDirectoryExists =========================================================
#' @importFrom checkmate assertDirectoryExists
#' @export
checkmate::assertDirectoryExists

#' @importFrom checkmate checkDirectoryExists
#' @export
checkmate::checkDirectoryExists

#' @importFrom checkmate testDirectoryExists
#' @export
checkmate::testDirectoryExists



# checkFactor ==================================================================
#' @importFrom checkmate assertFactor
#' @export
checkmate::assertFactor

#' @importFrom checkmate checkFactor
#' @export
checkmate::checkFactor

#' @importFrom checkmate testFactor
#' @export
checkmate::testFactor



# checkFileExists ==============================================================
#' @importFrom checkmate assertFileExists
#' @export
checkmate::assertFileExists

#' @importFrom checkmate checkFileExists
#' @export
checkmate::checkFileExists

#' @importFrom checkmate testFileExists
#' @export
checkmate::testFileExists



# checkFlag ====================================================================
#' @importFrom checkmate assertFlag
#' @export
checkmate::assertFlag

#' @importFrom checkmate checkFlag
#' @export
checkmate::checkFlag

#' @importFrom checkmate testFlag
#' @export
checkmate::testFlag



# checkLogical =================================================================
#' @importFrom checkmate assertLogical
#' @export
checkmate::assertLogical

#' @importFrom checkmate checkLogical
#' @export
checkmate::checkLogical

#' @importFrom checkmate testLogical
#' @export
checkmate::testLogical



# checkNames ===================================================================
#' @importFrom checkmate assertNames
#' @export
checkmate::assertNames

#' @importFrom checkmate checkNames
#' @export
checkmate::checkNames

#' @importFrom checkmate testNames
#' @export
checkmate::testNames



# checkScalar ==================================================================
#' @importFrom checkmate assertScalar
#' @export
checkmate::assertScalar

#' @importFrom checkmate checkScalar
#' @export
checkmate::checkScalar

#' @importFrom checkmate testScalar
#' @export
checkmate::testScalar



# checkString ==================================================================
#' @importFrom checkmate assertString
#' @export
checkmate::assertString

#' @importFrom checkmate checkString
#' @export
checkmate::checkString

#' @importFrom checkmate testString
#' @export
checkmate::testString
