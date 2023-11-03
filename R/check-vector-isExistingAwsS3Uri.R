#' Does the input contain an existing (active) AWS S3 URI?
#'
#' @name check-vector-isExistingAwsS3Uri
#' @note Updated 2023-11-03.
#'
#' @details
#' Requires the AWS CLI to be installed.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @param profile `character(1)`.
#' AWS profile name.
#'
#' @seealso
#' - `aws help`.
#' - https://www.learnaws.org/2023/01/30/aws-s3-cli-check-file/
#' - koopa function `koopa_is_existing_aws_s3_uri`.
#'
#' @examples
#' ## TRUE ====
#' ## > isAnExistingAwsS3Uri(
#' ## >     x = "s3://koopa.acidgenomics.com/install",
#' ## >     profile = "acidgenomics"
#' ## > )
#'
#' ## FALSE ====
#' ## > isAnExistingAwsS3Uri(
#' ## >     x = "s3://koopa.acidgenomics.com/foo",
#' ## >     profile = "acidgenomics"
#' ## > )
NULL



## Vector ======================================================================

#' @describeIn check-vector-isExistingAwsS3Uri Vectorized.
#' @export
isExistingAwsS3Uri <- function(x, profile = "default") {
    assert(isString(profile))
    ok <- isAwsS3Uri(x)
    if (!all(ok)) {
        return(ok)
    }
    ok <- isSystemCommand("git")
    if (!isTRUE(ok)) {
        ko <- rep(x = FALSE, times = length(x))
        return(setCause(ko, false = "no aws cli"))
    }
    requireNamespaces("AcidBase")
    ok <- bapply(
        X = x,
        profile = profile,
        FUN = function(x, profile) {
            bucket <- sub(
                pattern = "^s3://([^/]+)/(.+)$",
                replacement = "\\1",
                x = x
            )
            key <- sub(
                pattern = "^s3://([^/]+)/(.+)$",
                replacement = "\\2",
                x = x
            )
            ok <- tryCatch(
                expr = {
                    invisible(AcidBase::shell(
                        command = "aws",
                        args = c(
                            "--profile", profile,
                            "s3api", "head-object",
                            "--bucket", bucket,
                            "--key", key,
                            "--no-cli-pager"
                        ),
                        print = FALSE,
                        returnStdout = TRUE
                    ))
                    TRUE
                },
                warning = function(w) {
                    FALSE
                },
                error = function(e) {
                    FALSE
                }
            )
            ok
        },
        USE.NAMES = FALSE
    )
    setCause(ok, false = "AWS S3 URI doesn't exist")
}



## Scalar ======================================================================

#' @describeIn check-vector-isExistingAwsS3Uri Scalar. Requires a single URI.
#' @export
isAnExistingAwsS3Uri <- function(x, profile = "default") {
    ok <- isScalar(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- isExistingAwsS3Uri(x = x, profile = profile)
    if (!all(ok)) {
        return(falseFromVector(ok))
    }
    TRUE
}



#' @describeIn check-vector-isExistingAwsS3Uri Scalar. Checks that all strings
#' are existing URIs.
#' @export
allAreExistingAwsS3Uris <- function(x, profile = "default") {
    ok <- isExistingAwsS3Uri(x = x, profile = profile)
    if (!all(ok)) {
        return(falseFromVector(ok))
    }
    TRUE
}
