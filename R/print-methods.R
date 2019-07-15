#' Print methods for objects with a cause attribute
#'
#' @name print
#' @inheritParams params
#'
#' @return Print command and return invisibly.
NULL



# Updated 2019-07-15.
.assertHasCause <- function(x) {
    cause <- cause(x)
    if (
        length(cause) != 1L &&
        !identical(length(x), length(cause))
    ) {
        stop("cause error.")
    }
    TRUE
}



# @seealso `assertive.base::strip_attributes().
# Updated 2019-07-15.
.stripAttributes <- function(x) {
    attributes(x) <- NULL
    x
}



# @seealso `assertive.base:::truncate()`.
# Updated 2019-07-15.
.truncate <- function(x, width = getOption("width")) {
    x <- as.character(x)
    ifelse(
        test = nchar(x) > width,
        yes = paste0(substring(x, 1L, width - 3L), "..."),
        no = x
    )
}



# Updated 2019-07-15.
.print.goalie.scalar <-  # nolint
    function(x) {
        .assertHasCause(x)
        print(x[[1L]])
        cat("Cause of failure:", cause(x), sep = "\n")
    }



# Consider letting the user access `n` and `ignoreNA` in a future update.
# For now keep the method support as simple as possible.
# Updated 2019-07-15.
.print.goalie.vector <-  # nolint
    function(x, n = 10L, ignoreNA = FALSE) {
        .assertHasCause(x)
        cause <- cause(x)
        names <- names(x)
        if (is.null(names)) {
            names <- character(length(x))
        }
        # Run this step after getting cause and names.
        x <- .stripAttributes(x)

        ok <- if (isTRUE(ignoreNA)) {
            # OK can be TRUE or NA; FALSE is bad.
            x | is.na(x)
        } else {
            # OK can be TRUE; FALSE or NA is bad.
            x & !is.na(x)
        }

        # Here we're creating a failure index.
        index <- head(which(!ok), n = n)
        n <- length(index)

        # Create the corresponding data frame, which we'll print below.
        failures <- data.frame(
            pos = index,
            value = .truncate(names[index]),
            # See assertive bug 15997.
            cause = unclass(cause[index]),
            row.names = seq_along(index)
        )

        # Slightly convoluted way of creating message to ensure that ngettext
        # creates all the translation strings.
        header <- if (nrow(failures) < n) {
            paste0(" ", gettextf("(showing the first %d)", nrow(failures)))
        } else {
            ""
        }

        cat(enc2utf8(sprintf(
            fmt = ngettext(
                n = n,
                msg1 = "There was %d failure%s:\n",
                msg2 = "There were %d failures%s:\n"
            ),
            n, header
        )))

        print(failures)
    }



#' @rdname print
#' @method print goalie
#' @export
# Updated 2019-07-15.
print.goalie <- function(x, ...) {
    if (!is.logical(x)) {
        stop("x is not logical.")
    }
    if (length(x) == 1L) {
        .print.goalie.scalar(x)
    } else {
        .print.goalie.vector(x)
    }
}
