# @seealso assertive.base:::type_description
typeDescription <- function(x) {
    if (is.array(x)) {
        sprintf(sprintf("class '%s %s'", class(x[FALSE]), toString(class(x))))
    }
    else if (is.function(x)) {
        sprintf(sprintf("class '%s %s'", typeof(x), toString(class(x))))
    }
    else if (isS4(x)) {
        sprintf(sprintf("S4 class '%s'", toString(class(x))))
    }
    else {
        sprintf("class '%s'", toString(class(x)))
    }
}
