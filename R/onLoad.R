# Using backports to maintain R 3.4 support.
# Note that `...elt()` is used inside of `assert()`.
.onLoad <- function(libname, pkgname) {
    .major <- as.numeric(version[["major"]])
    .minor <- as.numeric(version[["minor"]])
    if (isTRUE(.major >= 3L) && isTRUE(.minor < 5L)) {
        backports::import(pkgname = pkgname, obj = "...elt")
    }
}
