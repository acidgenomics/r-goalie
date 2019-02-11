# Import backports necessary to maintain R 3.4 support.

# `...elt()` is used inside of `assert()`.
# help(topic = "...elt", package = "backports")

.major <- as.numeric(version[["major"]])
.minor <- as.numeric(version[["minor"]])

if (isTRUE(.major >= 3L) && isTRUE(.minor < 5L)) {
    .onLoad <- function(libname, pkgname) {
        backports::import(pkgname = pkgname, obj = "...elt")
    }
}

rm(.major, .minor)
