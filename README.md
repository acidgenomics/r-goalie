# goalie

[![Install with Bioconda](https://img.shields.io/badge/install%20with-bioconda-brightgreen.svg?style=flat)](http://bioconda.github.io/recipes/r-goalie/README.html)

Assertive check functions for defensive R programming.

## Introduction

goalie is an attempt to incorporate elements of multiple assertive check
packages into a single package with as few dependencies as possible. All
assertive checks are written in the most basic [R][] code possible, without
reliance on compilation (e.g. C++/Rcpp). It is still a work in progress, and
feature requests are welcome.

R exports the `stopifnot()` function as a simple assert engine that supports
multiple boolean returns (`TRUE`/`FALSE`) in a single call, separated by commas.
This is a nice, flexible system for defensive programming. However,
`stopifnot()` does not currently support custom error messages, and can be
confusing to debug.

Similar to the approach used in the [assertthat][] package, goalie exports the
`assert()` function, which is designed to be a drop-in replacement for
`stopifnot()` with improved error message support. `validate()` is a variation
on `assert()` that is intended to be used within S4 class validity checks, which
is useful for [Bioconductor][] packages.

Additionally, goalie exports a number of assertive check functions that are
intended to harden bioinformatics-oriented functions in R. These check functions
return `logical` with an additional `"goalie"` S4 class definition, which
enables us to keep track of additional useful information about the cause of the
error (see `cause()` for details). This approach draws heavily from the
[assertive][] package.

## Installation

This is an [R][] package.

```r
install.packages(
    pkgs = "goalie",
    repos = c(
        "https://r.acidgenomics.com",
        getOption("repos")
    ),
    dependencies = NA
)
```

### [conda][] method

Configure [conda][] to use the [bioconda][] channels.

```sh
# Don't install recipe into base environment.
name='r-goalie'
conda create --name="$name" "$name"
conda activate "$name"
R
```

### [Docker][] method

```sh
image='acidgenomics/r-packages:goalie'
workdir='/mnt/work'
docker pull "$image"
docker run -it \
    --volume="${PWD}:${workdir}" \
    --workdir="$workdir" \
    "$image" \
    R
```

## See also

The goalie source code incorporates elements from these excellent assert check
packages:

- [assertive][] by Richie Cotton.
- [assertthat][] by Hadley Wickham.
- [checkmate][] by Michael Lang.

[assertive]: https://cran.r-project.org/package=assertive
[assertthat]: https://cran.r-project.org/package=assertthat
[bioconda]: https://bioconda.github.io/
[bioconductor]: https://bioconductor.org/
[checkmate]: https://cran.r-project.org/package=checkmate
[conda]: https://conda.io/
[docker]: https://www.docker.com/
[r]: https://www.r-project.org/
