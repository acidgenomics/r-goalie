# goalie

[![Travis CI build status](https://travis-ci.com/acidgenomics/goalie.svg?branch=master)](https://travis-ci.com/acidgenomics/goalie)
[![AppVeyor CI build status](https://ci.appveyor.com/api/projects/status/81he1lj6usgke7x2?svg=true)](https://ci.appveyor.com/project/mjsteinbaugh/goalie)
[![Anaconda cloud version](https://anaconda.org/bioconda/r-goalie/badges/version.svg)](https://anaconda.org/bioconda/r-goalie)
[![Repo status: active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

Assertive check functions for defensive R programming.

## Installation

### [Bioconductor][] method

We recommend installing the package with [BiocManager][].

```r
if (!require("BiocManager")) {
    install.packages("BiocManager")
}
BiocManager::install("remotes")
BiocManager::install("acidgenomics/goalie")
```

### [conda][] method

Configure [conda][] to use the [bioconda][] channels.

```bash
conda install -c bioconda r-goalie
```

## Overview

[goalie][] is an attempt to incorporate elements of multiple assertive check packages into a single package with as few dependencies as possible. All assertive checks are written in the most basic [R][] code possible, without reliance on compilation (e.g. C++/Rcpp). It is still a work in progress, and feature requests are welcome.

R exports the `stopifnot()` function as a simple assert engine that supports multiple boolean returns (`TRUE`/`FALSE`) in a single call, separated by commas. This is a nice, flexible system for defensive programming. However, `stopifnot()` does not currently support custom error messages, and can be confusing to debug.

Similar to the approach used in the [assertthat][] package, goalie exports the `assert()` function, which is designed to be a drop-in replacement for `stopifnot()` with improved error message support. `validate()` is a variation on `assert()` that is intended to be used within S4 class validity checks, which is useful for [Bioconductor][] packages.

Additionally, goalie exports a number of assertive check functions that are intended to harden bioinformatics-oriented functions in R. These check functions return `logical` with an additional `"goalie"` S3 class definition, which enables us to define custom print methods that display additional useful information about the cause of the error (see `cause()` and `print()` for details). This approach draws heavily from the [assertive][] package.

The goalie source code incorporates elements from these excellent assert check packages:

- [assertive][] by Richie Cotton.
- [assertthat][] by Hadley Wickham.
- [checkmate][] by Michael Lang.

[assertive]: https://cran.r-project.org/package=assertive
[assertthat]: https://cran.r-project.org/package=assertthat
[BiocManager]: https://cran.r-project.org/package=BiocManager
[bioconda]: https://bioconda.github.io/
[Bioconductor]: https://bioconductor.org/
[checkmate]: https://cran.r-project.org/package=checkmate
[conda]: https://conda.io/
[goalie]: https://goalie.acidgenomics.com/
[R]: https://www.r-project.org/
