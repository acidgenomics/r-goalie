# goalie

[![Travis CI](https://travis-ci.org/steinbaugh/goalie.svg?branch=master)](https://travis-ci.org/steinbaugh/goalie)
[![AppVeyor CI](https://ci.appveyor.com/api/projects/status/007vq15089ukn6ej/branch/master?svg=true)](https://ci.appveyor.com/project/mjsteinbaugh/goalie/branch/master)
[![Codecov](https://codecov.io/gh/steinbaugh/goalie/branch/master/graph/badge.svg)](https://codecov.io/gh/steinbaugh/goalie)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

Assertive check functions for defensive R programming.

## Installation

### [Bioconductor][] method

We recommend installing the package with [BiocManager][].

```r
if (!require("BiocManager")) {
    install.packages("BiocManager")
}
BiocManager::install("remotes")
BiocManager::install("steinbaugh/goalie", ref = "v0.1.0")
```

[BiocManager]: https://cran.r-project.org/package=BiocManager
[Bioconductor]: https://bioconductor.org
[R]: https://www.r-project.org
