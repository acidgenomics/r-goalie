# goalie

[![Travis CI](https://travis-ci.com/steinbaugh/goalie.svg?branch=master)](https://travis-ci.com/steinbaugh/goalie)
[![AppVeyor CI](https://ci.appveyor.com/api/projects/status/81he1lj6usgke7x2?svg=true)](https://ci.appveyor.com/project/mjsteinbaugh/goalie)
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
BiocManager::install("steinbaugh/goalie")
```

[BiocManager]: https://cran.r-project.org/package=BiocManager
[Bioconductor]: https://bioconductor.org
[R]: https://www.r-project.org
