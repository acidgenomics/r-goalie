# goalie

[![Repo status: active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Travis CI build status](https://travis-ci.com/acidgenomics/goalie.svg?branch=master)](https://travis-ci.com/acidgenomics/goalie)
[![AppVeyor CI build status](https://ci.appveyor.com/api/projects/status/81he1lj6usgke7x2?svg=true)](https://ci.appveyor.com/project/mjsteinbaugh/goalie)
[![Install with Bioconda](https://img.shields.io/badge/install%20with-bioconda-brightgreen.svg?style=flat)](http://bioconda.github.io/recipes/r-goalie/README.html)

Assertive check functions for defensive R programming.

## Installation

### [R][] method

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
}
Sys.setenv(R_REMOTES_UPGRADE = "always")
## Set `GITHUB_PAT` in `~/.Renviron` if you get a rate limit error.
remotes::install_github("acidgenomics/goalie")
```

### [conda][] method

Configure [conda][] to use the [bioconda][] channels.

```sh
conda install -c bioconda r-goalie
```

[bioconda]: https://bioconda.github.io/
[conda]: https://conda.io/
[r]: https://www.r-project.org/
