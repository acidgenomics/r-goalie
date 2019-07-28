# goalie

[![Repo status: active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Travis CI build status](https://travis-ci.com/acidgenomics/goalie.svg?branch=master)](https://travis-ci.com/acidgenomics/goalie)
[![AppVeyor CI build status](https://ci.appveyor.com/api/projects/status/81he1lj6usgke7x2?svg=true)](https://ci.appveyor.com/project/mjsteinbaugh/goalie)
[![Anaconda version](https://anaconda.org/bioconda/r-goalie/badges/version.svg) ![Anaconda latest release date](https://anaconda.org/bioconda/r-goalie/badges/latest_release_date.svg) ![Anaconda downloads](https://anaconda.org/bioconda/r-goalie/badges/downloads.svg)](https://anaconda.org/bioconda/r-goalie)

Assertive check functions for defensive R programming.

## Installation

This is an [R][] package.

### [R][] method

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
}
Sys.setenv(R_REMOTES_UPGRADE = "always")
## Set `GITHUB_PAT` in `~/.Renviron` if you get a rate limit error.
remotes::install_github("acidgenomics/goalie")
remotes::update_packages()
```

### [conda][] method

Configure [conda][] to use the [bioconda][] channels.

```bash
conda install -c bioconda r-goalie
```

[BiocManager]: https://cran.r-project.org/package=BiocManager
[bioconda]: https://bioconda.github.io/
[Bioconductor]: https://bioconductor.org/
[conda]: https://conda.io/
[R]: https://www.r-project.org/
