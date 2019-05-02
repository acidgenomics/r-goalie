# goalie

[![Travis CI build status](https://travis-ci.com/acidgenomics/goalie.svg?branch=master)](https://travis-ci.com/acidgenomics/goalie)
[![AppVeyor CI build status](https://ci.appveyor.com/api/projects/status/81he1lj6usgke7x2?svg=true)](https://ci.appveyor.com/project/mjsteinbaugh/goalie)
[![Anaconda cloud version](https://anaconda.org/bioconda/r-goalie/badges/version.svg)](https://anaconda.org/bioconda/r-goalie)
[![Repo status: active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

Assertive check functions for defensive R programming.

Refer to the [vignette](vignettes/goalie.Rmd) for an overview of the package.

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
