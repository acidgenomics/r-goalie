# goalie

[![Repo status: active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Travis CI build status](https://travis-ci.com/acidgenomics/goalie.svg?branch=master)](https://travis-ci.com/acidgenomics/goalie)
[![AppVeyor CI build status](https://ci.appveyor.com/api/projects/status/81he1lj6usgke7x2?svg=true)](https://ci.appveyor.com/project/mjsteinbaugh/goalie)
[![Install with Bioconda](https://img.shields.io/badge/install%20with-bioconda-brightgreen.svg?style=flat)](http://bioconda.github.io/recipes/r-goalie/README.html)

Assertive check functions for defensive R programming.

## Installation

### [R][] method

```r
install.packages(
    pkgs = "goalie",
    repos = c("r.acidgenomics.com", getOption("repos"))
)
```

### [conda][] method

Configure [conda][] to use the [bioconda][] channels.

```sh
# Don't install recipe into base environment.
name="r-goalie"
conda create --name="$name" "$name"
conda activate "$name"
R
```

[bioconda]: https://bioconda.github.io/
[conda]: https://conda.io/
[r]: https://www.r-project.org/
