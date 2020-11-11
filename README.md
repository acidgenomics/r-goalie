# goalie

[![Install with Bioconda](https://img.shields.io/badge/install%20with-bioconda-brightgreen.svg?style=flat)](http://bioconda.github.io/recipes/r-goalie/README.html)

Assertive check functions for defensive R programming.

## Installation

### [R][] method

```r
install.packages(
    pkgs = "goalie",
    repos = c(
        "https://r.acidgenomics.com",
        getOption("repos")
    )
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
