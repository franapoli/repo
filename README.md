
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![](http://www.r-pkg.org/badges/version/repo)](https://cran.r-project.org/package=repo)
<sup><sub>Master: </sub></sup>[![Travis-CI Build
Status](https://travis-ci.org/franapoli/repo.svg?branch=master)](https://travis-ci.org/franapoli/repo)
<sup><sub>Dev: </sub></sup>[![Travis-CI Build
Status](https://travis-ci.org/franapoli/repo.svg?branch=dev)](https://travis-ci.org/franapoli/repo)

## Repo

Repo is a data-centered data flow manager. It allows to store R data
files in a central local repository, together with tags, annotations,
provenance and dependence information. Any saved object can then be
easily located and loaded through the repo interface.

A [paper about Repo](http://rdcu.be/pklt) has been published in BMC
Bioinformatics.

Latest news are found in the NEWS.md file of the “Untested” branch.

## Minimal example

Creating a dummy repository under the R temporary folder (skipping
confirmation):

``` r
library(repo)
rp <- repo_open(tempdir(), force=T)
#> Repo created.
```

Storing data. In this case, just item values and names are specified:

``` r
God <- Inf
rp$put(God)          ## item name inferred from variable name
rp$put(0, "user")    ## item name specified
```

More data with specified dependencies:

``` r
rp$put(pi, "The Pi costant", depends="God")
rp$put(1:10, "r", depends="user")
```

Loading items from the repository on the fly using names:

``` r
diam <- 2 * rp$get("r")
circum <- 2 * rp$get("The Pi costant") * rp$get("r")
area <- rp$get("The Pi costant") * rp$get("r") ^ 2
```

Storing more data with verbose descriptions:

``` r
rp$put(diam, "diameters", "These are the diameters", depends = "r")
rp$put(circum, "circumferences", "These are the circumferences",
       depends = c("The Pi costant", "r"))
rp$put(area, "areas", "These are the areas",
       depends = c("The Pi costant", "r"))
```

Showing repository contents:

``` r
print(rp)
#>              ID Dims  Size
#>             God    1  51 B
#>            user    1  49 B
#>  The Pi costant    1  55 B
#>               r   10  99 B
#>       diameters   10  75 B
#>  circumferences   10 103 B
#>           areas   10 103 B
```

``` r
rp$info()
#> Root:            /tmp/RtmppFMwnb 
#> Number of items: 7 
#> Total size:      535 B
```

``` r
rp$info("areas")
#> ID:           areas
#> Description:  These are the areas
#> Tags:         
#> Dimensions:   10
#> Timestamp:    2019-12-22 17:01:45
#> Size on disk: 103 B
#> Provenance:   
#> Attached to:  -
#> Stored in:    /tmp/RtmppFMwnb/a/areas
#> MD5 checksum: 56ad410055fedb0cae012d813a130291
#> URL:          -
```

Visualizing dependencies:

``` r
rp$dependencies()
```

![plot of chunk depgraph](inst/README-depgraph-1.png)

Manual acces to stored data:

``` r
fpath <- rp$attr("r", "path")
readRDS(fpath)
#>  [1]  1  2  3  4  5  6  7  8  9 10
```

## Development branches

  - [Master](https://github.com/franapoli/repo/tree/master): stable
    major releases, usually in sync with the latest CRAN version.

  - [Dev](https://github.com/franapoli/repo/tree/dev): minor releases
    passing automatic checks.

  - [Untested](https://github.com/franapoli/repo/tree/untested): in
    progress versions and prototype code, not necessarily working.

## Manuals

Besides inline help, two documents are available as introductory
material:

  - [A paper published on BMC Bioinformatics](http://rdcu.be/pklt)

  - [An introductory
    vignette](https://rawgit.com/franapoli/repo/gh-pages/index.html).

## Download and Installation

Repo is on CRAN and can be installed from within R as follows:

``` r
install.packages("repo")
```

Latest stable release can be downloaded from Github at
[https://github.com/franapoli/repo](https://www.github.com/franapoli/repo/).
Repo can then be installed from the downloaded sources as follows:

``` r
install.packages("path-to-downloaded-source", repos=NULL)
```

`devtools` users can install Repo directly from github as follows:

``` r
install_github("franapoli/repo", ref="dev")
```
