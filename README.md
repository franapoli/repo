---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



[![](http://cranlogs.r-pkg.org/badges/repo)](https://cran.r-project.org/package=repo)
<sup><sub>Master branch:</sub></sup> [![Travis-CI Build Status](https://travis-ci.org/franapoli/repo.svg?branch=master)](https://travis-ci.org/franapoli/repo)
<sup><sub>Dev branch:</sub></sup> [![Travis-CI Build Status](https://travis-ci.org/franapoli/repo.svg?branch=dev)](https://travis-ci.org/franapoli/repo)
<sup><sub>Untested branch:</sub></sup> [![Travis-CI Build Status](https://travis-ci.org/franapoli/repo.svg?branch=untested)](https://travis-ci.org/franapoli/repo)
<!-- Grab your social icons from https://github.com/carlsednaoui/gitsocial -->
[1.2]: http://i.imgur.com/wWzX9uB.png (me on Twitter)
[1]: http://www.twitter.com/franapoli
<!-- Grab your social icons from https://github.com/carlsednaoui/gitsocial -->

## Repo

Repo is a data-centered data flow manager. It allows to store R data
files in a central local repository, together with tags, annotations,
provenance and dependence information. Any saved object can then be
easily located and loaded through the repo interface.

A [paper about Repo](http://rdcu.be/pklt) has been published in BMC
Bioinformatics.

Latest news are found in the NEWS.md file of the "Untested" branch.

Repo is developed by Francesco Napolitano [![alt text][1.2]][1]


## Minimal example

Repository creation in the default folder:


```r
    library(repo)
    rp <- repo_open()
```




Putting some stuff (it is saved on permanent storage). In this case,
just values and names are specified:


```r
    rp$put(Inf, "God")
    rp$put(0, "user")
```

Putting specifying dependencies:


```r
    rp$put(pi, "The Pi costant", depends="God")
    rp$put(1:10, "r", depends="user")
```

Getting stuff from the repository on the fly:


```r
    diam <- 2 * rp$get("r")
    circum <- 2 * rp$get("The Pi costant") * rp$get("r")
    area <- rp$get("The Pi costant") * rp$get("r") ^ 2
```

Putting with verbose descriptions:


```r
    rp$put(diam, "diameters", "These are the diameters", depends = "r")
    rp$put(circum, "circumferences", "These are the circumferences",
           depends = c("The Pi costant", "r"))
    rp$put(area, "areas", "This are the areas",
           depends = c("The Pi costant", "r"))
```

Repository contents:


```r
    print(rp)
#>              ID Dims Size
#>             God    1 42 B
#>            user    1 40 B
#>  The Pi costant    1 45 B
#>               r   10 60 B
#>       diameters   10 65 B
#>  circumferences   10 94 B
#>           areas   10 93 B
```

```r
    rp$info()
#> Root:            /tmp/RtmpIjPq16/kZJJAjPdwgCB 
#> Number of items: 7 
#> Total size:      439 B
```

```r
    rp$info("areas")
#> ID:           areas
#> Description:  This are the areas
#> Tags:         
#> Dimensions:   10
#> Timestamp:    2017-08-04 15:40:03
#> Size on disk: 93 B
#> Provenance:   
#> Attached to:  -
#> Stored in:    sk/nr/zy/sknrzyen718nms80t89timt6fyrc2zvx
#> MD5 checksum: 65b946a5ffd6d1a63572e1ccfe3a9e08
#> URL:          -
```

Visualizing dependencies:


```r
    rp$dependencies()
```

![plot of chunk depgraph](inst/README-depgraph-1.png)


## Development branches

+ [Master](https://github.com/franapoli/repo/tree/master): stable major
releases, usually in sync with lastest CRAN version.

+ [Dev](https://github.com/franapoli/repo/tree/dev): fairly stable
minor releases.

+ [Untested](https://github.com/franapoli/repo/tree/untested):
unstable, in progress versions. Latest news appear in the "NEWS.md"
file of this branch.


## Manuals

Besides inline help, two documents are available as introductory
material:

+ [A paper published on BMC Bioinformatics](http://rdcu.be/pklt)

+ [An introductory
vignette](https://rawgit.com/franapoli/repo/gh-pages/index.html).


## Download and Installation

Repo is on CRAN and can be installed from within R as follows:

    > install.packages("repo")
    
However, CRAN versions are not updated very often. Latest stable
release can be downloaded from Github at
[https://github.com/franapoli/repo](https://www.github.com/franapoli/repo/).
Repo can then be installed from the downloaded sources as follows:

    > install.packages("path-to-downloaded-source", repos=NULL)

`devtools` users can download and install at once the latest development
version from github as follows:

    > install_github("franapoli/repo", ref="dev")


