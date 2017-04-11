[![](http://cranlogs.r-pkg.org/badges/repo)](http://cran.rstudio.com/web/packages/repo/index.html)
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

    library(repo)
    rp <- repo_open()

Putting some stuff (it is saved on permanent storage). In this case,
just values and names are specified:

    rp$put(Inf, "God")
    rp$put(0, "user")

Putting specifying dependencies:

    rp$put(pi, "The Pi costant", depends="God")
    rp$put(1:10, "r", depends="user")

Getting stuff from the repository on the fly:

    diam <- 2 * rp$get("r")
    circum <- 2 * rp$get("The Pi costant") * rp$get("r")
    area <- rp$get("The Pi costant") * rp$get("r") ^ 2

Putting with verbose descriptions:

    rp$put(diam, "diameters", "These are the diameters", depends = "r")
    rp$put(circum, "circumferences", "These are the circumferences",
           depends = c("The Pi costant", "r"))
    rp$put(area, "areas", "This are the areas",
           depends = c("The Pi costant", "r"))

Repository contents:

    print(rp)

                 ID Dims Size
                God    1 44 B
               user    1 41 B
     The Pi costant    1 47 B
                  r   10 60 B
          diameters   10 67 B
     circumferences   10 96 B
              areas   10 95 B

    rp$info()

    Root:            /tmp/RtmpjOSKE5 
    Number of items: 7 
    Total size:      450 B 

    rp$info("areas")

    ID:           areas
    Description:  This are the areas
    Tags:         
    Dimensions:   10
    Timestamp:    2017-04-11 18:13:40
    Size on disk: 95 B
    Provenance:   
    Attached to:  -
    Stored in:    xw/tt/zm/xwttzmigamg57309nvo3nd6cx8r59jss
    MD5 checksum: 8b24858f4e92014f5fb29f76c3505588
    URL:          -

Visualizing dependencies:

    rp$dependencies()

![](readme_example_files/figure-markdown_strict/unnamed-chunk-9-1.png)


## Development branches

+ [Master](https://github.com/franapoli/repo/tree/master): stable major
releases, usually in sync with lastest CRAN version.

+ [Dev](https://github.com/franapoli/repo/tree/dev): fairly stable
minor releases.

+ [Untested](https://github.com/franapoli/repo/tree/untested):
unstable, in progress versions. Latest news appear in the "NEWS.md"
file of this branch.


## Guides

An introductory vignette is available:

+ [Built from master
branch](https://rawgit.com/franapoli/repo/gh-pages/index.html).

+ [Built from dev
branch](https://rawgit.com/franapoli/repo/gh-pages-dev/index.html).


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


