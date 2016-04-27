<!-- Grab your social icons from https://github.com/carlsednaoui/gitsocial -->
[1.2]: http://i.imgur.com/wWzX9uB.png (me on Twitter)
[1]: http://www.twitter.com/franapoli
<!-- Grab your social icons from https://github.com/carlsednaoui/gitsocial -->

## Repo

Repo is a data-centered data flow manager. It allows to store R data
files in a central local repository, together with tags, annotations,
provenance and dependence information. Any saved object can then be
easily located and loaded through the repo interface.

Repo was developed by Francesco Napolitano [![alt text][1.2]][1]


## Development branches

+ [Master](https://github.com/franapoli/repo/tree/master): stable major
releases, usually in sync with CRAN version.

+ [Dev](https://github.com/franapoli/repo/tree/dev): stable minor 
releases.

+ [Untested](https://github.com/franapoli/repo/tree/untested):
unstable, in progress versions.

Latest news are found in the NEWS.md file of the "Untested" branch.

## Getting Started

An introductory vignette is available:

+ [Built from master
branch](https://rawgit.com/franapoli/repo/gh-pages/index.html).

+ [Built from dev
branch](https://rawgit.com/franapoli/repo/gh-pages-dev/index.html).


## Download and Installation

Repo is on CRAN and can be installed with:

    install.packages("repo")
    
to install the latest stable release.

However, CRAN versions are not updated very often. Latest stable
release can be downloaded from Github at:

    https://github.com/franapoli/repo

Repo can be installed from the downloaded sources as follows:

    install.packages("path-to-downloaded-source", repos=NULL)

devtools users can download and install at once the latest development
version from github as follows:

    install_github("franapoli/repo", ref="master")


