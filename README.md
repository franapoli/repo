<!-- Grab your social icons from https://github.com/carlsednaoui/gitsocial -->
[1.2]: http://i.imgur.com/wWzX9uB.png (me on Twitter)
[1]: http://www.twitter.com/franapoli
<!-- Grab your social icons from https://github.com/carlsednaoui/gitsocial -->

# Dev branch

This is the Repo development branch. It is merged from the untested
branch as soon as all example code runs without errors. Stable
releases are instead moved to the master branch.


# News
(Detailed log in the NEWS file)

New stable release 1.10.1.0 includes:
+ `find` method to search any item field
+ "maybe you were looking for" feature in `get`
+ `lazydo` (execute expression with cache)
+ `pull` (download item content from URL)
+ multiple tags can be matched with OR, AND, NOT or custom functions
+ safer item replacement
+ relative paths (easier to move a repo or use remote repos)
+ preliminary Shiny interface
+ other minor improvements and bug fixes


# Repo

Repo is a data-centered data flow manager. It allows to store R data
files in a central local repository, together with tags, annotations,
provenance and dependence information. Any saved object can then be
easily located and loaded through the repo interface.

Repo was developed by Francesco Napolitano [![alt text][1.2]][1]


## Getting Started

Please check out the getting started guide
[here](https://rawgit.com/franapoli/repo/gh-pages-dev/index.html).


## Download and Installation

### Latest release on CRAN

Repo is now on CRAN, just use:

    install.packages("repo")
    
to install the latest stable release.

### Latest development on GitHub

Latest development version is available on Github at:

    https://github.com/franapoli/repo

Repo can be installed from the downloaded sources as follows:

    install.packages("path-to-downloaded-source", type="source", repos=NULL)

devtools users can download and install at once the latest development
version from github as follows:

    install_github("franapoli/repo", ref="dev")


