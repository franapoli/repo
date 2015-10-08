<!-- Grab your social icons from https://github.com/carlsednaoui/gitsocial -->
[1.2]: http://i.imgur.com/wWzX9uB.png (me on Twitter)
[1]: http://www.twitter.com/franapoli
<!-- Grab your social icons from https://github.com/carlsednaoui/gitsocial -->

# News

## Development version

List updated on 10/08/2015

+ Atomization of item replacement to avoid the possibility of data
loss.

+ Added search field in print to match anything in items and the
corresponding "find" shortcut method.

+ When using multiple tags now by default they match when at least one
matches (OR).

+ Now tags can be matched using OR, AND, NOT or any external logical
function.

+ Fixed a bug in print when showing one item.

## v1.0

+ Aug 3 2015
    Now on CRAN

+ Jul 30 2015
    1.0 released.


# repo

Repo allows to store R data files in a central local repository, together
with tags, annotations, provenance and dependence information. Any saved
object can then be easily located and loaded through the repo interface.

Repo was developed by Francesco Napolitano [![alt text][1.2]][1]

## Tour

Please check out the getting started guide at http://franapoli.github.io/repo.


## Download and Installation

### Latest release on CRAN

Repo is now on CRAN, just use:

    install.packages("repo")
    
to install the latest release.

### Latest development on GitHub

Latest development version is available on Github at:

    https://github.com/franapoli/repo

Repo can be installed from the downloaded sources as follows:

    install.packages("path-to-downloaded-source", type="source", repos=NULL)

devtools users can download and install at once the latest development
version from github as follows:

    install_github("franapoli/repo", ref="dev")

