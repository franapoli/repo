<!-- Grab your social icons from https://github.com/carlsednaoui/gitsocial -->
[1.2]: http://i.imgur.com/wWzX9uB.png (me on Twitter)
[1]: http://www.twitter.com/franapoli
<!-- Grab your social icons from https://github.com/carlsednaoui/gitsocial -->

# Dev branch disclaimer

This is the Repo development branch. It could be unstable and cause
problems, you may want to check out the master branch for a safer
version. You may really want to backup *at least* your repo index file
before updating (you can find it with `repo$root()`).

# News

10/21/2015

+ Fixed regression in `check` after moving to relative paths
+ Added check points for reserved tags and warnings for existing tags
+ Improved print reaction when all matching results are hidden

10/19/2015

+ Added Bulk-edit feature.

10/17/2015

+ Added "Maybe-you-were-looking-for" feature to `get`.
+ Fixed bug with managing new relative paths.

10/12/2015

+ Absolute to relative paths for stored objects. This allows to easily
rebase a repository, for example to a remote machine, without affecting
the index.

+ Automatic update of old repository entries upon resource loading.

10/08/2015

+ Atomization of item replacement to avoid the possibility of data
loss.

Earlier...

+ Added search field in print to match anything in items and the
corresponding "find" shortcut method.

+ When using multiple tags now by default they match when at least one
matches (OR).

+ Now tags can be matched using OR, AND, NOT or any external logical
function.

+ Fixed a bug in print when showing one item.


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


# Version history
## v1.0

+ Aug 3 2015
    Now on CRAN

+ Jul 30 2015
    1.0 released.
