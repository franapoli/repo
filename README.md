<!-- Grab your social icons from https://github.com/carlsednaoui/gitsocial -->
[1.2]: http://i.imgur.com/wWzX9uB.png (me on Twitter)
[1]: http://www.twitter.com/franapoli
<!-- Grab your social icons from https://github.com/carlsednaoui/gitsocial -->

# Release notes:

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

Repo is not on R repositories at the moment, but is available
on Github at:

    https://github.com/franapoli/repo

Repo can be installed from the downloaded sources as follows:

    install.packages("path-to-downloaded-source", type="source", repos=NULL)

devtools users can download and install at once the latest stable
version from github as follows:

    install_github("franapoli/repo", ref="master")

or the latest development version at:

    install_github("franapoli/repo", ref="dev")
