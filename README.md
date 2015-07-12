# repo
*Thou Shalt Not save() or load()*

Store your R data files in a central repository on your disk, together
with tags, annotations, provenance and dependence information. Find
and load any saved objects easily through the repo interface, without
remembering its position on the disk.

## Tour

Please check out the getting started guide at http://franapoli.github.io/repo.


## Installation

### Download

Repo is not on R repositories at the moment. Git users can get it
with:

git clone https://github.com/franapoli/repo

Otherwise visit the same site with your browser.


### Install

You can install repo from sources with:

    install.packages("path-to-downloaded-source", type="source", repos=NULL)

devtools users can install and download directly from github with:

    install_github("franapoli/repo", ref="dev")

