
# News

Dev 18, 2019, v2.1.4

+ Completely renewd naming convention for the stored files. This
  change is transparent to the user and will not affect previous
  code. Also existing repositories can be used with this new version
  (previous files will be left untouched, new ones will follow the new
  convention). But now the directory tree of the repository has been
  greatly simplified and is human readable. Each item is stored 1
  level (as opposed to 3) under the repo root and using a sanitized
  version of the item name (as opposed to a random string). Possible
  name collisions are avoided by appending a progressive number. Item
  name is still stored as is in the repository (and will still collide
  with previously stored items by the same name as expected).
  
+ fix in the vignette to pass CRAN checks on some platforms

Dec 3, 2018, v2.1.3.1

+ `repo_get` now allows to suppress name suggestions for faster search
  in large repositories.

May 4, 2018, v2.1.3

+ Significant speedup of all operations involving a search through the
items, including `find` and `print`. These are now usable with large
repositories (tested with tens of thousands items).

Dic 6, 2017, v2.1.2.2

+ Changed error and warning calls with `call.=F`

Nov 27, 2017, v2.1.2.1

+ `stash` is not deprecated anymore, as it is useful to lazydo

+ fixed some `stash` behaviour, like when `put`-ing an item with the
same name of `stash`-ed item, which now does not throw an error.

+ `lazydo` with `force=T` will not throw an error for existing item,
which is now a `stash`-ed item

+ `laxydo` now does not take an object of type `expression`, but an
expression directly.


11/18/17, v2.1.2

+ Fixed broken link to remote sample

+ Changed the outputs of `repo_check`, which can now be
`suppressedMessages`-ed.

08/03/17, v2.1.1 - Major release submitted to CRAN

08/01/17, v2.1.1 - CRAN candidate release

+ Fixed bug in `repo_build`

+ Deprecated `stash`: now that `put` parameters are mostly optional,
it is not necessary anymore.


07/25/17, v2.1.0.9001

+ Correcting vignette for next release

+ fixed version numbering (there is no v2.2)

04/15/17, v2.1.0.9000

+ Also the item name in `put` is now optional. If not provided, the
name of the `obj` variable will be used.

+ The chunk format has been simplified, the close tag is now just `}`,
as follows:
```
	## chunk "ChunkName" {
		## ...
	## }
```

+ Some documentation updates.


04/11/17, v2.1.0

New features:

+ Descriptions and tags are no more mandatory
+ Alternative versions of the same chunk can now be defined like this:
```
	## chunk "ChunkName#fork1"{
		## ...
	## chunk "ChunkName#fork1"}
	## chunk "ChunkName#fork2"{
		## ...
	## chunk "ChunkName#fork2"}
	## the following sets the active chunk:
	rp$options(chunk="fork")
```
Outputs from different forks will be stored together in the repo
but all operations will refer to the output of the active
chunk. This is to be better documented.


Change log:

+ descriptions and tags are no more mandatory also in `attach`
+ fixed regression in `dependencies` (wrong plot edges)
+ fixed bug in `set`, was not working when setting `src` parameter


03/05/17, v2.0.5.13

+ Added forking

03/04/17, v2.0.5.12

+ Added `depends` function
+ Added `load` function
+ Added `force` parameter to `build`
+ More documentation updates


02/24/17, v2.0.5.11

+ Runs all checks
+ Documentation updated
+ `dependencies` now accepts overriding of default visual igraph
  parameters


02/22/17

+ A [paper about Repo](http://rdcu.be/pklt) has been published in BMC
  Bioinformatics.
+ Added some testing code.


10/18/16, v2.0.5.8

+ Major code refactoring. Direct call of `repo_*` function now
deprecated.


10/14/16

+ added `chunk` and `build` functions and `chunk` parameter to
  `put`. Repo can now associate a specific chunk of code with a
  resources and rebuild it upon request.
+ added support for special `project` items and corresponding
  `project` function and `prj` parameter in `put`. `project` items
  store session information automatically when `put` is called. The
  `info` command works differently on `project` items.
+ Improved aesthetics for the dependency graph through default visual
  parameters.
+ added `testthat` support for internal tests
+ added `options` function to set `put` default parameters.


10/08/16

+ added `buildURL` parameter to `set`. It will add an URL to all
items, such that it can be used is the repository is uploaded to a
website.

+ `get` can now be used on attachments, returns file path

+ Fixed bugs in `attach` not working with attachments

+ Fixed a few bugs with `copy` when copying multiple items. Now also
accepts `confirm` and `replace`

+ Added `related` function to extract items directly or indirectly
related to an input item.

+ added internal `testthat` unit testing

+ A few changes to `pies` and `dependencies`. They now support
additional parameters (...) to pass to `graphics:pie` and
`igraph:plot` respectively. `pies` now merges together all items with
size < 5% of the total. `dependencies` now supports tags to filter
nodes to be showed. Also missing documentation for `pies` has been
added.


05/03/16

+ new `attr` function in

+ `attach` now accepts `URL` parameter

+ 2.0.2 is now the latest stable on CRAN


05/02/16

+ 2.0.2 contains a fix to repo$pull for OS X

+ Documentation updated

+ NOTE: previous change implies that not all items have a "source"
field. This does not seem to be a problem. However, older items
storing current working directory as source could be affected.


04/28/16

+ now src must be an item (meant to be an attachment containing source
code). Documentation updated accordingly.

+ minor updates to vignette


04/27/2016

+ Bug fixes in lazydo and parameter check


04/24/2016

+ passes `devtools::check()`

+ stash simplified (now `stash(x)` instead of `stash("x")`)

+ Fixed bug when replacing entries due to new scheme for naming files.


01/04/2016

+ Bug fixes in lazydo

+ Minor additions to docs


03/20/3026

+ Fixed warnings in cpanel

+ All `check()` now passed

+ Merged latest fix from the dev branch

+ The NEWS file now used for main news


03/19/2016

+ Further testing

+ vignette updated

+ News moved to NEWS file

+ Minor release


03/18/2016

+ New pull feature now working

+ added lazydo (run expression and cache results)

+ Sources auto-attach suspended (had problems)


12/07/2015

+ Initial Shiny interface


11/19/2015

+ Sources are now auto-attached

+ Added safe-remove of data

+ Added auto-attach of sources

+ Added notes field


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
