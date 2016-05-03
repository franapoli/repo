
# News

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

+ Bugfixes in lazydo
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
