
This fixes a problem with compiling the vignette. I was explictly
creating a randomfolder, than removing it at the end. This version
does not remove it anymore.

This update also includes a new feature that makes stored data more
human accessible.

## Test environments
* Ubuntu 19.04
* Travis Ci
* devtools (check_on_debian, check_on_windows, release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs

## Downstream dependencies
No downstream dependencies.

