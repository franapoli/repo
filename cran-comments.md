
This responds to the following email:

> Please see the problems shown on
> <https://cran.r-project.org/web/checks/check_results_repo.html>.
> 
> Specifically, see the warnings about 'Documented arguments not in
> \usage' in the r-devel checks.  These are from a recent bug fix
> (PR#16223, see
> <https://bugs.r-project.org/bugzilla/show_bug.cgi?id=16223>): can
> you please fix your man pages as necessary?  (In most cases, remove
> the documentation for argument '...'.)
> 
> Please correct before 2020-02-17 to safely retain your package on
> CRAN.

I just implemented the suggestion.

## Test environments
* Ubuntu 19.04
* Travis Ci
* devtools (check_win_devel)
* rhub (debian-gcc-devel)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs

## Downstream dependencies
No downstream dependencies.

