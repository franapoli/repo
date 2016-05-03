## Test environments
* ubuntu 14.04 LTS, R 3.2.3
* win-builder

## R CMD check results
There were no ERRORs or WARNINGs. There's the following NOTE:

Days since last update: 0

see comments below.

## Downstream dependencies
No downstream dependencies.

## Comments

This is a quick fix to cope with the comment below (by Brian
Ripley). I unset the method and let R choose the default.

> This fails on OS X with

> > rp$pull("item1")
> sh: wget: command not found
> Warning in download.file(e$URL, tf, method = "wget") :
>   download had nonzero exit status

> and wget is not even listed as a SystemRequirements.  OS X comes with
> curl, not wget, and in any case why is unnecessary to override the R
> default here?

> Please correct ASAP. 