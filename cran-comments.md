
This fixes rmarkdown/knitr dependency:
https://github.com/yihui/knitr/issues/1864

Just added the following as suggested:
Suggests: rmarkdown
VignetteBuilder: knitr

## Test environments
* rhub (default platforms)

## R CMD check results
There were no ERRORs, WARNINGs. Ther was the following NOTE:
* checking for future file timestamps ... NOTE
unable to verify current time

## Downstream dependencies
gep2pep (also by me) is a rev dep, however repo's source is untouched

