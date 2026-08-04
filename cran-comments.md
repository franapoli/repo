## Resubmission (3rd)

Fixed issues found in CRAN incoming pre-tests:

* Updated Date field in DESCRIPTION (was over a month old)
* Added missing \usage section to man/repo_open.Rd (caused NOTE on Debian)

Remaining NOTEs are unavoidable:
* "New submission / Package was archived" — inherent to resubmission
* "Possibly misspelled: Napolitano" — this is the maintainer's surname

## Resubmission (2nd)

As requested by Uwe Ligges: replaced invalid vignette URL in README.md
with the canonical CRAN package URL (https://CRAN.R-project.org/package=repo).

## Resubmission (1st)

This package was previously on CRAN but was archived due to check failures
(missing pre-built vignette in inst/doc, and related warnings). Those issues
have now been fixed:

* Added pre-built vignette output in inst/doc/
* Aligned VignetteIndexEntry title with YAML title
* Fixed .Rbuildignore to exclude spurious files from the build
* Fixed a bug in the repo_pies() example (data(1) -> data1)
* Removed redundant Author field (kept only Authors@R) in DESCRIPTION
* Updated Date field in DESCRIPTION
* Fixed stale/redirected URLs in README.md, NEWS.md and vignette

## Test environments

* macOS aarch64 (local), R 4.5.2
* rhub: Linux (R-devel), Windows (R-devel)

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies

gep2pep (also by me) is a reverse dependency, but repo's source is unchanged.
