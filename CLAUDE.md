# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

`repo` is an R package (CRAN) that provides a data-centered data flow manager. It stores R objects in a centralized local repository with annotations, tags, dependency tracking, and provenance information. Published in BMC Bioinformatics (DOI: 10.1186/s12859-017-1510-6).

## Common Commands

```r
# Check/build the package
R CMD check .
R CMD build .

# Run tests
Rscript -e "devtools::test()"

# Run a single test file
Rscript -e "testthat::test_file('tests/testthat/testrepo.R')"

# Generate documentation (requires roxygen2)
Rscript -e "devtools::document()"

# Load package interactively
Rscript -e "devtools::load_all()"

# Run rhub checks (for CRAN submission prep)
Rscript -e "rhub::check_for_cran()"
```

## Architecture

The package exposes a single `repo` R5/reference-class-like object. All user-facing functions follow the `repo_*` naming convention but are called as methods on a repo object: `rp$put(...)`, `rp$get(...)`, etc.

**File layout:**
- `R/repo.R` — Package documentation and `repo_open()`, the constructor that creates/loads a repository and returns a `repo` object with all methods bound to it
- `R/repo_public.R` — All public methods (`repo_put`, `repo_get`, `repo_info`, `repo_find`, `repo_tag`, `repo_rm`, `repo_build`, `repo_load`, `repo_depends`, `repo_copy`, `repo_export`, `repo_pies`, `repo_check`, etc.)
- `R/repo_private.R` — Internal helper functions (chunk parsing, source line extraction, error handling via `handleErr()`) and `repo_methods_private()`
- `R/repo_cpanel.R` — Optional Shiny-based visual interface (`repo_cpanel()`)

**How methods are wired:** `repo_open()` creates an environment that closes over the repository state and returns a list of bound functions. Public methods are defined in `repo_public.R` and private helpers in `repo_private.R`; both are sourced/evaluated inside `repo_open()` via `repo_methods_public()` and `repo_methods_private()`.

**Repository storage:** Data is serialized with `saveRDS`/`readRDS` into a directory (default `~/.R_repo`). An index file tracks metadata (name, description, tags, dimensions, size, dependencies, provenance, source chunks).

**Key dependencies:** `digest` (hashing), `tools` (package utilities). Optional: `igraph` (dependency graphs), `shiny` (cpanel), `knitr`/`rmarkdown` (vignettes), `testthat`.

## CRAN Submission Notes

The `cran-comments.md` file tracks submission notes. The `CRAN-SUBMISSION` file is auto-generated. Current target: 0 errors, 0 warnings, 0 notes.
