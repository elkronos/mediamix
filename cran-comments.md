## Test environments

* local Ubuntu 24.04, R 4.3.3
* win-builder (devel and release)
* macOS builder

## R CMD check results

0 errors | 0 warnings | 1 note

The note is the standard "New submission" flag from the CRAN incoming checks.

## Notes for the reviewer

* `recipes`, `dials`, `generics` and `rlang` are in Suggests. The S3 methods for
  their generics use delayed registration (`S3method(recipes::prep, ...)`), so
  the package loads and the core transforms work with none of them installed.
  Every entry point that needs them checks and reports clearly.
* `ChannelAttribution`, `tune` and `parsnip` are in Suggests and used only in
  conditionally evaluated vignette chunks and examples.
* Both datasets are simulated; the generating scripts are in `data-raw/`.
* No DOIs are cited in the Description field. The two works referenced there
  predate reliable DOI assignment for their journals, and are given as full
  citations instead.
