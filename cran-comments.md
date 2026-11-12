## Test environments

* local Ubuntu 24.04, R 4.3.3
* GitHub Actions: macOS, Windows and Ubuntu (release), Ubuntu (devel, oldrel-1),
  and Ubuntu R 4.1 with hard dependencies only
* win-builder (devel and release)
* macOS builder

## R CMD check results

0 errors | 0 warnings | 1 note

The note is the standard "New submission" flag from the CRAN incoming checks.

## Notes for the reviewer

* `rlang` is in Imports because every error, warning and message goes through
  `cli::cli_abort()`, `cli::cli_warn()` and `cli::cli_inform()`, which call it,
  and the recipe steps call `rlang::enquos()` directly.
* `recipes`, `dials`, `generics` and `dplyr` are in Suggests. The S3 methods for
  their generics use delayed registration (`S3method(recipes::prep, ...)`), so
  the package loads and the core transforms work with none of them installed.
  Every entry point that needs them checks and reports clearly.
* `ChannelAttribution`, `tune` and `parsnip` are in Suggests and used only when
  they are installed: in conditionally evaluated vignette chunks, and for
  `ChannelAttribution` also in one example and one test.
* Both datasets are simulated; the generating scripts are in `data-raw/`.
* The Description field cites four works. Anderl et al. (2016) is given with
  its DOI and Jin et al. (2017) with its URL; Broadbent (1979) and Hill (1910)
  are given as full journal citations, without a DOI.
