# Contributing to mediamix

Bug reports, questions and pull requests are welcome.

## Reporting a bug

Open an issue with a minimal reproducible example: the smallest data and the
shortest call that show the problem, the output you got, and the output you
expected. The `reprex` package formats this well. Include the result of
`sessionInfo()`.

## Changing code

1. Fork the repository and create a branch from `main`.
2. Install the development dependencies with
   `remotes::install_deps(dependencies = TRUE)`.
3. Make the change and add a test under `tests/testthat/` that fails before it
   and passes after.
4. If you change roxygen comments, run `devtools::document()` and commit the
   regenerated files in `man/` and `NAMESPACE`.
5. Run `devtools::check()` and make sure it reports no errors or warnings.
6. Add a bullet to the development section of `NEWS.md` describing the
   user-visible change.

Commit messages use an imperative subject line under 72 characters, with a
body when the reason for the change is not obvious from the diff.

## Code style

Follow the style of the file you are editing: base R and `cli` for messages,
argument checks through the validators in `R/utils.R`, and British spelling
in prose (`normalise`, `behaviour`).
