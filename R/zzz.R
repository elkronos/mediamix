# Helpers for the optional recipes / dials layers. These packages live in
# Suggests so that the core transforms stay dependency-free; every entry point
# that needs them says so plainly rather than failing on a missing object.

#' @keywords internal
#' @noRd
.mm_require_recipes <- function(call = parent.frame()) {
  missing_pkgs <- c("recipes", "rlang")[
    !vapply(c("recipes", "rlang"), requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing_pkgs) > 0L) {
    cli::cli_abort(c(
      "The {.pkg recipes} layer needs {length(missing_pkgs)} package{?s} that \\
       {?is/are} not installed.",
      i = "Install {?it/them} with \\
           {.code install.packages(c({paste0('\"', missing_pkgs, '\"', collapse = ', ')}))}.",
      i = "The core transforms, such as {.fn adstock_geometric}, need none of \\
           this."
    ), call = call)
  }
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
.mm_require_dials <- function(call = parent.frame()) {
  if (!requireNamespace("dials", quietly = TRUE)) {
    cli::cli_abort(c(
      "The {.pkg dials} package is required for tuning parameter objects.",
      i = 'Install it with {.code install.packages("dials")}.'
    ), call = call)
  }
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
.mm_rand_id <- function(prefix = "step", len = 5L) {
  if (requireNamespace("recipes", quietly = TRUE)) return(recipes::rand_id(prefix, len))
  paste(prefix, paste(sample(c(letters, 0:9), len, replace = TRUE), collapse = ""),
        sep = "_")
}

#' @keywords internal
#' @noRd
.mm_sel2char <- function(terms) {
  if (requireNamespace("recipes", quietly = TRUE)) return(recipes::sel2char(terms))
  "<selector>"
}

# Row indices per group, as a named list. A single unnamed series is one group.
#' @keywords internal
#' @noRd
.mm_step_groups <- function(data, by) {
  if (is.null(by) || length(by) == 0L) {
    return(list(".all" = seq_len(nrow(data))))
  }
  # `.mm_paste_key()` joins with a control character that cannot occur in text
  # read from a file. A printable separator would let two distinct key
  # combinations collide into one group, silently, and a collided group
  # warm-starts the second series off the first series' carryover.
  split(seq_len(nrow(data)), .mm_paste_key(data[by]))
}

#' @keywords internal
#' @noRd
.mm_assert_cols <- function(data, cols, call = parent.frame()) {
  cols <- cols[!is.na(cols)]
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(c(
      "{length(missing_cols)} column{?s} named in the step {?is/are} not in \\
       the data.",
      x = "Missing: {.val {missing_cols}}."
    ), call = call)
  }
  invisible(TRUE)
}
