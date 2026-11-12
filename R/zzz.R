# Helpers for the optional recipes / dials layers. These packages live in
# Suggests so that the core transforms stay dependency-free; every entry point
# that needs them says so plainly rather than failing on a missing object.

#' @keywords internal
#' @noRd
.mm_require_recipes <- function(call = parent.frame()) {
  missing_pkgs <- c("recipes", "rlang")[
    !vapply(c("recipes", "rlang"), requireNamespace, logical(1), quietly = TRUE)
  ]
  .mm_abort_missing_recipes(missing_pkgs, call = call)
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
.mm_abort_missing_recipes <- function(missing_pkgs, call = parent.frame()) {
  if (length(missing_pkgs) == 0L) return(invisible(NULL))
  cli::cli_abort(c(
    "The {.pkg recipes} layer needs {length(missing_pkgs)} package{?s} that \\
     {?is/are} not installed.",
    i = "Install {cli::qty(length(missing_pkgs))}{?it/them} with \\
         {.code install.packages(c({paste0('\"', missing_pkgs, '\"', collapse = ', ')}))}.",
    i = "The core transforms, such as {.fn adstock_geometric}, need none of \\
         this."
  ), call = call)
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

# Row indices per group, as a named list in order of first appearance, so the
# result does not depend on the collation order of the session's locale. A
# single unnamed series is one group. Names are the joined keys; callers that
# look groups up should match on them rather than index the list by name,
# since a blank label gives a blank name.
#' @keywords internal
#' @noRd
.mm_step_groups <- function(data, by, call = parent.frame()) {
  if (is.null(by) || length(by) == 0L) {
    return(list(".all" = seq_len(nrow(data))))
  }
  # The parts are joined with a control character that text read from a file
  # almost never contains, and values that contain it are refused. A printable
  # separator would let two distinct key combinations collide into one group,
  # silently, and a collided group warm-starts the second series off the first
  # series' carryover.
  parts <- lapply(by, function(col) .mm_key_part(data[[col]], col, call))
  key <- .mm_join_key(parts, by, call = call)
  split(seq_len(nrow(data)), factor(key, levels = unique(key)))
}

#' @keywords internal
#' @noRd
.mm_has_by <- function(by) !is.null(by) && length(by) > 0L

# Infinite spend has no meaning, and a filter or a curve fed one either
# carries it into every later period or turns it into a silent zero.
#' @keywords internal
#' @noRd
.mm_check_media_finite <- function(data, cols, call = parent.frame()) {
  has_inf <- cols[vapply(cols, function(cn) {
    v <- data[[cn]]
    is.numeric(v) && any(is.infinite(v))
  }, logical(1))]
  if (length(has_inf) > 0L) {
    cli::cli_abort(c(
      "{length(has_inf)} media column{?s} contain{?s/} infinite values: \\
       {.val {has_inf}}.",
      i = "Replace or drop the infinite values before this step."
    ), call = call)
  }
  invisible(NULL)
}

# One grouping column as text. A missing label would be pasted into the
# string "NA" and merged with a real group of that name, so it aborts.
# Numbers are written with 15 significant digits whatever their storage type,
# so a geography coded 1e5 in one data set and 100000L in another is one
# group; date-times are written in UTC, so the key does not depend on the
# session's time zone.
#' @keywords internal
#' @noRd
.mm_key_part <- function(x, col, call = parent.frame()) {
  if (anyNA(x)) {
    cli::cli_abort(c(
      "Grouping column {.val {col}} has {sum(is.na(x))} missing value{?s}.",
      i = "Every row needs a group. Fill in or drop the missing labels first."
    ), call = call)
  }
  if (inherits(x, "POSIXt")) {
    return(format(as.POSIXct(x), "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  }
  if (is.numeric(x)) {
    u <- unique(as.numeric(x))
    u[u == 0] <- 0
    return(trimws(sprintf("%.15g", u))[match(as.numeric(x), u)])
  }
  as.character(x)
}

#' @keywords internal
#' @noRd
.mm_assert_cols <- function(data, cols, call = parent.frame()) {
  cols <- cols[!is.na(cols)]
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(c(
      "{length(missing_cols)} column{?s} {?is/are} not in the data.",
      x = "Missing: {.val {missing_cols}}.",
      i = "Columns are named with character strings."
    ), call = call)
  }
  invisible(TRUE)
}

# Tidy methods return tibbles, as recipes' own steps do. The class is set
# directly so that tibble does not have to be declared as a dependency; a
# data frame with default row names is a valid tibble once it has the class.
#' @keywords internal
#' @noRd
.mm_tibble <- function(...) {
  out <- data.frame(..., stringsAsFactors = FALSE)
  rownames(out) <- NULL
  class(out) <- c("tbl_df", "tbl", "data.frame")
  out
}

# A step argument for display: a `tune()` placeholder is shown as the call,
# since formatting it as a number fails.
#' @keywords internal
#' @noRd
.mm_fmt_param <- function(x) {
  if (.mm_is_tune(x)) {
    if (inherits(x, "quosure") || inherits(x, "formula")) x <- x[[length(x)]]
    return(paste(deparse(x), collapse = ""))
  }
  if (is.numeric(x) && length(x) == 1L && is.infinite(x)) return("Inf")
  format(x)
}

# A numeric step argument for a tidy() column: a `tune()` placeholder has no
# value yet.
#' @keywords internal
#' @noRd
.mm_param_value <- function(x) {
  if (.mm_is_tune(x)) NA_real_ else x
}
