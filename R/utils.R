# Internal helpers shared across the package. Not exported.

#' @keywords internal
#' @noRd
.mm_check_numeric <- function(x, arg = "x", allow_na = TRUE, finite = FALSE,
                              call = parent.frame()) {
  if (!is.numeric(x) || is.matrix(x) || is.data.frame(x)) {
    cli::cli_abort("{.arg {arg}} must be a numeric vector, not {.obj_type_friendly {x}}.",
                   call = call)
  }
  if (length(x) == 0L) {
    cli::cli_abort("{.arg {arg}} must have at least one element.", call = call)
  }
  if (!allow_na && anyNA(x)) {
    cli::cli_abort("{.arg {arg}} must not contain missing values.", call = call)
  }
  if (finite && any(!is.finite(x) & !is.na(x))) {
    cli::cli_abort("{.arg {arg}} must contain only finite values.", call = call)
  }
  as.numeric(x)
}

#' @keywords internal
#' @noRd
.mm_check_scalar <- function(x, arg, lower = -Inf, upper = Inf,
                             inclusive = c(TRUE, TRUE), allow_inf = FALSE,
                             call = parent.frame()) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x)) {
    cli::cli_abort("{.arg {arg}} must be a single non-missing number.", call = call)
  }
  if (!allow_inf && !is.finite(x)) {
    cli::cli_abort("{.arg {arg}} must be finite.", call = call)
  }
  lo_ok <- if (inclusive[1L]) x >= lower else x > lower
  hi_ok <- if (inclusive[2L]) x <= upper else x < upper
  if (!lo_ok || !hi_ok) {
    # An infinite bound is closed only when infinite values are accepted.
    lb <- if (inclusive[1L] && (is.finite(lower) || allow_inf)) "[" else "("
    rb <- if (inclusive[2L] && (is.finite(upper) || allow_inf)) "]" else ")"
    shown <- .mm_format_num(x)
    cli::cli_abort(
      "{.arg {arg}} must be in {lb}{lower}, {upper}{rb}, not {shown}.",
      call = call
    )
  }
  as.numeric(x)
}

# `max` bounds the finite values a caller accepts; the default `Inf` sets no
# bound beyond the integer range.
#' @keywords internal
#' @noRd
.mm_check_count <- function(x, arg, min = 1L, allow_inf = FALSE,
                            call = parent.frame(), max = Inf) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x)) {
    cli::cli_abort("{.arg {arg}} must be a single non-missing number.", call = call)
  }
  if (is.infinite(x)) {
    if (!allow_inf || x < 0) {
      cli::cli_abort("{.arg {arg}} must be a finite positive integer.", call = call)
    }
    return(Inf)
  }
  shown <- .mm_format_num(x)
  if (x %% 1 != 0) {
    cli::cli_abort("{.arg {arg}} must be a whole number, not {shown}.", call = call)
  }
  if (x < min) {
    cli::cli_abort("{.arg {arg}} must be at least {min}, not {shown}.", call = call)
  }
  int_max <- .Machine$integer.max
  if (x > int_max) {
    cli::cli_abort(c(
      "{.arg {arg}} is too large.",
      x = "{.val {x}} exceeds the largest representable integer \\
           ({.val {int_max}}).",
      i = if (allow_inf) "Use {.code Inf} for an unbounded kernel."
    ), call = call)
  }
  if (x > max) {
    shown_max <- .mm_format_num(max)
    cli::cli_abort(
      "{.arg {arg}} must be at most {shown_max}, not {shown}.",
      call = call
    )
  }
  as.integer(x)
}

# A number written with enough digits to tell it from its neighbours, so a
# message never reports 2.9999999999999996 as 3.
#' @keywords internal
#' @noRd
.mm_format_num <- function(x) {
  out <- format(x, digits = 15)
  if (is.finite(x) && as.numeric(out) != x) out <- format(x, digits = 17)
  out
}

#' @keywords internal
#' @noRd
.mm_check_by <- function(by, n, call = parent.frame()) {
  if (is.null(by)) return(NULL)
  if (is.data.frame(by)) {
    if (ncol(by) == 0L) {
      cli::cli_abort("{.arg by} is a data frame with no columns.", call = call)
    }
    if (nrow(by) != n) {
      cli::cli_abort(
        "{.arg by} must have the same length as {.arg x} ({n}), not {nrow(by)}.",
        call = call
      )
    }
    .mm_check_key_columns(by, "by", call = call)
    return(.mm_paste_key(by, call = call))
  }
  if (length(by) != n) {
    cli::cli_abort(
      "{.arg by} must have the same length as {.arg x} ({n}), not {length(by)}.",
      call = call
    )
  }
  if (anyNA(by)) {
    cli::cli_abort("{.arg by} must not contain missing values.", call = call)
  }
  .mm_key_values(by)
}

# Grouping columns must be complete. Pasted into a key, a missing value reads
# as the text "NA" and merges with a real "NA" label such as North America.
#' @keywords internal
#' @noRd
.mm_check_key_columns <- function(cols, arg = "by", call = parent.frame()) {
  for (j in seq_along(cols)) {
    n_missing <- sum(is.na(cols[[j]]))
    if (n_missing > 0L) {
      col <- names(cols)[j]
      cli::cli_abort(c(
        "Column {.val {col}} of {.arg {arg}} has {n_missing} missing \\
         value{?s}.",
        i = "Recode them to a named group, or drop those rows."
      ), call = call)
    }
  }
  invisible(TRUE)
}

# One grouping column as character keys that depend only on the values: a
# whole number gives the same key whether it is stored as integer or double,
# distinct doubles never share a key, and date-times are written in UTC
# rather than in the session's time zone.
#' @keywords internal
#' @noRd
.mm_key_values <- function(v) {
  if (is.factor(v)) return(as.character(v))
  if (inherits(v, "POSIXt")) {
    out <- format(as.POSIXct(v), "%Y-%m-%d %H:%M:%OS6", tz = "UTC")
    return(sub("\\.?0+$", "", out))
  }
  if (inherits(v, "Date")) return(format(v))
  if (!is.double(v)) return(as.character(v))
  out <- as.character(v)
  whole <- is.finite(v) & v == trunc(v) & abs(v) < 2^53
  # Adding zero turns -0 into 0, which would otherwise print as "-0".
  out[whole] <- sprintf("%.0f", v[whole] + 0)
  rest <- which(is.finite(v) & !whole)
  inexact <- rest[as.numeric(out[rest]) != v[rest]]
  out[inexact] <- sprintf("%.17g", v[inexact])
  out
}

#' @keywords internal
#' @noRd
.mm_check_flag <- function(x, arg, call = parent.frame()) {
  if (!isTRUE(x) && !isFALSE(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be {.code TRUE} or {.code FALSE}, not \\
       {.obj_type_friendly {x}}.",
      call = call
    )
  }
  invisible(as.logical(x))
}

#' @keywords internal
#' @noRd
.mm_is_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}

# The index as plain numbers, in its own unit: days for a Date, seconds for a
# POSIXct.
#' @keywords internal
#' @noRd
.mm_index_numeric <- function(index, arg = "index", call = parent.frame()) {
  if (inherits(index, "POSIXct")) return(as.numeric(index))
  if (inherits(index, "Date")) return(as.numeric(index))
  if (is.numeric(index)) return(as.numeric(index))
  cli::cli_abort(
    "{.arg {arg}} must be numeric, {.cls Date} or {.cls POSIXct}, \\
     not {.obj_type_friendly {index}}.",
    call = call
  )
}

# Join several key columns into one grouping label. The separator is the
# ASCII unit separator, a control character that text read from a file almost
# never contains. A key value that does contain it could make two distinct
# combinations join to the same label, so .mm_join_key() refuses it.
#' @keywords internal
#' @noRd
.mm_key_sep <- function() "\x1f"

# Columns are unnamed before the call so that a column called `sep` or
# `collapse` cannot be taken for an argument of paste().
#' @keywords internal
#' @noRd
.mm_paste_key <- function(cols, call = parent.frame()) {
  .mm_join_key(unname(lapply(cols, .mm_key_values)), names(cols), call = call)
}

# `parts` is a list of character vectors of equal length, one per key column.
#' @keywords internal
#' @noRd
.mm_join_key <- function(parts, cols = NULL, call = parent.frame()) {
  parts <- unname(parts)
  if (length(parts) > 1L) {
    bad <- vapply(parts, function(p) any(grepl(.mm_key_sep(), p, fixed = TRUE)),
                  logical(1))
    if (any(bad)) {
      shown <- if (is.null(cols)) which(bad) else cols[bad]
      cli::cli_abort(c(
        "Grouping values must not contain the ASCII unit separator \\
         ({.code \\x1f}).",
        x = "Found in {.val {shown}}.",
        i = "It joins several grouping columns into one key, so a value \\
             containing it could merge two groups."
      ), call = call)
    }
  }
  do.call(paste, c(parts, list(sep = .mm_key_sep())))
}

# Readable group labels for output: the parts of each key joined with " / ".
# Two different keys must not end up with the same label, since the label is
# what later matching sees.
#' @keywords internal
#' @noRd
.mm_label_key <- function(key, arg = "by", call = parent.frame()) {
  label <- .mm_show_key(key)
  if (length(unique(label)) != length(unique(key))) {
    cli::cli_abort(c(
      "Two different groups in {.arg {arg}} give the same label when their \\
       columns are joined with {.val {' / '}}.",
      i = "Change a value that contains {.val {' / '}}."
    ), call = call)
  }
  label
}

# Split a joined key back into its parts, for display.
#' @keywords internal
#' @noRd
.mm_split_key <- function(key) {
  strsplit(key, .mm_key_sep(), fixed = TRUE)
}

# Render a group key legibly in a message.
#' @keywords internal
#' @noRd
.mm_show_key <- function(key) {
  vapply(.mm_split_key(key), paste, character(1), collapse = " / ")
}

# Validation strips a series to a bare double, so a transform puts the input's
# names and time-series attributes back on its output, which has the same
# length and order.
#' @keywords internal
#' @noRd
.mm_like_input <- function(out, x) {
  if (!is.null(names(x))) names(out) <- names(x)
  if (stats::is.ts(x)) {
    stats::tsp(out) <- stats::tsp(x)
    class(out) <- "ts"
  }
  out
}

# A short, printable name for a function passed as an argument. `deparse()` on
# an anonymous function yields a fragment of its source, which is not a label.
#' @keywords internal
#' @noRd
.mm_fn_label <- function(expr) {
  if (is.name(expr)) return(as.character(expr))
  if (is.call(expr) && identical(expr[[1L]], as.name("::"))) {
    return(as.character(expr[[3L]]))
  }
  "<anonymous>"
}

# Is this argument a `tune()` placeholder rather than a value? Recipe steps
# accept `tune()` in place of any tunable argument, so validation has to stand
# aside for it: eagerly checking the value would make the argument untunable.
#
# `hardhat::tune()` returns a call. A quosure or formula wrapped around one is
# unwrapped first.
#' @keywords internal
#' @noRd
.mm_is_tune <- function(x) {
  if (inherits(x, "quosure") || inherits(x, "formula")) {
    if (length(x) >= 2L) x <- x[[length(x)]]
  }
  is.call(x) && length(x) >= 1L &&
    identical(as.character(x[[1L]]), "tune")
}
