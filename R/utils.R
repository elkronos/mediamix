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
    lb <- if (inclusive[1L]) "[" else "("
    rb <- if (inclusive[2L]) "]" else ")"
    cli::cli_abort(
      "{.arg {arg}} must be in {lb}{lower}, {upper}{rb}, not {.val {x}}.",
      call = call
    )
  }
  as.numeric(x)
}

#' @keywords internal
#' @noRd
.mm_check_count <- function(x, arg, min = 1L, allow_inf = FALSE,
                            call = parent.frame()) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x)) {
    cli::cli_abort("{.arg {arg}} must be a single non-missing number.", call = call)
  }
  if (is.infinite(x)) {
    if (!allow_inf || x < 0) {
      cli::cli_abort("{.arg {arg}} must be a finite positive integer.", call = call)
    }
    return(Inf)
  }
  if (x %% 1 != 0) {
    cli::cli_abort("{.arg {arg}} must be a whole number, not {.val {x}}.", call = call)
  }
  if (x < min) {
    cli::cli_abort("{.arg {arg}} must be at least {min}, not {.val {x}}.", call = call)
  }
  int_max <- .Machine$integer.max
  if (x > int_max) {
    cli::cli_abort(c(
      "{.arg {arg}} is too large to be a period count.",
      x = "{.val {x}} exceeds the largest representable integer \\
           ({.val {int_max}}).",
      i = "Use {.code Inf} for an unbounded kernel."
    ), call = call)
  }
  as.integer(x)
}

#' @keywords internal
#' @noRd
.mm_check_by <- function(by, n, call = parent.frame()) {
  if (is.null(by)) return(NULL)
  if (is.data.frame(by)) {
    by <- .mm_paste_key(by)
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
  as.character(by)
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

# Numeric spacing between index values, in whatever unit the index uses.
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

# Join several key columns into one grouping label. The separator is a
# control character that cannot occur in text read from a file, so two
# distinct key combinations can never collide into one group.
#' @keywords internal
#' @noRd
.mm_key_sep <- function() "\x1f"

#' @keywords internal
#' @noRd
.mm_paste_key <- function(cols) {
  do.call(paste, c(lapply(cols, as.character), list(sep = .mm_key_sep())))
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
