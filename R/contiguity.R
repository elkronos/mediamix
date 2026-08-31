# The contiguity guard. This is the load-bearing piece of the recipes layer and
# the only part with no prior art to copy, so it is written and tested on its
# own before any filter maths depends on it.
#
# The question it answers: given a series the step has already seen up to
# `last_index`, observed at spacing `period`, does a new block of data continue
# that series, repeat part of it, or start somewhere else entirely?

#' @keywords internal
#' @noRd
.mm_infer_period <- function(index, arg = "index", call = parent.frame()) {
  v <- sort(unique(.mm_index_numeric(index, arg, call = call)))
  if (length(v) < 2L) return(NA_real_)
  stats::median(diff(v))
}

# Classify how `new_index` relates to a series last seen at `last_index`.
# Returns one of "unseen", "overlap", "contiguous", "gap".
#
# `tol` is a *relative* tolerance on the step size, and it is loose on purpose.
# Calendar months are 28 to 31 days long, so a monthly series steps by 0.90 to
# 1.10 inferred periods and a tight tolerance would call every February a gap.
# A tolerance of 0.25 still separates one period from two, which is the only
# distinction that changes the answer.
#' @keywords internal
#' @noRd
.mm_carry_case <- function(new_index, last_index, period, tol = 0.25) {
  if (is.null(last_index) || length(last_index) == 0L || is.na(last_index)) {
    return("unseen")
  }
  # "The training data had too few distinct time points to infer a spacing" is
  # a different problem from "this group was never seen", and it needs a
  # different message, so it gets its own case rather than being folded in.
  if (!is.finite(period) || period <= 0) return("unknown_period")
  start <- min(new_index)
  steps <- (start - last_index) / period
  if (steps <= tol) return("overlap")
  if (abs(steps - 1) <= tol) return("contiguous")
  "gap"
}

# How many periods of daylight sit between the two blocks. Reported in warnings
# so the user can see whether they are missing one week or eleven.
#' @keywords internal
#' @noRd
.mm_gap_size <- function(new_index, last_index, period) {
  (min(new_index) - last_index) / period - 1
}

# Is a single block itself evenly spaced? An adstock filter treats consecutive
# rows as consecutive periods, so an irregular index silently changes what the
# decay coefficient means.
#' @keywords internal
#' @noRd
.mm_irregular_steps <- function(index, period, tol = 0.25) {
  v <- sort(.mm_index_numeric(index))
  if (length(v) < 2L || !is.finite(period) || period <= 0) return(0L)
  d <- diff(v)
  sum(abs(d / period - 1) > tol)
}

#' @keywords internal
#' @noRd
.mm_check_duplicated_index <- function(index, group, call = parent.frame()) {
  if (!anyDuplicated(index)) return(invisible(NULL))
  where <- if (is.null(group)) "" else sprintf(" in group %s", group)
  cli::cli_abort(c(
    "The time index has repeated values{where}.",
    i = "Adstock needs one row per period per series.",
    i = "Either aggregate to one row per period, or name the column that \\
         separates the series in {.arg by}."
  ), call = call)
}
