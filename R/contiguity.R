# The contiguity guard. This is the load-bearing piece of the recipes layer and
# the only part with no prior art to copy, so it is written and tested on its
# own before any filter maths depends on it.
#
# The question it answers: given a series the step has already seen at the
# time points `train_index`, observed at spacing `period`, does a new block of
# data continue that series, repeat part of it, or start somewhere else
# entirely?

#' @keywords internal
#' @noRd
.mm_infer_period <- function(index, arg = "index", call = parent.frame()) {
  v <- sort(unique(.mm_index_numeric(index, arg, call = call)))
  if (length(v) < 2L) return(NA_real_)
  stats::median(diff(v))
}

# The period of each group: `period` when the user gave one, otherwise the
# median step of that group's own index. Pooling the groups first is wrong
# when geographies report on different weekdays, because their dates
# interleave and the pooled median step comes out at a few days. A group with
# fewer than two time points borrows the median of the other groups' periods.
#' @keywords internal
#' @noRd
.mm_group_periods <- function(index, groups, period = NULL) {
  if (!is.null(period)) return(rep(period, length(groups)))
  p <- vapply(groups, function(rows) .mm_infer_period(index[rows]),
              numeric(1), USE.NAMES = FALSE)
  ok <- is.finite(p) & p > 0
  if (any(ok) && !all(ok)) p[!ok] <- stats::median(p[ok])
  p
}

# Classify how a new block relates to the training series of the same group,
# whose sorted time points are `train_index`. Returns a list with `case` and
# `k`, the number of training time points strictly before the block's first
# row. When the case is "overlap" or "contiguous", bake() runs the filter over
# the first `k` training rows followed by the new rows, which gives the new
# rows exactly the values a filter over the whole series gives them.
#
#   "overlap"         the block starts at or before the last training time
#                     point and lines up with the training rows. With k = 0
#                     nothing precedes it, so the filter starts from zero, as
#                     it did when the step was prepared.
#   "contiguous"      the block starts about one period after the last
#                     training time point.
#   "misaligned"      it starts less than one period after the training time
#                     point before it, so it does not line up with the series.
#   "gap"             it starts more than one period after that time point.
#   "unseen"          the group has no training data.
#   "unknown_period"  there is no period to judge the alignment by.
#
# `tol` is a *relative* tolerance on the step size, and it is loose on purpose.
# Calendar months are 28 to 31 days long, so a monthly series steps by 0.90 to
# 1.10 inferred periods and a tight tolerance would call every February a gap.
# It must stay below 0.5 so that one period, two periods and a start part-way
# through a period remain distinct.
#' @keywords internal
#' @noRd
.mm_carry_case <- function(new_index, train_index, period, tol = 0.25) {
  train_index <- train_index[!is.na(train_index)]
  n <- length(train_index)
  if (n == 0L) return(list(case = "unseen", k = 0L))
  start <- min(new_index)
  k <- sum(train_index < start)
  # A block that starts on a training time point lines up by construction,
  # however unevenly the training data was spaced; prep() has already warned
  # about uneven spacing there.
  if (k == 0L || (k < n && any(train_index == start))) {
    return(list(case = "overlap", k = k))
  }
  # "Too few distinct time points to infer a spacing" is a different problem
  # from "this group was never seen", and it needs a different message.
  if (!is.finite(period) || period <= 0) {
    return(list(case = "unknown_period", k = k))
  }
  steps <- (start - train_index[k]) / period
  case <- if (steps < 1 - tol) {
    "misaligned"
  } else if (steps <= 1 + tol) {
    if (k < n) "overlap" else "contiguous"
  } else {
    "gap"
  }
  list(case = case, k = k)
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

# A missing time point cannot be placed in the series: sorted, it lands last
# and stands in for the end of the training data. An infinite one is no better.
#' @keywords internal
#' @noRd
.mm_check_index_finite <- function(index, groups, grouped, col,
                                   call = parent.frame()) {
  bad <- !is.finite(index)
  if (!any(bad)) return(invisible(NULL))
  hit <- names(groups)[vapply(groups, function(rows) any(bad[rows]),
                              logical(1), USE.NAMES = FALSE)]
  msg <- c("The time index {.val {col}} has {sum(bad)} missing or infinite \\
            value{?s}.")
  if (grouped) {
    shown <- .mm_show_key(hit)
    msg <- c(msg, i = "In group{?s} {.val {utils::head(shown, 5)}}.")
  }
  cli::cli_abort(c(
    msg,
    i = "Every row needs a time point. Fill in or drop those rows first."
  ), call = call)
}
