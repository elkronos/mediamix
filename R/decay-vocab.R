#' Carryover decay vocabulary
#'
#' Practitioners reason about media carryover in half-lives ("television keeps
#' working for about three weeks"), while the arithmetic needs a decay
#' coefficient. These three functions translate between the two and are shared
#' by both halves of the package: `decay` means the same thing in
#' [adstock_geometric()], which spreads one impulse of spend *forward* through
#' time, and in [credit_time_decay()], which spreads one conversion's credit
#' *backward* across prior touchpoints.
#'
#' @param half_life Number of periods over which effect falls to half. Must be
#'   positive.
#' @param decay Geometric decay coefficient. A value of `0` means no carryover;
#'   values approaching `1` mean effect persists almost indefinitely.
#'   `effective_window()` accepts `[0, 1)`. `half_life()` requires `(0, 1)`,
#'   since a decay of exactly `0` has no half-life to report -- the effect is
#'   gone before the next period.
#' @param period Spacing between observations, expressed in the same time unit
#'   as `half_life`. The default `1` means `half_life` is already measured in
#'   periods, so `decay_from_half_life(3)` reads as "a three-period half-life".
#'   Supply both in days (say) to mix units: `decay_from_half_life(21, period = 7)`
#'   is a 21-day half-life observed weekly, and gives the same answer.
#' @param coverage Proportion of the total carryover effect the window should
#'   contain, in `(0, 1)`.
#'
#' @return A single number. `decay_from_half_life()` returns a decay
#'   coefficient, `half_life()` returns a number of periods, and
#'   `effective_window()` returns an integer number of periods.
#'
#' @details
#' The geometric kernel places weight \eqn{\theta^i} on lag \eqn{i}, so the
#' effect halves after \eqn{h} periods when \eqn{\theta^h = 0.5}. Hence
#' \eqn{\theta = 0.5^{p/h}} and \eqn{h = p \log(0.5) / \log(\theta)}.
#'
#' `effective_window()` returns the smallest \eqn{n} for which the first
#' \eqn{n} lags carry at least `coverage` of the infinite kernel's total mass,
#' that is the smallest \eqn{n} with \eqn{1 - \theta^n \ge} `coverage`. It is
#' the honest way to choose `max_lag` for a truncated kernel, and a useful
#' sanity check on a fitted decay: a decay implying a 40-week effective window
#' on 104 weeks of data is not identified by the data.
#'
#' @seealso [adstock_geometric()], which spreads spend forward with this decay,
#'   and [credit_time_decay()], which spreads credit backward with the same one.
#'   `vignette("spine")` shows they are the same kernel.
#'
#' @examples
#' # A three-week half-life on weekly data
#' theta <- decay_from_half_life(3)
#' theta
#'
#' # Round trip
#' half_life(theta)
#'
#' # The same half-life stated in days, observed weekly
#' decay_from_half_life(21, period = 7)
#'
#' # How many periods to keep before truncating loses 10% of the effect?
#' effective_window(theta, coverage = 0.90)
#'
#' # The vocabulary is shared by both halves of the package
#' adstock_geometric(c(100, 0, 0, 0, 0), decay = theta)
#' @name decay_vocabulary
NULL

#' @rdname decay_vocabulary
#' @export
decay_from_half_life <- function(half_life, period = 1) {
  half_life <- .mm_check_scalar(half_life, "half_life", lower = 0,
                                inclusive = c(FALSE, TRUE))
  period <- .mm_check_scalar(period, "period", lower = 0,
                             inclusive = c(FALSE, TRUE))
  0.5^(period / half_life)
}

#' @rdname decay_vocabulary
#' @export
half_life <- function(decay, period = 1) {
  decay <- .mm_check_scalar(decay, "decay", lower = 0, upper = 1,
                            inclusive = c(FALSE, FALSE))
  period <- .mm_check_scalar(period, "period", lower = 0,
                             inclusive = c(FALSE, TRUE))
  period * log(0.5) / log(decay)
}

#' @rdname decay_vocabulary
#' @export
effective_window <- function(decay, coverage = 0.9) {
  decay <- .mm_check_scalar(decay, "decay", lower = 0, upper = 1,
                            inclusive = c(TRUE, FALSE))
  coverage <- .mm_check_scalar(coverage, "coverage", lower = 0, upper = 1,
                               inclusive = c(FALSE, FALSE))
  if (decay == 0) return(1L)
  n <- ceiling(log(1 - coverage) / log(decay))
  int_max <- .Machine$integer.max
  if (!is.finite(n) || n > int_max) {
    cli::cli_abort(c(
      "The effective window is too long to represent.",
      x = "{.arg decay} = {decay} implies a window of more than \\
           {.val {int_max}} periods.",
      i = "A decay this close to 1 is not identifiable from any real series; \\
           check it against {.fn half_life}."
    ))
  }
  as.integer(n)
}
