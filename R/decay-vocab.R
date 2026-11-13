#' Carryover decay vocabulary
#'
#' Practitioners reason about media carryover in half-lives ("television's
#' effect halves every three weeks"), while the arithmetic needs a decay
#' coefficient. These three functions translate between the two and are shared
#' by both halves of the package: `decay` means the same thing in
#' [adstock_geometric()], which spreads one impulse of spend *forward* through
#' time, and in [credit_time_decay()], which spreads one conversion's credit
#' *backward* across prior touchpoints.
#'
#' @param half_life Numeric vector of half-lives: the number of periods over
#'   which the effect falls to half. Every element must be positive.
#' @param decay Numeric vector of geometric decay coefficients. A value of `0`
#'   means no carryover; values approaching `1` mean effect persists almost
#'   indefinitely. `effective_window()` accepts `[0, 1)`. `half_life()`
#'   requires `(0, 1)`, since a decay of exactly `0` has no half-life to
#'   report: the effect is gone before the next period.
#' @param period Spacing between observations, expressed in the same time unit
#'   as `half_life`, as a single number. The default `1` means `half_life` is
#'   already measured in periods, so `decay_from_half_life(3)` reads as "a
#'   three-period half-life". Supply both in days (say) to mix units:
#'   `decay_from_half_life(21, period = 7)` is a 21-day half-life observed
#'   weekly, and gives the same answer.
#' @param coverage Proportion of the total carryover effect the window should
#'   contain, a single number in `(0, 1)`.
#'
#' @return A numeric vector the same length as the first argument, carrying
#'   its names. `decay_from_half_life()` returns decay coefficients,
#'   `half_life()` returns half-lives in the time unit of `period` (numbers of
#'   periods with the default `period = 1`; days for `half_life(decay, period =
#'   7)` on weekly data), and `effective_window()` returns an integer vector of
#'   periods.
#'
#' @details
#' The geometric kernel places weight \eqn{\theta^i} on lag \eqn{i}, so the
#' effect halves after \eqn{h} periods when \eqn{\theta^h = 0.5}. Hence
#' \eqn{\theta = 0.5^{p/h}} and \eqn{h = p \log(0.5) / \log(\theta)}.
#'
#' `effective_window()` returns the smallest \eqn{n} for which the first
#' \eqn{n} lags carry at least `coverage` of the infinite kernel's total mass,
#' that is the smallest \eqn{n} with \eqn{1 - \theta^n \ge} `coverage`. At an
#' exact boundary, such as a coverage of 0.5 for a decay built from a whole
#' half-life, the comparison allows for rounding error, so
#' `effective_window(decay_from_half_life(3), 0.5)` is 3. It is
#' the honest way to choose `max_lag` for a truncated kernel, and a useful
#' sanity check on a fitted decay: a decay implying a 40-week effective window
#' on 104 weeks of data is not identified by the data.
#'
#' The first argument of each function is vectorised, so
#' `decay_from_half_life(c(1, 2, 4))` builds a grid of decays from half-lives,
#' ready for the `decays` argument of [tune_carryover()]. `period` and
#' `coverage` take a single value, which applies to every element. A missing
#' or out-of-range element is an error; in a vector of several, the message
#' gives the position of the first out-of-range element.
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
#' # A decay grid built from half-lives
#' decay_from_half_life(c(1, 2, 4, 8))
#'
#' # Names are kept, so one call covers every channel
#' decays <- c(tv = 0.85, video = 0.7, search = 0.15)
#' half_life(decays)
#' effective_window(decays, coverage = 0.90)
#'
#' # The vocabulary is shared by both halves of the package
#' adstock_geometric(c(100, 0, 0, 0, 0), decay = theta)
#' @name decay_vocabulary
NULL

#' @rdname decay_vocabulary
#' @export
decay_from_half_life <- function(half_life, period = 1) {
  nms <- names(half_life)
  half_life <- .mm_check_elements(half_life, "half_life", lower = 0,
                                  inclusive = c(FALSE, TRUE))
  period <- .mm_check_scalar(period, "period", lower = 0,
                             inclusive = c(FALSE, TRUE))
  out <- 0.5^(period / half_life)
  names(out) <- nms
  out
}

#' @rdname decay_vocabulary
#' @export
half_life <- function(decay, period = 1) {
  nms <- names(decay)
  decay <- .mm_check_elements(decay, "decay", lower = 0, upper = 1,
                              inclusive = c(FALSE, FALSE))
  period <- .mm_check_scalar(period, "period", lower = 0,
                             inclusive = c(FALSE, TRUE))
  out <- period * log(0.5) / log(decay)
  names(out) <- nms
  out
}

#' @rdname decay_vocabulary
#' @export
effective_window <- function(decay, coverage = 0.9) {
  nms <- names(decay)
  decay <- .mm_check_elements(decay, "decay", lower = 0, upper = 1,
                              inclusive = c(TRUE, FALSE))
  coverage <- .mm_check_scalar(coverage, "coverage", lower = 0, upper = 1,
                               inclusive = c(FALSE, FALSE))
  n <- vapply(decay, .mm_window, numeric(1), coverage = coverage)
  int_max <- .Machine$integer.max
  long <- which(!is.finite(n) | n > int_max)
  if (length(long) > 0L) {
    first <- long[1L]
    value <- decay[first]
    which_decay <- if (length(decay) == 1L) {
      "{.arg decay} = {value}"
    } else {
      "Element {first} of {.arg decay}, {value},"
    }
    cli::cli_abort(c(
      "The effective window is too long to represent.",
      x = paste(which_decay, "implies a window of more than \\
                 {.val {int_max}} periods."),
      i = "A decay this close to 1 is not identifiable from any real series; \\
           check it against {.fn half_life}."
    ))
  }
  out <- as.integer(n)
  names(out) <- nms
  out
}

# A numeric vector with every element inside the bounds. A single value goes
# through .mm_check_scalar(), so a scalar is checked and reported as it is
# everywhere else in the package; a longer vector is reported by the position
# of its first offending element.
#' @keywords internal
#' @noRd
.mm_check_elements <- function(x, arg, lower = -Inf, upper = Inf,
                               inclusive = c(TRUE, TRUE),
                               call = parent.frame()) {
  if (length(x) == 1L) {
    return(.mm_check_scalar(x, arg, lower = lower, upper = upper,
                            inclusive = inclusive, call = call))
  }
  x <- .mm_check_numeric(x, arg, allow_na = FALSE, finite = TRUE, call = call)
  lo_ok <- if (inclusive[1L]) x >= lower else x > lower
  hi_ok <- if (inclusive[2L]) x <= upper else x < upper
  bad <- which(!lo_ok | !hi_ok)
  if (length(bad) > 0L) {
    lb <- if (inclusive[1L]) "[" else "("
    rb <- if (inclusive[2L] && is.finite(upper)) "]" else ")"
    first <- bad[1L]
    shown <- .mm_format_num(x[first])
    n_bad <- length(bad)
    cli::cli_abort(c(
      "Every element of {.arg {arg}} must be in {lb}{lower}, {upper}{rb}.",
      x = "Element {first} is {shown}.",
      i = if (n_bad > 1L) "{n_bad} elements are out of range."
    ), call = call)
  }
  x
}

# The effective window as a double, without range checks. When coverage is
# exactly 1 - decay^k, rounding in the logarithms can put the ratio a hair
# above k and ceiling() one period too far, so a window one period shorter is
# taken when it meets the coverage to within rounding.
#' @keywords internal
#' @noRd
.mm_window <- function(decay, coverage) {
  if (decay == 0) return(1)
  n <- max(1, ceiling(log1p(-coverage) / log(decay)))
  if (n > 1 && decay^(n - 1) <= (1 - coverage) * (1 + 1e-10)) n <- n - 1
  n
}
