#' Geometric adstock weights
#'
#' The finite geometric kernel \eqn{w_i = \theta^i} for lags
#' \eqn{i = 0, 1, \ldots, L-1}. [adstock_geometric()] calls this internally when
#' `max_lag` is finite; it is exported because the weights themselves are often
#' what you want to plot or report.
#'
#' @param max_lag Number of periods the kernel spans, including the current
#'   period. `max_lag = 1` means no carryover.
#' @param decay Geometric decay coefficient in `[0, 1]`. See
#'   [decay_from_half_life()].
#' @param normalise Should the weights sum to 1? See the *Normalisation*
#'   section of [adstock_geometric()] for what this does to the interpretation
#'   of a downstream coefficient.
#'
#' @return A numeric vector of length `max_lag`, ordered from the current period
#'   to the most distant lag.
#'
#' @seealso [adstock_geometric()], [adstock_weights_weibull()]
#'
#' @examples
#' adstock_weights(max_lag = 5, decay = 0.5)
#'
#' # Unnormalised: the current period always carries weight 1
#' adstock_weights(max_lag = 5, decay = 0.5, normalise = FALSE)
#'
#' # Normalised weights sum to 1, so the series level is preserved
#' sum(adstock_weights(max_lag = 10, decay = 0.7))
#' @export
adstock_weights <- function(max_lag, decay, normalise = TRUE) {
  max_lag <- .mm_check_count(max_lag, "max_lag", min = 1L)
  decay <- .mm_check_scalar(decay, "decay", lower = 0, upper = 1)
  .mm_check_flag(normalise, "normalise")
  .mm_geometric_weights(max_lag, decay, max_lag, normalise)
}

#' Weibull adstock weights
#'
#' A two-parameter kernel that, unlike the geometric kernel, can place its peak
#' *after* the period in which the money was spent. Television, cinema and
#' out-of-home routinely behave this way: the response builds for a week or two
#' before it turns over. A geometric kernel cannot represent that shape at any
#' decay rate.
#'
#' @param max_lag Number of periods the kernel spans, including the current
#'   period. Unlike the geometric kernel, Weibull adstock has no infinite form:
#'   `max_lag` is required and must be finite.
#' @param shape Weibull shape parameter, positive. In the `"pdf"` form,
#'   `shape <= 1` produces a monotonically decaying kernel, and `shape > 1`
#'   allows a delayed peak without guaranteeing one: see *Details* for when the
#'   peak moves past the period of spend. In the `"cdf"` form, larger values
#'   produce a flatter plateau followed by a sharper drop.
#' @param scale Weibull scale parameter, positive, measured in periods. Larger
#'   values stretch the kernel over more periods.
#' @param type Either `"cdf"` (default) or `"pdf"`. See *Details*.
#' @param normalise Should the weights sum to 1? When `FALSE`, weights are
#'   scaled so the largest is 1, matching the unnormalised geometric convention.
#'
#' @return A numeric vector of length `max_lag`, ordered from the current period
#'   to the most distant lag.
#'
#' @details
#' The two forms answer different questions.
#'
#' The `"cdf"` form builds the kernel as a cumulative product of Weibull
#' survival values. Numbering lags from 1 so that \eqn{w_1} is the period of
#' spend, \eqn{w_i = \prod_{j<i} (1 - F(j))}, where \eqn{F} is the Weibull
#' distribution function, so \eqn{w_1 = 1} and each later weight is the
#' previous one times \eqn{1 - F(j)}. Read \eqn{1 - F(j)} as a time-varying
#' retention rate: it plays the role that the constant \eqn{\theta} plays in
#' the geometric kernel, which is the sense in which this form *generalises*
#' geometric decay. It is always monotonically decreasing, but the rate of
#' decay can itself change over time. The kernel is the running product of
#' these values, so it falls faster than the Weibull survival function
#' \eqn{1 - F(i)}. The construction is the one Robyn calls
#' `"weibull_cdf"`; Robyn, however, expresses `scale` as a quantile of the
#' window length rather than in periods, so its fitted scale values are not
#' directly comparable with this function's.
#'
#' The `"pdf"` form uses the Weibull density directly, \eqn{w_i \propto f(i)}.
#' This is the form that permits a delayed peak, and it is the reason to reach
#' for Weibull adstock at all. With `shape <= 1` it collapses back to a
#' monotone decay. Lags are numbered from 1 here too, so the period of spend
#' carries \eqn{f(1)} and the next period \eqn{f(2)}, and the kernel peaks
#' after the period of spend only when \eqn{f(2) > f(1)}. That needs
#' `shape > 1` and a Weibull mode, `scale * ((shape - 1) / shape)^(1 / shape)`,
#' of roughly 1.5 or more. With `shape = 2`, `scale = 2` puts the mode at 1.41
#' and the kernel still peaks in the period of spend; `scale = 3` moves the
#' peak one period later.
#'
#' @seealso [adstock_weibull()], [adstock_weights()]
#'
#' @examples
#' # Monotone decay, a flexible generalisation of geometric
#' round(adstock_weights_weibull(8, shape = 2, scale = 3, type = "cdf"), 4)
#'
#' # Delayed peak: the largest single weight lands one period after the spend,
#' # not in the period of spend itself. No geometric decay rate can do this.
#' w <- adstock_weights_weibull(8, shape = 2, scale = 3, type = "pdf")
#' round(w, 4)
#' which.max(w)
#' @export
adstock_weights_weibull <- function(max_lag, shape, scale,
                                    type = c("cdf", "pdf"),
                                    normalise = TRUE) {
  type <- match.arg(type)
  max_lag <- .mm_check_count(max_lag, "max_lag", min = 1L)
  shape <- .mm_check_scalar(shape, "shape", lower = 0, inclusive = c(FALSE, TRUE))
  scale <- .mm_check_scalar(scale, "scale", lower = 0, inclusive = c(FALSE, TRUE))
  .mm_check_flag(normalise, "normalise")
  .mm_weibull_weights(max_lag, max_lag, shape, scale, type, normalise)
}

#' Geometric adstock
#'
#' Spreads each period's media forward in time with geometric decay, the
#' standard representation of advertising carryover. This is the forward-facing
#' half of the package's spine; [credit_time_decay()] is the same kernel run
#' backward.
#'
#' @param x Numeric vector of media spend (or impressions, or GRPs) in time
#'   order. `x` must already be sorted by time and evenly spaced; the function
#'   has no index argument and cannot check this for you. Use [step_adstock()]
#'   if you want the time index validated. Infinite values are an error, since
#'   one would turn every later value into `NaN`; missing values are handled by
#'   `na_action`.
#' @param decay Geometric decay coefficient in `[0, 1]`. See
#'   [decay_from_half_life()] for the half-life vocabulary.
#' @param max_lag Number of periods the kernel spans. The default `Inf` uses the
#'   infinite (recursive) form; a finite value truncates the kernel. See the
#'   *Infinite versus truncated kernels* section of [adstock_geometric()].
#' @param normalise Should the kernel sum to 1? Defaults to `TRUE`. See the
#'   *Normalisation* section of [adstock_geometric()].
#' @param state Carryover already in flight at the start of `x`, for chaining
#'   calls across contiguous chunks of a series. The default `0` is a cold
#'   start, meaning no media ran before `x` began. For the infinite kernel this
#'   is a single number; for a finite `max_lag` it is the preceding
#'   `max_lag - 1` values of `x`. Obtain it from [adstock_state()], which
#'   records the `max_lag` (and, for the infinite kernel, the `decay`) it was
#'   computed for, so that a state from another kernel is refused rather than
#'   used. A plain numeric vector of the right length is accepted too.
#'
#'   When `by` is supplied, pass either the scalar `0` (or `NULL`) to cold-start
#'   every group, or a named list with one entry per group -- which is the shape
#'   [adstock_state()] returns when it is given `by`. A list with a `NULL` entry
#'   is an error rather than a silent cold start for that group.
#' @param by Optional grouping vector, or data frame of grouping vectors, the
#'   same length as `x`. Adstock is applied independently within each group,
#'   which is what geo-level and panel models need. Never rely on
#'   `dplyr::group_by()` for this: grouping metadata does not reliably survive
#'   into every context where this function is called. Missing values are an
#'   error, in a vector or in any column of a data frame.
#' @param na_action What to do about missing values in `x`. `"error"` (the
#'   default) refuses to guess. `"zero"` treats missing media as no media, which
#'   is usually right for spend but is a substantive assumption. `"keep"` lets
#'   `NA` propagate through the filter, which for the recursive form poisons
#'   every subsequent value.
#' @param missing_state What to do when `state` is a per-group list with no
#'   entry for a group in `by`. `"error"` (the default) refuses; `"cold"`
#'   cold-starts those groups, as [step_adstock()] does for groups it did not
#'   see at `prep()` time.
#'
#' @return A numeric vector the same length as `x`, in the same order, with the
#'   names and time-series attributes of `x`. Row count and order are
#'   preserved unconditionally.
#'
#' @section Normalisation:
#' This is the argument that most often changes an answer without anyone
#' noticing, so it is explicit rather than implied.
#'
#' With `normalise = TRUE` the kernel weights sum to 1. A constant spend of
#' \eqn{c} adstocks to \eqn{c}, the series level is preserved, and a downstream
#' regression coefficient reads as the effect of *one unit of media*, directly
#' comparable to the coefficient on untransformed spend.
#'
#' With `normalise = FALSE` you get raw Koyck accumulation,
#' \eqn{a_t = x_t + \theta a_{t-1}}. A constant spend of \eqn{c} accumulates to
#' \eqn{c/(1-\theta)}, so the series level rises with the decay rate, and the
#' coefficient reads as the effect of one unit of *accumulated* media. Both are
#' defensible; comparing a coefficient fitted one way against a coefficient
#' fitted the other is not.
#'
#' Both conventions are in use. Robyn's geometric adstock is the unnormalised
#' recursion; the Bayesian MMM of Jin et al. (2017) normalises its kernel
#' weights to sum to 1. This package defaults to normalised because it keeps
#' the coefficient interpretable and keeps decay and coefficient magnitude
#' from trading off against each other during fitting.
#'
#' @section Infinite versus truncated kernels:
#' `max_lag = Inf` runs the recursive filter, an infinite impulse response. Its
#' entire memory is one number per series, which is what makes
#' [step_adstock()] able to warm-start across a train/test boundary without
#' storing any training rows.
#'
#' A finite `max_lag` truncates to a finite impulse response, which needs the
#' preceding `max_lag - 1` observations as state. Truncation is a modelling
#' choice, not an approximation to be minimised: use [effective_window()] to see
#' how much of the kernel a given `max_lag` retains. A `max_lag` longer than
#' the series costs no more than one as long as the series: from a cold start
#' only the lags the series can reach are computed, and the normalising total
#' comes from the closed form of the geometric series.
#'
#' Note that `decay = 1` with `normalise = TRUE` and `max_lag = Inf` is an
#' error, not an edge case: an undecaying infinite kernel has infinite mass and
#' cannot be normalised.
#'
#' @section Causality:
#' The filter is strictly causal. `adstock_geometric(x)[1:m]` is identical to
#' `adstock_geometric(x[1:m])` for every `m`, so no future spend can leak into a
#' past adstock value. This is what makes it safe to adstock a full series once
#' and then slice it into cross-validation folds, as [tune_carryover()] does.
#'
#' @seealso [adstock_weibull()] for delayed peaks, [adstock_state()] for
#'   chaining, [media_transform()] to compose with saturation,
#'   [credit_time_decay()] for the mirrored backward kernel.
#'
#' @references
#' Broadbent, S. (1979). One way TV advertisements work.
#' *Journal of the Market Research Society*, 21(3), 139--166.
#'
#' Jin, Y., Wang, Y., Sun, Y., Chan, D. and Koehler, J. (2017). Bayesian
#' methods for media mix modeling with carryover and shape effects. Google Inc.
#'
#' @examples
#' spend <- c(100, 0, 0, 0, 0, 0)
#'
#' # A single burst decaying forward through time
#' round(adstock_geometric(spend, decay = 0.5), 2)
#'
#' # Normalisation preserves the level of a constant series. The equality is
#' # asymptotic: from a cold start the filter needs a few periods to fill up,
#' # so take a value well after the burn-in.
#' constant <- rep(100, 100)
#' tail(adstock_geometric(constant, decay = 0.8), 1)                     # 100
#' tail(adstock_geometric(constant, decay = 0.8, normalise = FALSE), 1)  # 500
#'
#' # Half-life vocabulary
#' adstock_geometric(spend, decay = decay_from_half_life(2))
#'
#' # Dark weeks: carryover decays across a flight gap rather than resetting
#' flighted <- c(100, 100, 0, 0, 0, 0, 100)
#' round(adstock_geometric(flighted, decay = 0.6), 2)
#'
#' # Geo-level panel data
#' spend_panel <- c(100, 50, 25, 200, 100, 50)
#' geo <- c("north", "north", "north", "south", "south", "south")
#' round(adstock_geometric(spend_panel, decay = 0.5, by = geo), 2)
#' @export
adstock_geometric <- function(x, decay, max_lag = Inf, normalise = TRUE,
                              state = 0, by = NULL,
                              na_action = c("error", "zero", "keep"),
                              missing_state = c("error", "cold")) {
  na_action <- match.arg(na_action)
  missing_state <- match.arg(missing_state)
  x_in <- x
  x <- .mm_check_numeric(x, "x", finite = TRUE)
  decay <- .mm_check_scalar(decay, "decay", lower = 0, upper = 1)
  max_lag <- .mm_check_count(max_lag, "max_lag", min = 1L, allow_inf = TRUE)
  .mm_check_flag(normalise, "normalise")
  by <- .mm_check_by(by, length(x))
  x <- .mm_handle_na(x, na_action)

  if (normalise && is.infinite(max_lag) && decay == 1) {
    cli::cli_abort(c(
      "An infinite kernel with {.arg decay} = 1 has infinite mass and cannot \\
       be normalised.",
      i = "Set a finite {.arg max_lag}, or use {.code normalise = FALSE}."
    ))
  }

  state <- .mm_resolve_state(state, by, max_lag, decay = decay,
                             kernel = "geometric",
                             missing_state = missing_state)

  fun <- if (is.infinite(max_lag)) {
    function(xi, si) .mm_recursive_filter(xi, decay, normalise, si)
  } else {
    w <- .mm_geometric_weights(.mm_reach(x, state, max_lag), decay, max_lag,
                               normalise)
    function(xi, si) .mm_fir_filter(xi, w, si)
  }
  .mm_like_input(.mm_apply_stateful(x, by, state, fun), x_in)
}

#' Weibull adstock
#'
#' Adstock with a Weibull kernel, which can place its peak after the period of
#' spend. See [adstock_weights_weibull()] for the two parameterisations.
#'
#' @inheritParams adstock_geometric
#' @param shape,scale Weibull shape and scale, both positive. `scale` is in
#'   periods.
#' @param max_lag Number of periods the kernel spans. Required and finite:
#'   the Weibull kernel has no recursive form.
#' @param type Either `"cdf"` (monotone decay) or `"pdf"` (permits a delayed
#'   peak).
#' @param state The preceding `max_lag - 1` values of `x`, oldest first, or `0`
#'   for a cold start. When `by` is supplied, a named list with one entry per
#'   group. [adstock_state()] produces one of the right shape when called with
#'   the same `max_lag`; for a finite kernel the state is simply the tail of
#'   `x`, so `utils::tail(x, max_lag - 1)` works too.
#'
#' @return A numeric vector the same length as `x`, in the same order, with the
#'   names and time-series attributes of `x`.
#'
#' @seealso [adstock_geometric()], [adstock_weights_weibull()]
#'
#' @examples
#' spend <- c(100, 0, 0, 0, 0, 0, 0, 0)
#'
#' # Delayed peak: the response builds before it decays
#' round(adstock_weibull(spend, shape = 2, scale = 3, max_lag = 8,
#'                       type = "pdf"), 2)
#'
#' # Monotone form
#' round(adstock_weibull(spend, shape = 2, scale = 3, max_lag = 8,
#'                       type = "cdf"), 2)
#' @export
adstock_weibull <- function(x, shape, scale, max_lag,
                            type = c("cdf", "pdf"), normalise = TRUE,
                            state = 0, by = NULL,
                            na_action = c("error", "zero", "keep"),
                            missing_state = c("error", "cold")) {
  type <- match.arg(type)
  na_action <- match.arg(na_action)
  missing_state <- match.arg(missing_state)
  x_in <- x
  x <- .mm_check_numeric(x, "x", finite = TRUE)
  if (missing(max_lag)) {
    cli::cli_abort(c(
      "{.arg max_lag} is required for Weibull adstock.",
      i = "The Weibull kernel has no infinite recursive form. Use \\
           {.fn effective_window} on a comparable geometric decay if you need \\
           a starting value."
    ))
  }
  max_lag <- .mm_check_count(max_lag, "max_lag", min = 1L)
  shape <- .mm_check_scalar(shape, "shape", lower = 0, inclusive = c(FALSE, TRUE))
  scale <- .mm_check_scalar(scale, "scale", lower = 0, inclusive = c(FALSE, TRUE))
  .mm_check_flag(normalise, "normalise")
  by <- .mm_check_by(by, length(x))
  x <- .mm_handle_na(x, na_action)

  state <- .mm_resolve_state(state, by, max_lag, kernel = "weibull",
                             missing_state = missing_state)
  w <- .mm_weibull_weights(.mm_reach(x, state, max_lag), max_lag, shape, scale,
                           type, normalise)
  fun <- function(xi, si) .mm_fir_filter(xi, w, si)
  .mm_like_input(.mm_apply_stateful(x, by, state, fun), x_in)
}

#' Delayed adstock
#'
#' The delayed-peak geometric kernel of Jin et al. (2017),
#' \eqn{w_l = \theta^{(l - \delta)^2}} for lags \eqn{l = 0, \ldots, L-1},
#' where \eqn{\theta} is the retention rate and \eqn{\delta} the lag at which
#' the effect peaks. With `peak = 0` the weights fall off as
#' \eqn{\theta^{l^2}}, faster than geometric; with `peak > 0` the response
#' builds for `peak` periods before it decays, the shape television and
#' out-of-home often show. It is the delayed form used in Google's Bayesian
#' MMM work, and a one-parameter-per-idea alternative to the Weibull kernel:
#' `peak` is read directly in periods.
#'
#' @inheritParams adstock_geometric
#' @param decay Retention rate \eqn{\theta} in `(0, 1)`.
#' @param peak Lag of the peak effect, in periods, in `[0, max_lag - 1]`. Need
#'   not be a whole number.
#' @param max_lag Number of periods the kernel spans, including the current
#'   period. Required and finite: the kernel has no recursive form. Jin et al.
#'   use 13 weeks for weekly data.
#' @param normalise Should the weights sum to 1? When `FALSE` the weights are
#'   left as \eqn{\theta^{(l - \delta)^2}}, whose largest value is 1 when the
#'   peak falls on a whole period.
#' @param state The preceding `max_lag - 1` values of `x`, oldest first, or
#'   `0` for a cold start; a named list of them when `by` is supplied.
#'
#' @return `adstock_weights_delayed()` returns the kernel, ordered from the
#'   current period outward. `adstock_delayed()` returns a numeric vector the
#'   same length as `x`, in the same order, with the names and time-series
#'   attributes of `x`.
#'
#' @references
#' Jin, Y., Wang, Y., Sun, Y., Chan, D. and Koehler, J. (2017). Bayesian
#' methods for media mix modeling with carryover and shape effects. Google
#' Inc. <https://research.google/pubs/pub46001/>
#'
#' @seealso [adstock_geometric()], [adstock_weibull()], [media_transform()]
#'
#' @examples
#' # Peak two weeks after the spend
#' round(adstock_weights_delayed(8, decay = 0.6, peak = 2), 3)
#'
#' spend <- c(100, 0, 0, 0, 0, 0, 0, 0)
#' round(adstock_delayed(spend, decay = 0.6, peak = 2, max_lag = 8), 2)
#'
#' # Through media_transform()
#' media_transform(spend, adstock = list(kernel = "delayed", decay = 0.6,
#'                                       peak = 2, max_lag = 8))
#' @name adstock_delayed
NULL

#' @rdname adstock_delayed
#' @export
adstock_weights_delayed <- function(max_lag, decay, peak = 0, normalise = TRUE) {
  max_lag <- .mm_check_count(max_lag, "max_lag", min = 1L)
  decay <- .mm_check_scalar(decay, "decay", lower = 0, upper = 1,
                            inclusive = c(FALSE, FALSE))
  peak <- .mm_check_scalar(peak, "peak", lower = 0, upper = max_lag - 1)
  .mm_check_flag(normalise, "normalise")
  .mm_delayed_weights(max_lag, max_lag, decay, peak, normalise)
}

#' @rdname adstock_delayed
#' @export
adstock_delayed <- function(x, decay, peak = 0, max_lag, normalise = TRUE,
                            state = 0, by = NULL,
                            na_action = c("error", "zero", "keep"),
                            missing_state = c("error", "cold")) {
  na_action <- match.arg(na_action)
  missing_state <- match.arg(missing_state)
  x_in <- x
  x <- .mm_check_numeric(x, "x", finite = TRUE)
  if (missing(max_lag)) {
    cli::cli_abort(c(
      "{.arg max_lag} is required for delayed adstock.",
      i = "The kernel has no recursive form. Jin et al. (2017) use 13 periods \\
           for weekly data."
    ))
  }
  max_lag <- .mm_check_count(max_lag, "max_lag", min = 1L)
  decay <- .mm_check_scalar(decay, "decay", lower = 0, upper = 1,
                            inclusive = c(FALSE, FALSE))
  peak <- .mm_check_scalar(peak, "peak", lower = 0, upper = max_lag - 1)
  .mm_check_flag(normalise, "normalise")
  by <- .mm_check_by(by, length(x))
  x <- .mm_handle_na(x, na_action)

  state <- .mm_resolve_state(state, by, max_lag, kernel = "delayed",
                             missing_state = missing_state)
  w <- .mm_delayed_weights(.mm_reach(x, state, max_lag), max_lag, decay, peak,
                           normalise)
  fun <- function(xi, si) .mm_fir_filter(xi, w, si)
  .mm_like_input(.mm_apply_stateful(x, by, state, fun), x_in)
}

#' Adstock with an arbitrary kernel
#'
#' Applies a user-supplied weight vector as a causal filter. Use this when you
#' have a kernel from somewhere else -- an econometric study, a vendor's
#' published curve, a shape you fitted yourself -- and want the same state
#' handling, grouping and length guarantees as the built-in transforms.
#'
#' @inheritParams adstock_geometric
#' @param weights Numeric vector of kernel weights, ordered from the current
#'   period to the most distant lag. Not rescaled: whatever you supply is what
#'   is applied.
#' @param state The preceding `length(weights) - 1` values of `x`, oldest first,
#'   or `0` for a cold start -- that is, `utils::tail(x_previous,
#'   length(weights) - 1)`.
#'
#' @return A numeric vector the same length as `x`, in the same order, with the
#'   names and time-series attributes of `x`.
#'
#' @seealso [adstock_geometric()] and [adstock_weibull()] for the built-in
#'   kernels, [adstock_weights()] to build a geometric kernel to pass here,
#'   [adstock_state()] for chaining.
#'
#' @examples
#' # An explicitly humped kernel: the peak lands one period after the spend
#' adstock_filter(c(100, 0, 0, 0, 0), weights = c(0.2, 0.5, 0.3))
#'
#' # Weights are applied as supplied, never rescaled. These sum to 2, and the
#' # output level doubles accordingly.
#' adstock_filter(c(100, 100, 100, 100), weights = c(1, 1))
#'
#' # Chaining across chunks, and grouping, work as they do for the built-in
#' # kernels. Use `adstock_state()` with a matching `max_lag` to get the state.
#' w <- c(0.5, 0.3, 0.2)
#' whole <- adstock_filter(c(100, 80, 60, 40, 20), weights = w)
#' s <- utils::tail(c(100, 80, 60), length(w) - 1L)
#' chunked <- c(adstock_filter(c(100, 80, 60), weights = w),
#'              adstock_filter(c(40, 20), weights = w, state = s))
#' all.equal(chunked, whole)
#' @export
adstock_filter <- function(x, weights, state = 0, by = NULL,
                           na_action = c("error", "zero", "keep"),
                           missing_state = c("error", "cold")) {
  na_action <- match.arg(na_action)
  missing_state <- match.arg(missing_state)
  x_in <- x
  x <- .mm_check_numeric(x, "x", finite = TRUE)
  weights <- .mm_check_numeric(weights, "weights", allow_na = FALSE, finite = TRUE)
  by <- .mm_check_by(by, length(x))
  x <- .mm_handle_na(x, na_action)
  state <- .mm_resolve_state(state, by, length(weights), kernel = "filter",
                             missing_state = missing_state)
  fun <- function(xi, si) .mm_fir_filter(xi, weights, si)
  .mm_like_input(.mm_apply_stateful(x, by, state, fun), x_in)
}

#' Terminal adstock state
#'
#' Returns the carryover left in flight at the end of a series, in a form
#' [adstock_geometric()] and friends accept as their `state` argument. This is
#' how you continue a filter across contiguous chunks of a series without
#' re-running it from the beginning, and it is what [step_adstock()] stores at
#' `prep()` time.
#'
#' @inheritParams adstock_geometric
#' @param decay Geometric decay coefficient in `[0, 1]`. Required for the
#'   infinite kernel. A finite kernel's state is the tail of `x` and does not
#'   depend on the decay, so there `decay` can be left out.
#' @param max_lag Number of periods the kernel spans. `Inf` gives the recursive
#'   form, whose state is a single number.
#'
#' @return An object of class `mm_adstock_state`, a numeric vector that records
#'   what it was computed for. For the infinite kernel it holds a single
#'   number, the raw (unnormalised) accumulator, and records `max_lag` and
#'   `decay`. For a finite `max_lag` it holds the last `max_lag - 1` values of
#'   `x`, oldest first, and records `max_lag`. When `by` is supplied, a named
#'   list of such objects, one per group, in order of first appearance.
#'
#' @details
#' The stored state is always in *raw* units, independent of `normalise`, so a
#' state captured under one normalisation setting stays valid under the other.
#'
#' The transforms check a state against their own arguments and refuse one
#' computed for another kernel, instead of starting from the wrong carryover.
#' The recursive accumulator is valid only for the same `decay` and
#' `max_lag = Inf`. A finite state is the raw tail of the series, so it serves
#' every kernel with the same `max_lag`: [adstock_weibull()], [adstock_delayed()]
#' and [adstock_filter()] accept it as readily as [adstock_geometric()]. A
#' plain numeric vector is still accepted and is checked only for its length.
#'
#' The size of this object is the reason [step_adstock()] can survive a
#' train/test boundary without the leakage and row-count problems that
#' `recipes::step_lag()` runs into. For the recursive kernel it is one
#' double per series: no training observations are retained, so there is nothing
#' to leak and nothing to inflate the size of a fitted workflow.
#'
#' @seealso [adstock_geometric()], [adstock_weibull()], [adstock_filter()],
#'   and [step_adstock()], which stores this state at `prep()` time.
#'
#' @examples
#' first_half <- c(100, 80, 60, 40)
#' second_half <- c(20, 10, 5, 0)
#'
#' s <- adstock_state(first_half, decay = 0.5)
#' s
#'
#' # Filtering in two chunks with the state carried across gives exactly the
#' # same answer as filtering the whole series at once
#' chunked <- c(
#'   adstock_geometric(first_half, decay = 0.5),
#'   adstock_geometric(second_half, decay = 0.5, state = s)
#' )
#' whole <- adstock_geometric(c(first_half, second_half), decay = 0.5)
#' all.equal(chunked, whole)
#'
#' # A state from another kernel is refused
#' try(adstock_geometric(second_half, decay = 0.5, max_lag = 2, state = s))
#' @export
adstock_state <- function(x, decay, max_lag = Inf, state = 0, by = NULL,
                          na_action = c("error", "zero", "keep"),
                          missing_state = c("error", "cold")) {
  na_action <- match.arg(na_action)
  missing_state <- match.arg(missing_state)
  x <- .mm_check_numeric(x, "x", finite = TRUE)
  max_lag <- .mm_check_count(max_lag, "max_lag", min = 1L, allow_inf = TRUE)
  if (missing(decay)) {
    if (is.infinite(max_lag)) {
      cli::cli_abort(c(
        "{.arg decay} is required for the infinite kernel.",
        i = "Its state is an accumulator built at one decay rate."
      ))
    }
    decay <- NULL
  } else {
    decay <- .mm_check_scalar(decay, "decay", lower = 0, upper = 1)
  }
  by <- .mm_check_by(by, length(x))
  x <- .mm_handle_na(x, na_action)
  state <- .mm_resolve_state(state, by, max_lag, decay = decay,
                             kernel = "geometric",
                             missing_state = missing_state)

  one <- function(xi, si) {
    .mm_new_state(.mm_terminal_state(xi, decay, max_lag, si), max_lag, decay)
  }
  if (is.null(by)) return(one(x, state))
  idx <- .mm_group_index(by)
  out <- vector("list", length(idx))
  for (i in seq_along(idx)) out[[i]] <- one(x[idx[[i]]], state[[i]])
  stats::setNames(out, names(idx))
}

#' @export
print.mm_adstock_state <- function(x, ...) {
  lag <- attr(x, "max_lag")
  kernel <- attr(x, "kernel")
  decay <- attr(x, "decay")
  header <- if (is.null(lag)) {
    "<adstock state>"
  } else if (is.infinite(lag)) {
    paste0("<adstock state: ", if (is.null(kernel)) "recursive" else kernel,
           " accumulator",
           if (!is.null(decay)) paste0(" at decay ", format(decay)), ">")
  } else {
    sprintf("<adstock state: last %s value%s of x, for max_lag = %s>",
            format(lag - 1), if (lag == 2) "" else "s", format(lag))
  }
  cat(header, "\n", sep = "")
  print(as.numeric(x), ...)
  invisible(x)
}

#' Steady-state starting value for an adstock filter
#'
#' A filter started from zero assumes no media ran before the series began, so
#' the first periods of a long-carryover channel are understated: at
#' `decay = 0.85` a constant spend reaches only 15% of its steady-state adstock
#' in the first period. This function returns the state the filter would hold
#' had the average spend of the first `periods` been running forever, which is
#' the usual remedy when real pre-period spend is not available.
#'
#' @inheritParams adstock_geometric
#' @param decay Geometric decay coefficient in `[0, 1]`. Required for the
#'   infinite kernel; a finite kernel's steady state does not depend on it, so
#'   there it is used only to choose the default `periods` and can be left out.
#' @param max_lag Number of periods the kernel spans. `Inf` gives the recursive
#'   form, whose state is a single number.
#' @param periods Number of leading periods to average. Defaults to the
#'   geometric kernel's 90% effective window, and for a finite `max_lag` to at
#'   most the `max_lag - 1` periods the kernel's state covers, capped at the
#'   series length, since that is how far back the start of the series
#'   "remembers".
#'
#' @return A state in the form [adstock_geometric()], [adstock_filter()] and
#'   friends accept: a single raw accumulator for `max_lag = Inf`, the
#'   `max_lag - 1` preceding values for a finite kernel, or a named list of
#'   these when `by` is supplied, in order of first appearance. These are
#'   plain numeric vectors, so they carry no record of the kernel and are
#'   checked only for their length when used.
#'
#' @details
#' For the recursive kernel the raw accumulator under constant spend \eqn{m} is
#' \eqn{m / (1 - \theta)}; for a finite kernel the preceding values are all
#' \eqn{m}. Either way a series that really was constant at \eqn{m} adstocks
#' to \eqn{m} (normalised) from its first period.
#'
#' The seed uses the series' own early *media*, never the KPI, so it does not
#' leak outcome information into a cross-validation. It does assume the
#' pre-period looked like the first few observed periods; when you have the
#' real pre-period spend, pass [adstock_state()] of it instead.
#'
#' Missing values follow `na_action`, as in [adstock_state()]: an error by
#' default, zeros with `"zero"`, and a missing state with `"keep"`.
#'
#' @seealso [adstock_state()], [adstock_geometric()], [tune_carryover()],
#'   which accepts `warm_start = TRUE`.
#'
#' @examples
#' x <- rep(100, 10)
#' round(adstock_geometric(x, decay = 0.85), 1)             # cold start
#' s <- adstock_steady_state(x, decay = 0.85)
#' round(adstock_geometric(x, decay = 0.85, state = s), 1)  # no burn-in
#'
#' # Finite kernels and grouped series work the same way
#' adstock_steady_state(c(10, 20, 30, 40), decay = 0.5, max_lag = 3)
#' adstock_steady_state(c(10, 20, 100, 200), decay = 0.5,
#'                      by = c("a", "a", "b", "b"), periods = 2)
#' @export
adstock_steady_state <- function(x, decay, max_lag = Inf, periods = NULL,
                                 by = NULL,
                                 na_action = c("error", "zero", "keep")) {
  na_action <- match.arg(na_action)
  x <- .mm_check_numeric(x, "x", finite = TRUE)
  max_lag <- .mm_check_count(max_lag, "max_lag", min = 1L, allow_inf = TRUE)
  if (missing(decay)) {
    if (is.infinite(max_lag)) {
      cli::cli_abort(c(
        "{.arg decay} is required for the infinite kernel.",
        i = "Its steady state is the mean spend divided by {.code 1 - decay}."
      ))
    }
    decay <- NULL
  } else {
    decay <- .mm_check_scalar(decay, "decay", lower = 0, upper = 1)
  }
  if (!is.null(periods)) periods <- .mm_check_count(periods, "periods", min = 1L)
  by <- .mm_check_by(by, length(x))
  x <- .mm_handle_na(x, na_action)
  if (is.infinite(max_lag) && decay == 1) {
    cli::cli_abort(c(
      "An undecaying infinite kernel has no steady state.",
      i = "Use a finite {.arg max_lag}, or a {.arg decay} below 1."
    ))
  }
  if (is.null(periods)) {
    window <- if (is.null(decay) || decay == 1) Inf else .mm_window(decay, 0.9)
    periods <- min(window, if (is.finite(max_lag)) max(1L, max_lag - 1L) else Inf)
  }
  one <- function(xi) {
    m <- mean(xi[seq_len(min(periods, length(xi)))])
    if (is.infinite(max_lag)) m / (1 - decay) else rep(m, max_lag - 1L)
  }
  if (is.null(by)) return(one(x))
  lapply(.mm_group_index(by), function(i) one(x[i]))
}

# ---- internals ---------------------------------------------------------------

#' @keywords internal
#' @noRd
.mm_handle_na <- function(x, na_action, call = parent.frame()) {
  if (!anyNA(x)) return(x)
  switch(
    na_action,
    error = cli::cli_abort(c(
      "{.arg x} contains {sum(is.na(x))} missing value{?s}.",
      i = "Use {.code na_action = \"zero\"} to treat missing media as no \\
           media, or impute before transforming."
    ), call = call),
    zero = {
      x[is.na(x)] <- 0
      x
    },
    keep = x
  )
}

# Row positions of each group, named by key and ordered by first appearance.
# Looping over positions rather than names keeps every label usable, a blank
# one included, and keeps the work linear in the number of groups.
#' @keywords internal
#' @noRd
.mm_group_index <- function(by) {
  split(seq_along(by), factor(by, levels = unique(by)))
}

# A classed state records what it was computed for, so that it can be checked
# where it is used. The recursive state is an accumulator built at one decay;
# a finite state is the raw tail of the series, which any kernel of the same
# length can use, so it records only `max_lag`.
#' @keywords internal
#' @noRd
.mm_new_state <- function(value, max_lag, decay) {
  if (is.infinite(max_lag)) {
    structure(value, kernel = "geometric", decay = decay, max_lag = max_lag,
              class = "mm_adstock_state")
  } else {
    structure(value, max_lag = max_lag, class = "mm_adstock_state")
  }
}

#' @keywords internal
#' @noRd
.mm_state_label <- function(group) {
  if (is.null(group)) "" else sprintf(" for group \"%s\"", .mm_show_key(group))
}

# Refuse a classed state whose recorded kernel does not match the call.
#' @keywords internal
#' @noRd
.mm_check_state_fit <- function(s, group, max_lag, decay, kernel, call) {
  s_lag <- attr(s, "max_lag")
  if (!is.null(s_lag) && !isTRUE(as.numeric(s_lag) == as.numeric(max_lag))) {
    label <- .mm_state_label(group)
    cli::cli_abort(c(
      "{.arg state}{label} was computed for a different kernel length.",
      x = "It is the state of a kernel with {.code max_lag = {s_lag}}; this \\
           call uses {.code max_lag = {max_lag}}.",
      i = "Recompute it with {.fn adstock_state} and the same {.arg max_lag}."
    ), call = call)
  }
  s_kernel <- attr(s, "kernel")
  if (!is.null(s_kernel) && !identical(s_kernel, kernel)) {
    label <- .mm_state_label(group)
    cli::cli_abort(c(
      "{.arg state}{label} is the accumulator of a {s_kernel} kernel, and \\
       this call runs a {kernel} kernel.",
      i = "Recompute it with the function that will use it."
    ), call = call)
  }
  s_decay <- attr(s, "decay")
  if (!is.null(s_decay) && !is.null(decay) && abs(s_decay - decay) > 1e-12) {
    label <- .mm_state_label(group)
    cli::cli_abort(c(
      "{.arg state}{label} was computed with {.code decay = {s_decay}}, not \\
       {.code decay = {decay}}.",
      i = "The recursive state is an accumulator built at one decay rate, so \\
           it does not carry over to another. Recompute it with \\
           {.fn adstock_state} and the same {.arg decay}."
    ), call = call)
  }
  invisible(TRUE)
}

# Normalise the user-facing `state` argument into one state per group, in the
# order of first appearance in `by`, or a single state when `by` is NULL. A
# cold start for a finite kernel is a zero-length vector, which the filters
# read as zeros, so a long `max_lag` allocates nothing until real state is
# supplied.
#' @keywords internal
#' @noRd
.mm_resolve_state <- function(state, by, max_lag, decay = NULL,
                              kernel = "geometric", missing_state = "error",
                              call = parent.frame()) {
  n_state <- if (is.infinite(max_lag)) 1L else max_lag - 1L
  cold <- if (is.infinite(max_lag)) 0 else numeric(0)

  coerce_one <- function(s, group) {
    if (inherits(s, "mm_adstock_state")) {
      .mm_check_state_fit(s, group, max_lag, decay, kernel, call)
      s <- as.numeric(s)
    }
    if (!is.numeric(s) || anyNA(s) || any(is.infinite(s))) {
      label <- .mm_state_label(group)
      cli::cli_abort(
        "{.arg state}{label} must be a numeric vector of finite, non-missing \\
         values.",
        call = call
      )
    }
    # A bare 0 is the documented cold-start sentinel. It is only ambiguous when
    # the kernel's state is itself a single number, and there a state of 0 and
    # a cold start mean exactly the same thing.
    if (length(s) == 1L && s == 0 && n_state != 1L) return(cold)
    if (length(s) != n_state) {
      label <- .mm_state_label(group)
      cli::cli_abort(c(
        "{.arg state}{label} has the wrong length for this kernel.",
        i = "Expected {n_state} value{?s}, got {length(s)}.",
        i = "Use {.fn adstock_state} to produce a state of the right shape."
      ), call = call)
    }
    as.numeric(s)
  }

  if (is.null(by)) {
    if (is.null(state)) return(cold)
    return(coerce_one(state, NULL))
  }

  groups <- unique(by)
  sentinel <- is.numeric(state) && !inherits(state, "mm_adstock_state") &&
    length(state) == 1L && isTRUE(state == 0)
  if (is.null(state) || sentinel) return(rep(list(cold), length(groups)))
  if (!is.list(state) || is.null(names(state))) {
    cli::cli_abort(c(
      "{.arg state} must be a named list when {.arg by} is supplied.",
      i = "One entry per group, named after the group.",
      i = "{.fn adstock_state} returns exactly this shape when given {.arg by}."
    ), call = call)
  }
  pos <- match(groups, names(state))
  absent <- which(is.na(pos))
  if (length(absent) > 0L && missing_state == "error") {
    shown <- .mm_show_key(groups[absent])
    cli::cli_abort(c(
      "{.arg state} is missing {length(absent)} group{?s}.",
      i = "Missing: {.val {utils::head(shown, 5)}}.",
      i = "Use {.code missing_state = \"cold\"} to cold-start them."
    ), call = call)
  }
  out <- rep(list(cold), length(groups))
  for (i in which(!is.na(pos))) {
    s <- state[[pos[i]]]
    if (is.null(s)) {
      label <- .mm_state_label(groups[i])
      cli::cli_abort(c(
        "{.arg state}{label} is {.code NULL}.",
        i = "Give every group a state, or pass {.code state = 0} to \\
             cold-start every group."
      ), call = call)
    }
    out[[i]] <- coerce_one(s, groups[i])
  }
  out
}

#' @keywords internal
#' @noRd
.mm_apply_stateful <- function(x, by, state, fun) {
  if (is.null(by)) return(fun(x, state))
  out <- numeric(length(x))
  idx <- .mm_group_index(by)
  for (i in seq_along(idx)) {
    rows <- idx[[i]]
    out[rows] <- fun(x[rows], state[[i]])
  }
  out
}

# How many leading weights any series in a call can reach: its length plus
# the length of its state, and never more than the kernel has. From a cold
# start that is the series length, however long `max_lag` is.
#' @keywords internal
#' @noRd
.mm_reach <- function(x, state, max_lag) {
  state_len <- if (is.list(state)) max(0L, lengths(state)) else length(state)
  min(max_lag, length(x) + state_len)
}

#' @keywords internal
#' @noRd
.mm_recursive_filter <- function(x, decay, normalise, state) {
  raw <- as.numeric(stats::filter(x, filter = decay, method = "recursive",
                                  init = state))
  if (normalise) raw * (1 - decay) else raw
}

# The first `m` weights of a geometric kernel spanning `max_lag` lags. The
# normalising total is the closed form of the geometric series, so it costs
# the same for any `max_lag`.
#' @keywords internal
#' @noRd
.mm_geometric_weights <- function(m, decay, max_lag, normalise) {
  w <- decay^(seq_len(m) - 1L)
  if (!normalise) return(w)
  total <- if (decay == 1) max_lag else -expm1(max_lag * log(decay)) / (1 - decay)
  w / total
}

# Scan a kernel of `max_lag` lags in blocks, keeping its first `m` weights and
# its total and largest weight. `block(from, to, last)` returns the weights for
# lags `from` to `to`, given the weight at lag `from - 1`. From lag `peak` on
# the weights never rise, so a block that ends in an exact zero ends the
# kernel. Memory grows with `m` and the block size, not with `max_lag`; a
# kernel of up to `size` lags is one block, computed as a single vector.
#' @keywords internal
#' @noRd
.mm_scan_kernel <- function(m, max_lag, block, peak = 1, size = 1e6) {
  head <- list()
  total <- 0
  top <- 0
  last <- NULL
  from <- 1
  repeat {
    to <- min(max_lag, from + size - 1)
    w <- suppressWarnings(block(from, to, last))
    w[!is.finite(w)] <- 0
    if (from <= m) head[[length(head) + 1L]] <- w[seq_len(min(to, m) - from + 1)]
    total <- total + sum(w)
    top <- max(top, w)
    last <- w[length(w)]
    if (to >= max_lag || (to >= m && to >= peak && last == 0)) break
    from <- to + 1
  }
  list(head = unlist(head, use.names = FALSE), total = total, top = top)
}

# The first `m` weights of a Weibull kernel spanning `max_lag` lags, scaled
# over the whole kernel.
#' @keywords internal
#' @noRd
.mm_weibull_weights <- function(m, max_lag, shape, scale, type, normalise,
                                call = parent.frame()) {
  if (type == "cdf") {
    block <- function(from, to, last) {
      if (from == 1) {
        s <- 1 - stats::pweibull(seq_len(to - 1), shape = shape, scale = scale)
        cumprod(c(1, s))
      } else {
        s <- 1 - stats::pweibull(seq.int(from - 1, to - 1), shape = shape,
                                 scale = scale)
        cumprod(c(last, s))[-1L]
      }
    }
    peak <- 1
  } else {
    block <- function(from, to, last) {
      stats::dweibull(seq.int(from, to), shape = shape, scale = scale)
    }
    peak <- if (shape > 1) ceiling(scale * ((shape - 1) / shape)^(1 / shape)) + 1 else 1
  }
  k <- .mm_scan_kernel(m, max_lag, block, peak = peak)

  if (!is.finite(k$total) || k$total <= 0) {
    # Which way the mass escaped depends on the scale relative to the window,
    # and the two cases need opposite remedies.
    too_late <- scale > max_lag
    cli::cli_abort(c(
      "The Weibull kernel is numerically zero at every lag.",
      i = if (too_late) {
        "{.arg scale} = {scale} puts all the mass beyond {.arg max_lag} = \\
         {max_lag}. Increase {.arg max_lag} or decrease {.arg scale}."
      } else {
        "{.arg shape} = {shape} and {.arg scale} = {scale} concentrate all \\
         the mass before the first lag. Increase {.arg scale}."
      }
    ), call = call)
  }
  if (normalise) k$head / k$total else k$head / k$top
}

# The first `m` weights of a delayed kernel spanning `max_lag` lags.
#' @keywords internal
#' @noRd
.mm_delayed_weights <- function(m, max_lag, decay, peak, normalise) {
  if (!normalise) return(decay^((seq_len(m) - 1 - peak)^2))
  block <- function(from, to, last) decay^((seq.int(from, to) - 1 - peak)^2)
  k <- .mm_scan_kernel(m, max_lag, block, peak = ceiling(peak) + 1)
  k$head / k$total
}

# Causal finite-impulse-response filter with explicit leading state. A
# zero-length state is a cold start. Weights beyond the reach of `x` and its
# state only ever multiply zeros, so they are dropped first.
#' @keywords internal
#' @noRd
.mm_fir_filter <- function(x, w, state) {
  n <- length(x)
  L <- min(length(w), n + length(state))
  if (L < length(w)) w <- w[seq_len(L)]
  if (L == 1L) return(x * w)
  pad <- if (length(state) == L - 1L) state else numeric(L - 1L)
  ext <- c(pad, x)
  out <- as.numeric(stats::filter(ext, filter = w, sides = 1L,
                                  method = "convolution"))
  out[(L):(L + n - 1L)]
}

#' @keywords internal
#' @noRd
.mm_terminal_state <- function(x, decay, max_lag, state) {
  if (is.infinite(max_lag)) {
    raw <- as.numeric(stats::filter(x, filter = decay, method = "recursive",
                                    init = state))
    return(raw[length(raw)])
  }
  k <- max_lag - 1L
  n <- length(x)
  if (k == 0L) return(numeric(0))
  if (n >= k) return(x[(n - k + 1L):n])
  # The state is the last `k` values of the state followed by `x`.
  pad <- if (length(state) == k) state[(n + 1L):k] else numeric(k - n)
  c(pad, x)
}
