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
  w <- decay^(seq_len(max_lag) - 1L)
  if (normalise) w / sum(w) else w
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
#'   `shape > 1` produces a delayed peak and `shape <= 1` produces a
#'   monotonically decaying kernel. In the `"cdf"` form, larger values produce a
#'   flatter plateau followed by a sharper drop.
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
#' survival probabilities. Numbering lags from 1 so that \eqn{w_1} is the
#' period of spend, \eqn{w_i = \prod_{j<i} (1 - F(j))}, so \eqn{w_1 = 1} and
#' each later weight is the previous one times the probability of surviving
#' another period. This is a
#' *generalisation of the geometric kernel*: it is always monotonically
#' decreasing, but the rate of decay can itself change over time, which the
#' single-parameter geometric kernel cannot do.
#'
#' The `"pdf"` form uses the Weibull density directly, \eqn{w_i \propto f(i)}.
#' This is the form that permits a delayed peak, and it is the reason to reach
#' for Weibull adstock at all. With `shape <= 1` it collapses back to a
#' monotone decay.
#'
#' @seealso [adstock_weibull()], [adstock_weights()]
#'
#' @examples
#' # Monotone decay, a flexible generalisation of geometric
#' round(adstock_weights_weibull(8, shape = 2, scale = 3, type = "cdf"), 4)
#'
#' # Delayed peak: most weight lands one period after the spend, not in the
#' # period of spend itself. No geometric decay rate can do this.
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

  lags <- seq_len(max_lag)
  w <- suppressWarnings(if (type == "cdf") {
    if (max_lag == 1L) {
      1
    } else {
      cumprod(c(1, 1 - stats::pweibull(lags[-max_lag], shape = shape, scale = scale)))
    }
  } else {
    stats::dweibull(lags, shape = shape, scale = scale)
  })
  w[!is.finite(w)] <- 0

  total <- sum(w)
  if (!is.finite(total) || total <= 0) {
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
    ))
  }
  if (normalise) w / total else w / max(w)
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
#'   if you want the time index validated.
#' @param decay Geometric decay coefficient in `[0, 1]`. See
#'   [decay_from_half_life()] for the half-life vocabulary.
#' @param max_lag Number of periods the kernel spans. The default `Inf` uses the
#'   infinite (recursive) form; a finite value truncates the kernel. See
#'   *Details*.
#' @param normalise Should the kernel sum to 1? Defaults to `TRUE`. See the
#'   *Normalisation* section.
#' @param state Carryover already in flight at the start of `x`, for chaining
#'   calls across contiguous chunks of a series. The default `0` is a cold
#'   start, meaning no media ran before `x` began. For the infinite kernel this
#'   is a single number; for a finite `max_lag` it is the preceding
#'   `max_lag - 1` values of `x`. Obtain it from [adstock_state()].
#'
#'   When `by` is supplied, pass either the scalar `0` (or `NULL`) to cold-start
#'   every group, or a named list with one entry per group -- which is the shape
#'   [adstock_state()] returns when it is given `by`. A list with a `NULL` entry
#'   is an error rather than a silent cold start for that group.
#' @param by Optional grouping vector, or data frame of grouping vectors, the
#'   same length as `x`. Adstock is applied independently within each group,
#'   which is what geo-level and panel models need. Never rely on
#'   `dplyr::group_by()` for this: grouping metadata does not reliably survive
#'   into every context where this function is called.
#' @param na_action What to do about missing values in `x`. `"error"` (the
#'   default) refuses to guess. `"zero"` treats missing media as no media, which
#'   is usually right for spend but is a substantive assumption. `"keep"` lets
#'   `NA` propagate through the filter, which for the recursive form poisons
#'   every subsequent value.
#'
#' @return A numeric vector the same length as `x`, in the same order. Row count
#'   and order are preserved unconditionally.
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
#' Robyn and most of the MMM literature use the unnormalised form. This package
#' defaults to normalised because it keeps the coefficient interpretable and
#' keeps decay and coefficient magnitude from trading off against each other
#' during fitting.
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
#' how much of the kernel a given `max_lag` retains.
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
                              na_action = c("error", "zero", "keep")) {
  na_action <- match.arg(na_action)
  x <- .mm_check_numeric(x, "x")
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

  state <- .mm_resolve_state(state, by, max_lag)

  fun <- function(xi, si) {
    .mm_adstock_geometric_one(xi, decay = decay, max_lag = max_lag,
                              normalise = normalise, state = si)
  }
  .mm_apply_stateful(x, by, state, fun)
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
#' @return A numeric vector the same length as `x`, in the same order.
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
                            na_action = c("error", "zero", "keep")) {
  type <- match.arg(type)
  na_action <- match.arg(na_action)
  x <- .mm_check_numeric(x, "x")
  if (missing(max_lag)) {
    cli::cli_abort(c(
      "{.arg max_lag} is required for Weibull adstock.",
      i = "The Weibull kernel has no infinite recursive form. Use \\
           {.fn effective_window} on a comparable geometric decay if you need \\
           a starting value."
    ))
  }
  max_lag <- .mm_check_count(max_lag, "max_lag", min = 1L)
  .mm_check_flag(normalise, "normalise")
  by <- .mm_check_by(by, length(x))
  x <- .mm_handle_na(x, na_action)

  w <- adstock_weights_weibull(max_lag, shape = shape, scale = scale,
                               type = type, normalise = normalise)
  state <- .mm_resolve_state(state, by, max_lag)
  fun <- function(xi, si) .mm_fir_filter(xi, w, si)
  .mm_apply_stateful(x, by, state, fun)
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
#' @return A numeric vector the same length as `x`, in the same order.
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
                           na_action = c("error", "zero", "keep")) {
  na_action <- match.arg(na_action)
  x <- .mm_check_numeric(x, "x")
  weights <- .mm_check_numeric(weights, "weights", allow_na = FALSE, finite = TRUE)
  by <- .mm_check_by(by, length(x))
  x <- .mm_handle_na(x, na_action)
  state <- .mm_resolve_state(state, by, length(weights))
  fun <- function(xi, si) .mm_fir_filter(xi, weights, si)
  .mm_apply_stateful(x, by, state, fun)
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
#' @param max_lag Number of periods the kernel spans. `Inf` gives the recursive
#'   form, whose state is a single number.
#'
#' @return For the infinite kernel, a single number: the raw (unnormalised)
#'   accumulator. For a finite `max_lag`, the last `max_lag - 1` values of `x`,
#'   oldest first. When `by` is supplied, a named list of such objects, one per
#'   group.
#'
#' @details
#' The stored state is always in *raw* units, independent of `normalise`, so a
#' state captured under one normalisation setting stays valid under the other.
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
#' @export
adstock_state <- function(x, decay, max_lag = Inf, state = 0, by = NULL,
                          na_action = c("error", "zero", "keep")) {
  na_action <- match.arg(na_action)
  x <- .mm_check_numeric(x, "x")
  decay <- .mm_check_scalar(decay, "decay", lower = 0, upper = 1)
  max_lag <- .mm_check_count(max_lag, "max_lag", min = 1L, allow_inf = TRUE)
  by <- .mm_check_by(by, length(x))
  x <- .mm_handle_na(x, na_action)
  state <- .mm_resolve_state(state, by, max_lag)

  one <- function(xi, si) .mm_terminal_state(xi, decay, max_lag, si)
  if (is.null(by)) return(one(x, state))
  idx <- split(seq_along(x), by)
  out <- lapply(names(idx), function(g) one(x[idx[[g]]], state[[g]]))
  stats::setNames(out, names(idx))
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

# Normalise the user-facing `state` argument into a per-group list (or a bare
# state object when `by` is NULL).
#' @keywords internal
#' @noRd
.mm_resolve_state <- function(state, by, max_lag, call = parent.frame()) {
  n_state <- if (is.infinite(max_lag)) 1L else max_lag - 1L
  blank <- numeric(n_state)

  coerce_one <- function(s, label, allow_null) {
    if (is.null(s)) {
      if (allow_null) return(blank)
      cli::cli_abort(c(
        "{.arg state}{label} is {.code NULL}.",
        i = "Give every group a state, or pass {.code state = 0} to \\
             cold-start every group."
      ), call = call)
    }
    if (!is.numeric(s) || anyNA(s)) {
      cli::cli_abort("{.arg state}{label} must be a non-missing numeric vector.",
                     call = call)
    }
    # A bare 0 is the documented cold-start sentinel. It is only ambiguous when
    # the kernel's state is itself a single number, and there a state of 0 and
    # a cold start mean exactly the same thing.
    if (length(s) == 1L && s == 0 && n_state != 1L) return(blank)
    if (length(s) != n_state) {
      cli::cli_abort(c(
        "{.arg state}{label} has the wrong length for this kernel.",
        i = "Expected {n_state} value{?s}, got {length(s)}.",
        i = "Use {.fn adstock_state} to produce a state of the right shape."
      ), call = call)
    }
    as.numeric(s)
  }

  if (is.null(by)) return(coerce_one(state, "", allow_null = TRUE))

  groups <- unique(by)
  if (is.null(state) || (is.numeric(state) && length(state) == 1L && state == 0)) {
    return(stats::setNames(rep(list(blank), length(groups)), groups))
  }
  if (!is.list(state) || is.null(names(state))) {
    cli::cli_abort(c(
      "{.arg state} must be a named list when {.arg by} is supplied.",
      i = "One entry per group, named after the group.",
      i = "{.fn adstock_state} returns exactly this shape when given {.arg by}."
    ), call = call)
  }
  missing_groups <- setdiff(groups, names(state))
  if (length(missing_groups) > 0) {
    cli::cli_abort(c(
      "{.arg state} is missing {length(missing_groups)} group{?s}.",
      i = "Missing: {.val {utils::head(missing_groups, 5)}}."
    ), call = call)
  }
  stats::setNames(
    lapply(groups, function(g) {
      coerce_one(state[[g]], sprintf(" for group %s", g), allow_null = FALSE)
    }),
    groups
  )
}

#' @keywords internal
#' @noRd
.mm_apply_stateful <- function(x, by, state, fun) {
  if (is.null(by)) return(fun(x, state))
  out <- numeric(length(x))
  idx <- split(seq_along(x), by)
  for (g in names(idx)) out[idx[[g]]] <- fun(x[idx[[g]]], state[[g]])
  out
}

#' @keywords internal
#' @noRd
.mm_adstock_geometric_one <- function(x, decay, max_lag, normalise, state) {
  if (is.infinite(max_lag)) {
    init <- if (length(state) == 1L) state else 0
    raw <- as.numeric(stats::filter(x, filter = decay, method = "recursive",
                                    init = init))
    return(if (normalise) raw * (1 - decay) else raw)
  }
  w <- adstock_weights(max_lag, decay, normalise = normalise)
  .mm_fir_filter(x, w, state)
}

# Causal finite-impulse-response filter with explicit leading state.
#' @keywords internal
#' @noRd
.mm_fir_filter <- function(x, w, state) {
  L <- length(w)
  n <- length(x)
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
    init <- if (length(state) == 1L) state else 0
    raw <- as.numeric(stats::filter(x, filter = decay, method = "recursive",
                                    init = init))
    return(raw[length(raw)])
  }
  if (max_lag == 1L) return(numeric(0))
  pad <- if (length(state) == max_lag - 1L) state else numeric(max_lag - 1L)
  ext <- c(pad, x)
  utils::tail(ext, max_lag - 1L)
}
