#' Tuning parameters for media transforms
#'
#' \pkg{dials} parameter objects for the quantities [step_adstock()] and
#' [step_saturation()] expose to tuning. They are one reason for the
#' transforms to be recipe steps rather than functions you call beforehand:
#' they let carryover decay, saturation shape and a model's own penalty be
#' tuned *jointly*, in one `tune::tune_bayes()` call over proper rolling-origin
#' resampling, rather than fixing the transform parameters by eye and tuning
#' only the model.
#'
#' @param range A two-element vector giving the range of the parameter.
#' @param trans A transformation from \pkg{scales}, or `NULL` for none.
#'
#' @return A `dials` parameter object.
#'
#' @details
#' `carryover_decay()` spans `[0, 0.95]` rather than `[0, 1)`. The upper bound
#' is a practical one: a decay of 0.99 implies a half-life of 69 periods, which
#' no ordinary media dataset can identify, and leaving it in the range mostly
#' wastes tuning iterations on parameter values that trade off against the
#' intercept.
#'
#' `carryover_max_lag()` spans 1 to 26 periods. It is finite by necessity:
#' `Inf` is a legitimate value for [step_adstock()] but not a tunable one, since
#' a search cannot propose an infinite integer. So tuning `max_lag` over a
#' finite grid and leaving it fixed at `Inf` are genuinely different searches:
#' the first over truncated kernels, the second over the recursive one. Most of
#' the time, tuning `decay` with `max_lag = Inf` is the better use of the
#' budget: the truncation point is usually a modelling decision rather than
#' something the data speaks to.
#'
#' `carryover_shape()` and `carryover_scale()` are the shape and scale of
#' [step_adstock()]'s Weibull kernel. The shape spans `[0.5, 5]` and the
#' scale `[0.5, 10]` periods. Over those ranges the kernel keeps some weight at
#' its first lag in both forms and at every `max_lag`, so no candidate is
#' rejected as a kernel that is zero everywhere. `carryover_peak()` is the
#' delayed kernel's peak lag, from 0 to 4 periods. A peak must not pass
#' `max_lag - 1`, so [step_adstock()] stops this range there when `max_lag` is
#' fixed, and starts a tuned `max_lag` above the largest peak. For the delayed
#' kernel it also narrows `carryover_decay()` to `[0.05, 0.95]`, since that
#' kernel needs a positive retention rate.
#'
#' `saturation_half_max()` is a fraction of each column's reference level, so
#' its range is unitless and comparable across channels. See
#' [step_saturation()].
#'
#' `saturation_shape()` spans `[0.5, 3]`. Values above 1 give an S-shaped
#' response with a threshold below which media barely registers; below 1 the
#' curve bends harder than a plain hyperbola. [step_saturation()] narrows
#' this range to `[0.1, 1]` when `type = "power"`, where `shape` is the
#' exponent itself and a value above 1 would not be saturating at all. It
#' offers `saturation_shape()` only for the Hill and power curves and
#' `saturation_half_max()` only for the three curves with a half-saturation
#' point, since the others ignore them.
#'
#' @seealso [step_adstock()], [step_saturation()]
#'
#' @examplesIf requireNamespace("dials", quietly = TRUE)
#' carryover_decay()
#' dials::value_seq(carryover_decay(), 5)
#'
#' saturation_shape()
#' dials::grid_regular(carryover_decay(), saturation_shape(), levels = 3)
#'
#' # The Weibull kernel's two parameters
#' dials::grid_regular(carryover_shape(), carryover_scale(), levels = 3)
#' @name mediamix_params
NULL

#' @rdname mediamix_params
#' @export
carryover_decay <- function(range = c(0, 0.95), trans = NULL) {
  .mm_require_dials()
  dials::new_quant_param(
    type = "double", range = range, inclusive = c(TRUE, TRUE), trans = trans,
    label = c(decay = "Carryover Decay"), finalize = NULL
  )
}

#' @rdname mediamix_params
#' @export
carryover_max_lag <- function(range = c(1L, 26L), trans = NULL) {
  .mm_require_dials()
  dials::new_quant_param(
    type = "integer", range = range, inclusive = c(TRUE, TRUE), trans = trans,
    label = c(max_lag = "Maximum Carryover Lag"), finalize = NULL
  )
}

#' @rdname mediamix_params
#' @export
carryover_shape <- function(range = c(0.5, 5), trans = NULL) {
  .mm_require_dials()
  dials::new_quant_param(
    type = "double", range = range, inclusive = c(TRUE, TRUE), trans = trans,
    label = c(shape = "Weibull Carryover Shape"), finalize = NULL
  )
}

#' @rdname mediamix_params
#' @export
carryover_scale <- function(range = c(0.5, 10), trans = NULL) {
  .mm_require_dials()
  dials::new_quant_param(
    type = "double", range = range, inclusive = c(TRUE, TRUE), trans = trans,
    label = c(scale = "Weibull Carryover Scale (periods)"), finalize = NULL
  )
}

#' @rdname mediamix_params
#' @export
carryover_peak <- function(range = c(0, 4), trans = NULL) {
  .mm_require_dials()
  dials::new_quant_param(
    type = "double", range = range, inclusive = c(TRUE, TRUE), trans = trans,
    label = c(peak = "Carryover Peak Lag (periods)"), finalize = NULL
  )
}

#' @rdname mediamix_params
#' @export
saturation_half_max <- function(range = c(0.05, 1), trans = NULL) {
  .mm_require_dials()
  dials::new_quant_param(
    type = "double", range = range, inclusive = c(TRUE, TRUE), trans = trans,
    label = c(half_max = "Half-Saturation Point (fraction of reference)"),
    finalize = NULL
  )
}

#' @rdname mediamix_params
#' @export
saturation_shape <- function(range = c(0.5, 3), trans = NULL) {
  .mm_require_dials()
  dials::new_quant_param(
    type = "double", range = range, inclusive = c(TRUE, TRUE), trans = trans,
    label = c(shape = "Saturation Shape"), finalize = NULL
  )
}
