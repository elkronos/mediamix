#' Saturation curves
#'
#' Diminishing returns to media. Adstock says *when* money works; saturation
#' says *how hard* it works at the margin. Four curves are provided; they differ
#' in whether the response is bounded, and in whether they allow an S-shape.
#'
#' @param x Numeric vector of media, normally already adstocked. Values must be
#'   non-negative: all four curves are defined for spend, not for arbitrary
#'   reals.
#' @param half_max The value of `x` at which the response reaches half its
#'   ceiling. Interpretable in the units of `x`, which is why it is used here in
#'   preference to the equivalent unnamed scale parameter.
#' @param shape Hill exponent, positive. `shape = 1` gives a concave curve with
#'   diminishing returns everywhere. `shape > 1` gives an S-shape with an
#'   initial convex region, the usual representation of a threshold effect.
#' @param rate Exponential rate parameter, positive. Larger values saturate
#'   sooner.
#' @param vmax Michaelis--Menten asymptote: the response as `x` grows without
#'   bound.
#' @param km Michaelis--Menten constant: the value of `x` at which the response
#'   reaches half of `vmax`.
#' @param exponent Power exponent in `(0, 1]`. `exponent = 1` is the identity
#'   (no saturation); smaller values bend the curve harder.
#' @param type Which curve [saturate()] should dispatch to.
#' @param ... Passed to the individual curve function.
#'
#' @return A numeric vector the same length as `x`, in the same order.
#'
#' @details
#' `saturate_hill()` and `saturate_exponential()` are bounded on `[0, 1)`, so
#' the fitted coefficient carries the channel's ceiling.
#' `saturate_michaelis_menten()` is bounded by `vmax`. All three approach their
#' ceiling smoothly at arbitrarily large `x`, including `Inf`, rather than
#' overflowing.
#' `saturate_power()` is unbounded but concave: response keeps growing, just
#' ever more slowly. Unboundedness is not automatically wrong, but it does mean
#' the model will happily extrapolate a return on a spend level never observed.
#'
#' `saturate_hill(x, half_max, shape = 1)` and
#' `saturate_michaelis_menten(x, vmax = 1, km = half_max)` are the same
#' function. Both are provided because the two literatures name it differently
#' and practitioners arrive expecting one or the other.
#'
#' @section Transform order:
#' Saturation is applied *after* adstock, not before. Saturating first would
#' cap each period's spend in isolation and then let carryover accumulate the
#' capped values past the cap, which defeats the point of having a ceiling. Use
#' [media_transform()] to get the order right without having to remember it.
#'
#' @references
#' Hill, A. V. (1910). The possible effects of the aggregation of the molecules
#' of haemoglobin on its dissociation curves. *The Journal of Physiology*,
#' 40(Suppl), iv--vii.
#'
#' @seealso [media_transform()], [response_curve()]
#'
#' @examples
#' spend <- c(0, 25, 50, 100, 200, 400)
#'
#' # Concave: diminishing returns from the first pound
#' round(saturate_hill(spend, half_max = 100), 3)
#'
#' # S-shaped: a threshold below which media barely registers
#' round(saturate_hill(spend, half_max = 100, shape = 3), 3)
#'
#' # The four curves side by side. Hill, exponential and Michaelis-Menten are
#' # bounded; power keeps growing, just ever more slowly.
#' round(rbind(
#'   hill        = saturate_hill(spend, half_max = 100),
#'   exponential = saturate_exponential(spend, rate = 0.007),
#'   michaelis   = saturate_michaelis_menten(spend, vmax = 1, km = 100),
#'   power       = saturate_power(spend, exponent = 0.5)
#' ), 3)
#'
#' # Bounded means bounded: no overflow, even at the extremes
#' saturate_hill(c(1e300, Inf), half_max = 100, shape = 3)
#'
#' # The dispatcher, for programmatic use
#' round(saturate(spend, type = "hill", half_max = 100), 3)
#' @name saturation
NULL

#' @rdname saturation
#' @export
saturate_hill <- function(x, half_max, shape = 1) {
  x <- .mm_check_saturation_input(x)
  half_max <- .mm_check_scalar(half_max, "half_max", lower = 0,
                               inclusive = c(FALSE, TRUE))
  shape <- .mm_check_scalar(shape, "shape", lower = 0, inclusive = c(FALSE, TRUE))
  # Computed as 1 / (1 + (half_max/x)^shape) rather than x^s / (x^s + h^s).
  # The two are algebraically identical, but the direct form overflows to
  # Inf/Inf = NaN for large x or large shape, which would break the documented
  # [0, 1) bound exactly where the curve should be flattest.
  out <- 1 / (1 + (half_max / x)^shape)
  out[!is.na(x) & x == 0] <- 0
  out
}

#' @rdname saturation
#' @export
saturate_exponential <- function(x, rate) {
  x <- .mm_check_saturation_input(x)
  rate <- .mm_check_scalar(rate, "rate", lower = 0, inclusive = c(FALSE, TRUE))
  1 - exp(-rate * x)
}

#' @rdname saturation
#' @export
saturate_michaelis_menten <- function(x, vmax = 1, km) {
  x <- .mm_check_saturation_input(x)
  vmax <- .mm_check_scalar(vmax, "vmax", lower = 0, inclusive = c(FALSE, TRUE))
  km <- .mm_check_scalar(km, "km", lower = 0, inclusive = c(FALSE, TRUE))
  # vmax / (1 + km/x) rather than vmax * x / (km + x): identical, but finite at
  # x = Inf where the direct form gives Inf/Inf.
  out <- vmax / (1 + km / x)
  out[!is.na(x) & x == 0] <- 0
  out
}

#' @rdname saturation
#' @export
saturate_power <- function(x, exponent) {
  x <- .mm_check_saturation_input(x)
  exponent <- .mm_check_scalar(exponent, "exponent", lower = 0, upper = 1,
                               inclusive = c(FALSE, TRUE))
  x^exponent
}

#' @rdname saturation
#' @export
saturate <- function(x, type = c("hill", "exponential", "michaelis_menten", "power"),
                     ...) {
  type <- match.arg(type)
  switch(
    type,
    hill = saturate_hill(x, ...),
    exponential = saturate_exponential(x, ...),
    michaelis_menten = saturate_michaelis_menten(x, ...),
    power = saturate_power(x, ...)
  )
}

#' @keywords internal
#' @noRd
.mm_check_saturation_input <- function(x, call = parent.frame()) {
  x <- .mm_check_numeric(x, "x", call = call)
  neg <- !is.na(x) & x < 0
  if (any(neg)) {
    cli::cli_abort(c(
      "{.arg x} contains {sum(neg)} negative value{?s}.",
      i = "Saturation curves are defined for non-negative media.",
      i = "If these are residuals or index values rather than spend, saturate \\
           the underlying media instead."
    ), call = call)
  }
  x
}
