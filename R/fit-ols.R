#' Default linear fitter for carryover tuning
#'
#' A bare intercept-and-slope least squares fit of `y` on transformed media,
#' supplied so that [tune_carryover()] has a sensible default and the simple
#' case stays one line. It exists to be replaced: pass your own `fit_fn` and
#' `predict_fn` to tune carryover against the model you intend to fit.
#'
#' @param x For `fit_ols()`, a numeric vector of transformed (adstocked)
#'   media. For `print()`, an `mm_ols` object.
#' @param y Numeric vector of the response, the same length as `x`.
#' @param object An `mm_ols` object.
#' @param newdata Numeric vector of transformed media to predict from. When
#'   omitted, `predict()` returns the fitted values.
#' @param ... Ignored, present for generic consistency.
#'
#' @return `fit_ols()` returns an object of class `mm_ols`: a list with elements
#'   `intercept`, `slope`, `n` and `fitted.values`, the fitted values of the
#'   `n` finite pairs used. `predict()` returns a numeric vector the same
#'   length as `newdata`. `coef()` returns the intercept and slope, named
#'   `(Intercept)` and `media`, so a fit on a column named `media` works with
#'   [contributions()]. `fitted()` returns `fitted.values`.
#'
#' @details
#' This is deliberately the simplest possible model. It has no trend, no
#' seasonality, no price term and no other controls, so carryover parameters
#' chosen against it absorb whatever those omitted terms would have explained.
#' That is fine for a first pass and wrong for a deliverable.
#'
#' To tune against a model with controls, pass them to [tune_carryover()]'s
#' `controls` argument, which switches the default model to least squares on
#' the adstocked media plus the controls and keeps every row aligned with every
#' split. For several channels at once use [tune_carryover_joint()], and for
#' carryover tuned jointly with saturation and a model penalty, [step_adstock()]
#' in a \pkg{recipes} pipeline. The examples of [tune_carryover()] show the
#' first; `vignette("carryover")` and `vignette("tidymodels")` work through
#' the other two.
#'
#' Pairs where `x` or `y` is missing or non-finite are dropped before fitting.
#' With fewer than two pairs left the slope is zero and the intercept is the
#' one remaining `y`, or `NA` when none remains. Otherwise the slope is set to
#' zero when `x` has no spread relative to its own scale, so the answer does
#' not depend on the units `x` is measured in.
#'
#' @seealso [tune_carryover()], which uses `fit_ols()` and [rmse()] as its
#'   defaults.
#'
#' @examples
#' set.seed(1)
#' spend <- adstock_geometric(c(100, 50, 0, 0, 200, 100, 0, 50), decay = 0.5)
#' kpi <- 10 + 0.4 * spend + rnorm(8, sd = 0.1)
#'
#' m <- fit_ols(spend, kpi)
#' m
#' coef(m)
#' predict(m, spend)
#' @export
fit_ols <- function(x, y) {
  x <- .mm_check_numeric(x, "x")
  y <- .mm_check_numeric(y, "y")
  if (length(x) != length(y)) {
    cli::cli_abort("{.arg x} ({length(x)}) and {.arg y} ({length(y)}) must be \\
                    the same length.")
  }
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  n <- length(x)
  if (n < 2L) {
    out <- list(intercept = if (n == 1L) y else NA_real_, slope = 0, n = n)
  } else {
    # Centred cross-products. The shortcut `sum(x*x) - sum(x)^2/n` subtracts
    # two nearly equal numbers when the mean of x is large next to its spread,
    # and can lose most of the digits of the result. Centring first keeps `sxx`
    # and `sxy` accurate for the singularity test and the slope.
    xbar <- mean(x)
    ybar <- mean(y)
    dx <- x - xbar
    sxx <- sum(dx * dx)
    sxy <- sum(dx * (y - ybar))
    out <- .mm_ols_from_moments(n, sum(x), sum(y), sxx, sxy, sum(x * x))
  }
  out$fitted.values <- out$intercept + out$slope * x
  structure(out, class = "mm_ols")
}

# The one implementation of the least-squares arithmetic. Both `fit_ols()` and
# the prefix-sum fast path inside `tune_carryover()` route through this, so the
# two cannot drift apart. The singularity test is relative to the raw sum of
# squares with no floor, so it treats x measured in billionths exactly as it
# treats x measured in billions.
#' @keywords internal
#' @noRd
.mm_ols_from_moments <- function(n, sx, sy, sxx, sxy, sxx_raw) {
  if (n < 1L) return(list(intercept = NA_real_, slope = 0, n = n))
  if (n < 2L) return(list(intercept = sy / n, slope = 0, n = n))
  if (!is.finite(sxx) || sxx <= .Machine$double.eps * sxx_raw) {
    return(list(intercept = sy / n, slope = 0, n = n))
  }
  slope <- sxy / sxx
  if (!is.finite(slope)) return(list(intercept = sy / n, slope = 0, n = n))
  list(intercept = (sy - slope * sx) / n, slope = slope, n = n)
}

#' @rdname fit_ols
#' @export
predict.mm_ols <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) return(object$fitted.values)
  newdata <- .mm_check_numeric(newdata, "newdata")
  object$intercept + object$slope * newdata
}

#' @rdname fit_ols
#' @export
coef.mm_ols <- function(object, ...) {
  c(`(Intercept)` = object$intercept, media = object$slope)
}

#' @rdname fit_ols
#' @export
fitted.mm_ols <- function(object, ...) {
  object$fitted.values
}

#' @rdname fit_ols
#' @export
print.mm_ols <- function(x, ...) {
  cli::cli_text("{.cls mm_ols} fitted on {x$n} observation{?s}")
  cli::cli_text("response = {signif(x$intercept, 4)} + \\
                 {signif(x$slope, 4)} * media")
  invisible(x)
}
