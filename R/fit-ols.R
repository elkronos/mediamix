#' Default linear fitter for carryover tuning
#'
#' A bare intercept-and-slope least squares fit of `y` on transformed media,
#' supplied so that [tune_carryover()] has a sensible default and the simple
#' case stays one line. It exists to be replaced: pass your own `fit_fn` and
#' `predict_fn` to tune carryover against the model you actually intend to fit.
#'
#' @param x For `fit_ols()`, a numeric vector of transformed (adstocked)
#'   media. For `print()`, an `mm_ols` object.
#' @param y Numeric vector of the response, the same length as `x`.
#' @param object An `mm_ols` object.
#' @param newdata Numeric vector of transformed media to predict from.
#' @param ... Ignored, present for generic consistency.
#'
#' @return `fit_ols()` returns an object of class `mm_ols`: a list with elements
#'   `intercept`, `slope` and `n`. `predict()` returns a numeric vector the same
#'   length as `newdata`.
#'
#' @details
#' This is deliberately the simplest possible model. It has no seasonality, no
#' price term, no baseline and no controls, so carryover parameters chosen
#' against it absorb whatever those omitted terms would have explained. That is
#' fine for a first pass and wrong for a deliverable.
#'
#' To tune against a model with controls, note that [tune_carryover()] hands
#' `fit_fn` only the adstocked media and the response -- there is no mechanism
#' for passing extra columns through. Two things follow: controls have to be
#' captured from the enclosing environment, and `predict_fn` has to supply them
#' for the assessment rows itself, which means knowing which rows those are.
#' Under a forward-only scheme the assessment rows are always the ones
#' immediately following the training rows, so a `predict_fn` can reconstruct
#' them from `length(newdata)` -- but it has to get that arithmetic exactly
#' right, and a silently misaligned control column produces a plausible number
#' rather than an error.
#'
#' Two safer routes exist, and one of them is almost always what you want.
#' Residualise `y` against the controls first and tune carryover on the
#' residual, which needs no row bookkeeping at all; or use [step_adstock()] in a
#' \pkg{recipes} pipeline, where the resampling machinery keeps every row
#' aligned for you and carryover is tuned jointly with everything else. Both are
#' worked through in `vignette("carryover")`.
#'
#' @seealso [tune_carryover()]
#'
#' @examples
#' set.seed(1)
#' spend <- adstock_geometric(c(100, 50, 0, 0, 200, 100, 0, 50), decay = 0.5)
#' kpi <- 10 + 0.4 * spend + rnorm(8, sd = 0.1)
#'
#' m <- fit_ols(spend, kpi)
#' m
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
    return(structure(list(intercept = if (n == 1L) y else NA_real_,
                          slope = 0, n = n), class = "mm_ols"))
  }
  # Centred cross-products. Computing these as `sum(x*x) - sum(x)^2/n` would
  # make the singularity test an absolute one on a quantity that scales with
  # x-squared, so expressing x in pounds rather than thousands of pounds could
  # flip a real slope to zero. Centring first keeps the test relative to the
  # actual spread of x.
  xbar <- mean(x)
  ybar <- mean(y)
  dx <- x - xbar
  sxx <- sum(dx * dx)
  sxy <- sum(dx * (y - ybar))
  structure(.mm_ols_from_moments(n, sum(x), sum(y), sxx, sxy, sum(x * x)),
            class = "mm_ols")
}

# The one implementation of the least-squares arithmetic. Both `fit_ols()` and
# the prefix-sum fast path inside `tune_carryover()` route through this, so the
# two cannot drift apart.
#' @keywords internal
#' @noRd
.mm_ols_from_moments <- function(n, sx, sy, sxx, sxy, sxx_raw) {
  if (n < 1L) return(list(intercept = NA_real_, slope = 0, n = n))
  if (n < 2L) return(list(intercept = sy / n, slope = 0, n = n))
  if (!is.finite(sxx) || sxx <= .Machine$double.eps * max(sxx_raw, 1)) {
    return(list(intercept = sy / n, slope = 0, n = n))
  }
  slope <- sxy / sxx
  if (!is.finite(slope)) return(list(intercept = sy / n, slope = 0, n = n))
  list(intercept = (sy - slope * sx) / n, slope = slope, n = n)
}

#' @rdname fit_ols
#' @export
predict.mm_ols <- function(object, newdata, ...) {
  newdata <- .mm_check_numeric(newdata, "newdata")
  object$intercept + object$slope * newdata
}

#' @rdname fit_ols
#' @export
print.mm_ols <- function(x, ...) {
  cli::cli_text("{.cls mm_ols} fitted on {x$n} observation{?s}")
  cli::cli_text("response = {round(x$intercept, 4)} + \\
                 {round(x$slope, 4)} * media")
  invisible(x)
}
