#' Convert Robyn hyperparameters to mediamix arguments
#'
#' Translates one channel's adstock and saturation hyperparameters from a
#' Robyn model into the `adstock` and `saturation` lists that
#' [media_transform()] takes, so that mediamix reproduces the series Robyn
#' fitted its model on. Geometric adstock and Robyn's Weibull CDF convert
#' exactly. Hill `alpha` and `gamma` convert exactly given the media series,
#' because Robyn places the inflexion point relative to the adstocked series.
#'
#' @param x The media series Robyn transformed for this channel: its column of
#'   `InputCollect$dt_mod`, every row, in date order. Values must be finite,
#'   non-missing and non-negative. Needed to convert `gamma` and Weibull
#'   adstock; a geometric `theta` converts without it.
#' @param adstock Robyn's adstock type, as in `InputCollect$adstock`: one of
#'   `"geometric"`, `"weibull_cdf"` or `"weibull_pdf"`. `"weibull_pdf"` has no
#'   exact equivalent and is refused; see *Details*.
#' @param theta Robyn's geometric decay rate (the `_thetas` hyperparameter),
#'   in `[0, 1)`. Required for geometric adstock.
#' @param shape,scale Robyn's Weibull shape and scale (the `_shapes` and
#'   `_scales` hyperparameters), with `shape >= 0` and `scale` in `[0, 1]`.
#'   Required for Weibull adstock. These are adstock parameters: the Hill
#'   shape is `alpha`.
#' @param alpha,gamma Robyn's Hill exponent and inflexion share (the `_alphas`
#'   and `_gammas` hyperparameters), with `alpha > 0` and `gamma` in `(0, 1]`.
#'   Give both or neither. Without them the saturation step is `"none"`.
#' @param window Row positions of `x` inside Robyn's modelling window,
#'   `InputCollect$rollingWindowStartWhich:InputCollect$rollingWindowEndWhich`.
#'   The default `NULL` uses every row. Used only to convert `gamma`.
#' @param inflexion How the Robyn version that fitted the model placed the
#'   Hill inflexion point: `"max"` (the default) for Robyn 3.12.0 and later,
#'   `"range"` for Robyn 3.7.0 to 3.11.1. Used only to convert `gamma`.
#'
#' @return A list with two elements, `adstock` and `saturation`, to pass to
#'   the arguments of the same names in [media_transform()]:
#'   \describe{
#'     \item{`adstock`}{`list(kernel = "geometric", decay, normalise = FALSE)`
#'       for geometric adstock; `list(kernel = "weibull", shape, scale,
#'       max_lag, type = "cdf", normalise = FALSE)` for the Weibull CDF, with
#'       `scale` in periods and `max_lag` equal to `length(x)`; or
#'       `list(kernel = "none")` when the Weibull `shape` or `scale` is 0.}
#'     \item{`saturation`}{`list(type = "hill", half_max, shape)`, where
#'       `half_max` is Robyn's inflexion point in the units of the adstocked
#'       media and `shape` is `alpha`; or `list(type = "none")` when `alpha`
#'       and `gamma` are not given.}
#'   }
#'
#' @details
#' Each conversion follows Robyn's transformation code, in the file
#' `R/R/transformation.R` of its repository.
#'
#' **Geometric.** Robyn accumulates \eqn{a_t = x_t + \theta a_{t-1}} from
#' \eqn{a_1 = x_1}, the unnormalised recursion. That is [adstock_geometric()]
#' with `decay = theta` and `normalise = FALSE`.
#'
#' **Weibull CDF.** Robyn converts `scale` to periods as
#' `round(quantile(1:n, scale))`, where `n` is the length of the series it
#' adstocks, and uses a kernel of `n` lags whose first weight is 1. That is
#' [adstock_weibull()] with `type = "cdf"`, `max_lag = n`, the rounded scale
#' and `normalise = FALSE`. The number of periods depends on `n`, which is why
#' the conversion needs `x`: the same `scale` fitted on a longer series means a
#' longer carryover. From Robyn 3.10.0, a `shape` or `scale` of 0 switches the
#' carryover off, and the conversion returns `kernel = "none"`.
#'
#' **Weibull PDF.** Robyn rescales the Weibull density over its `n` lags so
#' that the smallest weight is 0 and the largest is 1. [adstock_weibull()]
#' divides by the largest weight and keeps the smallest above 0, so no
#' `adstock_weibull()` arguments reproduce Robyn's series, and `from_robyn()`
#' refuses rather than return an approximation. The gap grows with the ratio
#' of the smallest density value to the largest: it is negligible when the
#' density dies out well inside the series, and not when the tail is heavy
#' (`shape` below 1) or `scale` is long. [adstock_filter()] applies any kernel
#' given as weights, Robyn's included.
#'
#' **Hill.** Robyn's curve is \eqn{a^\alpha / (a^\alpha + g^\alpha)}, which is
#' [saturate_hill()] with `shape = alpha` and `half_max = g`. Robyn computes
#' the inflexion point \eqn{g} from the adstocked series `a` over the rows of
#' the modelling window: `gamma * max(a)` from Robyn 3.12.0, and
#' `(1 - gamma) * min(a) + gamma * max(a)` in Robyn 3.7.0 to 3.11.1. The two
#' agree when the adstocked series reaches 0 inside the window. Set
#' `inflexion` to match the version that fitted the model.
#'
#' Robyn adstocks every row and saturates the rows of the modelling window, so
#' with the converted arguments `media_transform(x, ...)[window]` equals
#' Robyn's saturated series. Once converted, `half_max` is a fixed amount of
#' adstocked media, so the curve does not move when the converted arguments
#' are applied to other data.
#'
#' @references
#' Robyn, Meta's open-source marketing mix modelling package, MIT licence.
#' <https://github.com/facebookexperimental/Robyn>. The conversions follow
#' `R/R/transformation.R` on its main branch at commit `5915aa9` (the file
#' last changed in commit `33baf19`, June 2025), and release 3.11.1 of the
#' same file for the `"range"` inflexion point.
#'
#' @seealso [media_transform()], which takes the result; [adstock_geometric()],
#'   [adstock_weibull()] and [saturate_hill()] for the individual steps.
#'
#' @examples
#' spend <- c(0, 120, 300, 80, 0, 0, 450, 200, 60, 0, 0, 0)
#'
#' # Geometric adstock with Hill saturation
#' args <- from_robyn(spend, theta = 0.3, alpha = 2, gamma = 0.5)
#' str(args)
#' media_transform(spend, adstock = args$adstock,
#'                 saturation = args$saturation)
#'
#' # A geometric decay converts without the series
#' str(from_robyn(theta = 0.3)$adstock)
#'
#' # Weibull CDF: a scale of 0.1 on a 12-period series is 2 periods
#' str(from_robyn(spend, adstock = "weibull_cdf", shape = 1.5,
#'                scale = 0.1)$adstock)
#'
#' # A model fitted by Robyn 3.11.1 with a window of rows 3 to 12
#' str(from_robyn(spend, theta = 0.3, alpha = 2, gamma = 0.5, window = 3:12,
#'                inflexion = "range")$saturation)
#' @export
from_robyn <- function(x = NULL,
                       adstock = c("geometric", "weibull_cdf", "weibull_pdf"),
                       theta = NULL, shape = NULL, scale = NULL,
                       alpha = NULL, gamma = NULL, window = NULL,
                       inflexion = c("max", "range")) {
  adstock <- match.arg(adstock)
  inflexion <- match.arg(inflexion)
  if (adstock == "weibull_pdf") {
    cli::cli_abort(c(
      "Robyn's {.val weibull_pdf} adstock has no exact mediamix equivalent.",
      i = "Robyn rescales the Weibull density so that its smallest weight is \\
           0 and its largest 1; {.fn adstock_weibull} divides by the largest \\
           weight only.",
      i = "{.fn adstock_filter} applies any kernel given as weights."
    ))
  }

  if (adstock == "geometric") {
    if (!is.null(shape) || !is.null(scale)) {
      cli::cli_abort(c(
        "{.arg shape} and {.arg scale} are Weibull adstock parameters, and \\
         {.arg adstock} is {.val geometric}.",
        i = "Set {.arg adstock} to {.val weibull_cdf} to convert them, or \\
             give the Hill exponent as {.arg alpha}."
      ))
    }
    if (is.null(theta)) {
      cli::cli_abort("Geometric adstock needs {.arg theta}.")
    }
    theta <- .mm_check_scalar(theta, "theta", lower = 0, upper = 1,
                              inclusive = c(TRUE, FALSE))
  } else {
    if (!is.null(theta)) {
      cli::cli_abort(c(
        "{.arg theta} is a geometric adstock parameter, and {.arg adstock} \\
         is {.val {adstock}}.",
        i = "Set {.arg adstock} to {.val geometric} to convert it."
      ))
    }
    if (is.null(shape) || is.null(scale)) {
      cli::cli_abort(
        "Weibull adstock needs both {.arg shape} and {.arg scale}."
      )
    }
    shape <- .mm_check_scalar(shape, "shape", lower = 0)
    scale <- .mm_check_scalar(scale, "scale", lower = 0, upper = 1)
  }

  hill <- !is.null(alpha) || !is.null(gamma)
  if (hill) {
    if (is.null(alpha) || is.null(gamma)) {
      cli::cli_abort(c(
        "Give {.arg alpha} and {.arg gamma} together.",
        i = "Robyn's Hill curve needs both. Leave both out to convert the \\
             adstock alone."
      ))
    }
    alpha <- .mm_check_scalar(alpha, "alpha", lower = 0,
                              inclusive = c(FALSE, TRUE))
    gamma <- .mm_check_scalar(gamma, "gamma", lower = 0, upper = 1,
                              inclusive = c(FALSE, TRUE))
  }

  if (is.null(x)) {
    if (adstock == "weibull_cdf") {
      cli::cli_abort(c(
        "{.arg x} is needed to convert a Weibull {.arg scale}.",
        i = "Robyn measures {.arg scale} as a share of the series length, so \\
             the number of periods depends on {.code length(x)}."
      ))
    }
    if (hill) {
      cli::cli_abort(c(
        "{.arg x} is needed to convert {.arg gamma}.",
        i = "Robyn places the inflexion point relative to the adstocked \\
             series, so {.arg half_max} depends on the media."
      ))
    }
  } else {
    x <- .mm_check_numeric(x, "x", allow_na = FALSE, finite = TRUE)
    if (any(x < 0)) {
      cli::cli_abort(c(
        "{.arg x} contains {sum(x < 0)} negative value{?s}.",
        i = "Robyn and the Hill curve both need non-negative media."
      ))
    }
  }

  ad <- if (adstock == "geometric") {
    list(kernel = "geometric", decay = theta, normalise = FALSE)
  } else if (shape == 0 || scale == 0) {
    list(kernel = "none")
  } else {
    n <- length(x)
    periods <- round(stats::quantile(seq_len(n), probs = scale,
                                     names = FALSE), 0)
    list(kernel = "weibull", shape = shape, scale = periods, max_lag = n,
         type = "cdf", normalise = FALSE)
  }

  sat <- list(type = "none")
  if (hill) {
    rows <- .mm_check_window(window, length(x))
    a <- media_transform(x, adstock = ad, saturation = NULL)[rows]
    top <- max(a)
    g <- if (inflexion == "max") {
      top * gamma
    } else {
      (1 - gamma) * min(a) + gamma * top
    }
    if (!(g > 0)) {
      cli::cli_abort(c(
        "Robyn's inflexion point is 0 here, so its Hill curve is undefined.",
        x = "The adstocked media is 0 on every row of the modelling window."
      ))
    }
    sat <- list(type = "hill", half_max = g, shape = alpha)
  }
  list(adstock = ad, saturation = sat)
}

# Row positions inside a series of length `n`; NULL means every row.
#' @keywords internal
#' @noRd
.mm_check_window <- function(window, n, call = parent.frame()) {
  if (is.null(window)) return(seq_len(n))
  ok <- is.numeric(window) && !is.matrix(window) && length(window) > 0L &&
    !anyNA(window) && all(is.finite(window)) && all(window %% 1 == 0) &&
    all(window >= 1 & window <= n)
  if (!ok) {
    cli::cli_abort(c(
      "{.arg window} must hold row positions of {.arg x}: whole numbers from \\
       1 to {n}.",
      i = "Robyn's window runs from {.code rollingWindowStartWhich} to \\
           {.code rollingWindowEndWhich} in {.code InputCollect}."
    ), call = call)
  }
  as.integer(window)
}
