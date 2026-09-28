#' Average and marginal return by counterfactual simulation
#'
#' Computes a channel's average return and its marginal return by re-running
#' the full media transform -- carryover *and* saturation -- on a
#' counterfactually increased spend plan, rather than by differentiating the
#' saturation curve at a single point. This is the definition of marginal ROI
#' used in the Bayesian MMM literature (Jin et al., 2017) and in Google's
#' Meridian: the incremental response from a small proportional increase in
#' spend, divided by the incremental spend.
#'
#' @param spend Numeric vector of raw spend for one channel, in time order.
#' @param coefficient The channel's fitted coefficient on the transformed
#'   regressor.
#' @param adstock,saturation Lists describing the transform, exactly as for
#'   [media_transform()]. They must match the transform the model was fitted
#'   on.
#' @param by Optional grouping vector, as for [media_transform()].
#' @param lift Proportional increase in spend used for the marginal return.
#'   The default `0.01` asks what a 1% larger budget would have returned.
#' @param rows Optional logical or integer index of the periods whose spend is
#'   increased -- the planning window. Defaults to every period. Response is
#'   always summed over the whole series, so carryover from the window into
#'   later periods is counted.
#' @param extend Number of zero-spend periods appended to the end of the series
#'   (to each group, when `by` is supplied) before summing response, so that
#'   carryover from late spend is allowed to play out. The default `0` matches
#'   [contributions()] and [roi()], which only count response inside the
#'   observed window; set it to [effective_window()] of the decay to measure
#'   long-run return.
#'
#' @return A one-row data frame with columns `spend` (total spend over the
#'   series), `contribution` (the channel's total response), `roi` (their
#'   ratio, the average return) and `mroi` (incremental response per unit of
#'   incremental spend, when the spend in `rows` is increased by `lift`).
#'
#' @details
#' Why not just differentiate the saturation curve? [mroi()] does that, and it
#' answers a narrower question: the slope of the curve at one level of
#' *transformed* media. Turning it into a return on *spend* needs two further
#' steps that are easy to get wrong.
#'
#' * **Carryover.** A unit spent this week enters this week's adstock with the
#'   kernel's first weight only -- `1 - decay` for a normalised geometric
#'   kernel -- but it also enters every later week's adstock. Under a
#'   normalised kernel those weights sum to one, so the *total* marginal return
#'   is close to the curve's slope, not to the slope times `1 - decay`.
#'   Multiplying by the first weight gives the return inside the week of spend
#'   and ignores the rest; for a slow channel that understates its marginal
#'   return several-fold, and a reallocation driven by it moves money away from
#'   exactly the channels whose effect arrives late.
#' * **Averaging over periods.** The curve's slope at the *mean* adstocked level
#'   is not the mean of its slope across periods. Flighted media spends some
#'   weeks near zero and some near saturation, and for an S-shaped curve the
#'   two can differ by a large factor (Jensen's inequality).
#'
#' Simulation handles both by construction, for any kernel and any curve
#' [media_transform()] supports, and it is what makes average and marginal
#' return directly comparable: both are totals over the same periods.
#'
#' @section What this is not:
#' The answer is only as good as the fitted curve, and a curve is identified
#' only over the spend levels the data contains. A marginal return is a local
#' quantity -- a 1% change is the kind of question the data can speak to; a
#' 50% change is extrapolation, which is why `lift` defaults to 0.01.
#'
#' @references
#' Jin, Y., Wang, Y., Sun, Y., Chan, D. and Koehler, J. (2017). Bayesian
#' methods for media mix modeling with carryover and shape effects. Google
#' Inc. <https://research.google/pubs/pub46001/>
#'
#' @seealso [roi()], [mroi()], [response_curve()], [media_transform()]
#'
#' @examples
#' data(mm_weekly)
#' north <- mm_weekly[mm_weekly$geo == "north", ]
#' truth <- attr(mm_weekly, "truth")
#'
#' # Television: long carryover, S-shaped response
#' marginal_roi(
#'   north$tv, coefficient = truth$beta[["tv"]],
#'   adstock = list(decay = truth$decay[["tv"]]),
#'   saturation = list(half_max = truth$half_max[["tv"]],
#'                     shape = truth$shape[["tv"]])
#' )
#'
#' # Letting the carryover from the final weeks play out
#' marginal_roi(
#'   north$tv, coefficient = truth$beta[["tv"]],
#'   adstock = list(decay = truth$decay[["tv"]]),
#'   saturation = list(half_max = truth$half_max[["tv"]],
#'                     shape = truth$shape[["tv"]]),
#'   extend = effective_window(truth$decay[["tv"]], 0.99)
#' )
#'
#' # Only the last quarter's budget changes
#' last_q <- seq_len(nrow(north)) > nrow(north) - 13
#' marginal_roi(
#'   north$tv, coefficient = truth$beta[["tv"]],
#'   adstock = list(decay = truth$decay[["tv"]]),
#'   saturation = list(half_max = truth$half_max[["tv"]],
#'                     shape = truth$shape[["tv"]]),
#'   rows = last_q
#' )
#' @export
marginal_roi <- function(spend, coefficient,
                         adstock = list(kernel = "geometric", decay = 0.5),
                         saturation = list(type = "none"),
                         by = NULL, lift = 0.01, rows = NULL, extend = 0L) {
  spend <- .mm_check_numeric(spend, "spend", allow_na = FALSE, finite = TRUE)
  if (any(spend < 0)) cli::cli_abort("{.arg spend} must be non-negative.")
  coefficient <- .mm_check_scalar(coefficient, "coefficient")
  lift <- .mm_check_scalar(lift, "lift", lower = 0, inclusive = c(FALSE, TRUE))
  extend <- .mm_check_count(extend, "extend", min = 0L)
  n <- length(spend)
  by <- .mm_check_by(by, n)

  in_window <- if (is.null(rows)) {
    rep(TRUE, n)
  } else if (is.logical(rows)) {
    if (length(rows) != n || anyNA(rows)) {
      cli::cli_abort("A logical {.arg rows} must have {n} non-missing values.")
    }
    rows
  } else {
    rows <- .mm_check_numeric(rows, "rows", allow_na = FALSE, finite = TRUE)
    if (any(rows %% 1 != 0) || any(rows < 1 | rows > n)) {
      cli::cli_abort("{.arg rows} must index periods 1 to {n}.")
    }
    seq_len(n) %in% rows
  }
  window_spend <- sum(spend[in_window])
  if (window_spend <= 0) {
    cli::cli_abort(c(
      "There is no spend in the window to increase.",
      i = "A proportional lift of zero spend is zero. Use {.fn mroi} at \\
           {.code spend_level = 0} to ask what the first unit would return."
    ))
  }

  # Appending zero-spend periods lets carryover from the end of the window
  # finish; with `by`, each group gets its own tail.
  pad <- function(v) {
    if (extend == 0L) return(list(x = v, by = by))
    if (is.null(by)) return(list(x = c(v, numeric(extend)), by = NULL))
    idx <- split(seq_along(v), factor(by, levels = unique(by)))
    xs <- unlist(lapply(idx, function(i) c(v[i], numeric(extend))),
                 use.names = FALSE)
    bs <- rep(names(idx), lengths(idx) + extend)
    list(x = xs, by = bs)
  }
  response <- function(v) {
    p <- pad(v)
    coefficient * sum(media_transform(p$x, adstock = adstock,
                                      saturation = saturation, by = p$by))
  }

  base <- response(spend)
  bumped <- spend
  bumped[in_window] <- bumped[in_window] * (1 + lift)
  data.frame(
    spend = sum(spend),
    contribution = base,
    roi = base / sum(spend),
    mroi = (response(bumped) - base) / (lift * window_spend),
    stringsAsFactors = FALSE
  )
}
