#' Average and marginal return by counterfactual simulation
#'
#' Computes a channel's average return and its marginal return by re-running
#' the full media transform (carryover *and* saturation) on a
#' counterfactually increased spend plan, rather than by differentiating the
#' saturation curve at a single point. This is the definition of marginal ROI
#' used in the Bayesian MMM literature (Jin et al., 2017) and in Google's
#' Meridian: the incremental response from a small proportional increase in
#' spend, divided by the incremental spend.
#'
#' @param spend Numeric vector of raw spend for one channel, in time order.
#'   Or a data frame with one row per period and a column of spend per
#'   channel, to compute every channel in one call. Spend must be complete,
#'   finite and non-negative.
#' @param coefficient The channel's fitted coefficient on the transformed
#'   regressor. When `spend` is a data frame, a numeric vector of
#'   coefficients named by channel: `coef(fit)` will do with `spec`, but
#'   without `spec` every name is taken as a channel, so pass only the
#'   channels' coefficients.
#' @param adstock,saturation Lists describing the transform, exactly as for
#'   [media_transform()]. They must match the transform the model was fitted
#'   on. When `spend` is a data frame and `spec` is `NULL`, every channel
#'   uses them.
#' @param by Optional grouping vector, as for [media_transform()], one element
#'   per element of `spend`. When `spend` is a data frame, one element or row
#'   per row, or column names of `spend`, as for [transform_plan()].
#' @param lift Proportional increase in spend used for the marginal return.
#'   The default `0.01` asks what a 1% larger budget would have returned.
#' @param rows Optional logical or integer index of the periods whose spend is
#'   increased (the planning window). Defaults to every period. Response is
#'   always summed over the whole series, so carryover from the window into
#'   later periods is counted.
#' @param extend Number of zero-spend periods appended to the end of the series
#'   (to each group, when `by` is supplied) before summing response, so that
#'   carryover from late spend is allowed to play out. At most 1,000,000. The
#'   default `0` counts response only inside the observed window, as
#'   [contributions()] and [roi()] do, so the average return matches theirs.
#'   It also drops the carryover of late spend, which understates the marginal
#'   return of slow channels; see "Carryover after the window". Set it to
#'   [effective_window()] of the decay to measure long-run return.
#' @param spec Optional list with one transform per channel, named by column,
#'   as for [transform_plan()]: each entry holds `adstock` and `saturation`,
#'   and optionally `order`. Only when `spend` is a data frame, and in place
#'   of `adstock` and `saturation`, which must then be left out. The channels
#'   are then the names of `spec`, and `coefficient` may hold other names,
#'   which are ignored. Without `spec`, the channels are the names of
#'   `coefficient`.
#'
#' @return A one-row data frame with columns `spend` (total spend over the
#'   series), `contribution` (the channel's total response), `roi` (their
#'   ratio, the average return) and `mroi` (incremental response per unit of
#'   incremental spend, when the spend in `rows` is increased by `lift`). A
#'   window with no spend is an error.
#'
#'   When `spend` is a data frame, one row per channel, in the order of
#'   `spec` or of `coefficient`, with a leading `channel` column and then the
#'   same columns. A channel with no spend in the window has an `mroi` of
#'   `NA`, and a message names it; with no spend at all, its `roi` is `NA`
#'   too.
#'
#' @details
#' Differentiating the saturation curve, as [mroi()] does, answers a narrower
#' question: the slope of the curve at one level of *adstocked* media.
#' Turning it into a return on *spend* needs two further steps that are easy
#' to get wrong.
#'
#' * **Carryover.** A unit spent this week enters this week's adstock with the
#'   kernel's first weight only (`1 - decay` for a normalised geometric
#'   kernel), but it also enters every later week's adstock. Under a
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
#' @section Carryover after the window:
#' With the default `extend = 0`, response that spend would produce after the
#' last observed period is not counted. The loss falls on spend near the end
#' of the window and grows with the decay. For `mm_weekly`'s north television
#' (decay 0.85) at its true parameters, the marginal return with `extend = 0`
#' is 1.084, about 4% below the 1.129 it reaches with
#' `extend = effective_window(0.85, 0.99)`; for search (decay 0.15) the
#' difference is under 0.5%.
#'
#' The default makes the average return here equal the one [roi()] reports
#' from [contributions()]. For a budget decision, where each unit of spend
#' should be credited with all the response it produces, pass
#' `extend = effective_window(decay, 0.99)`.
#'
#' @section What this is not:
#' The answer is only as good as the fitted curve, and a curve is identified
#' only over the spend levels the data contains. A marginal return is a local
#' quantity: a 1% change is the kind of question the data can speak to; a
#' 50% change is extrapolation, which is why `lift` defaults to 0.01.
#'
#' @references
#' Jin, Y., Wang, Y., Sun, Y., Chan, D. and Koehler, J. (2017). Bayesian
#' methods for media mix modeling with carryover and shape effects. Google
#' Inc. <https://research.google/pubs/pub46001/>
#'
#' @seealso [roi()], [mroi()], [response_curve()], [media_transform()],
#'   [simulate_plan()], [allocate_budget()]
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
#'
#' # Every channel in one call, each with its own transform
#' channels <- c("tv", "video", "search", "social", "display")
#' spec <- lapply(stats::setNames(channels, channels), function(ch) list(
#'   adstock = list(decay = truth$decay[[ch]]),
#'   saturation = list(half_max = truth$half_max[[ch]],
#'                     shape = truth$shape[[ch]])
#' ))
#' marginal_roi(north, truth$beta, spec = spec)
#' @export
marginal_roi <- function(spend, coefficient,
                         adstock = list(kernel = "geometric", decay = 0.5),
                         saturation = list(type = "none"),
                         by = NULL, lift = 0.01, rows = NULL, extend = 0L,
                         spec = NULL) {
  # `adstock` may carry `by`, as in media_transform(). Moving it to `by` here
  # lets the padding for `extend` and the window see the groups too.
  if (is.list(adstock) && "by" %in% names(adstock)) {
    if (!is.null(by)) {
      cli::cli_abort(c(
        "{.arg by} is given both as an argument and inside {.arg adstock}.",
        i = "Give it once."
      ))
    }
    by <- adstock$by
    adstock$by <- NULL
  }
  if (is.data.frame(spend)) {
    return(.mm_marginal_roi_table(
      spend, coefficient, adstock, saturation, by, lift, rows, extend, spec,
      shared = !missing(adstock) || !missing(saturation)
    ))
  }
  if (!is.null(spec)) {
    cli::cli_abort(c(
      "{.arg spec} needs {.arg spend} as a data frame with a column per \\
       channel.",
      i = "For one channel, describe its transform with {.arg adstock} and \\
           {.arg saturation}."
    ))
  }
  spend <- .mm_check_numeric(spend, "spend", allow_na = FALSE, finite = TRUE)
  if (any(spend < 0)) cli::cli_abort("{.arg spend} must be non-negative.")
  coefficient <- .mm_check_scalar(coefficient, "coefficient")
  lift <- .mm_check_scalar(lift, "lift", lower = 0, inclusive = c(FALSE, TRUE))
  extend <- .mm_check_bounded_count(extend, "extend", min = 0L, max = 1e6)
  n <- length(spend)
  if (!is.null(by)) {
    len <- if (is.data.frame(by)) nrow(by) else length(by)
    if (len != n) {
      cli::cli_abort("{.arg by} must have one element per element of \\
                      {.arg spend} ({n}), not {len}.")
    }
  }
  by <- .mm_check_by(by, n)
  in_window <- .mm_window_rows(rows, n)
  if (sum(spend[in_window]) <= 0) {
    cli::cli_abort(c(
      "There is no spend in the window to increase.",
      i = "A proportional lift of zero spend is zero. Use {.fn mroi} at \\
           {.code spend_level = 0} to ask what the first unit would return."
    ))
  }
  transform <- function(x, by) {
    media_transform(x, adstock = adstock, saturation = saturation, by = by)
  }
  .mm_mroi_row(spend, coefficient, transform, by, lift, in_window, extend)
}

# The periods whose spend is increased, as a logical vector over `n` periods.
#' @keywords internal
#' @noRd
.mm_window_rows <- function(rows, n, call = parent.frame()) {
  if (is.null(rows)) return(rep(TRUE, n))
  if (is.logical(rows)) {
    if (length(rows) != n || anyNA(rows)) {
      cli::cli_abort("A logical {.arg rows} must have {n} non-missing values.",
                     call = call)
    }
    return(rows)
  }
  rows <- .mm_check_numeric(rows, "rows", allow_na = FALSE, finite = TRUE,
                            call = call)
  if (any(rows %% 1 != 0) || any(rows < 1 | rows > n)) {
    cli::cli_abort("{.arg rows} must index periods 1 to {n}.", call = call)
  }
  seq_len(n) %in% rows
}

# Average and marginal return of one channel, whose transform is
# `transform(x, by)`. Appending zero-spend periods lets carryover from the end
# of the window finish; with `by`, each group gets its own tail. The marginal
# return is NA when the window holds no spend.
#' @keywords internal
#' @noRd
.mm_mroi_row <- function(spend, coefficient, transform, by, lift, in_window,
                         extend) {
  total <- sum(spend)
  window_spend <- sum(spend[in_window])
  base <- .mm_total_response(spend, coefficient, transform, by, extend)
  mroi <- NA_real_
  if (window_spend > 0) {
    bumped <- spend
    bumped[in_window] <- bumped[in_window] * (1 + lift)
    mroi <- (.mm_total_response(bumped, coefficient, transform, by, extend) -
               base) / (lift * window_spend)
  }
  data.frame(
    spend = total,
    contribution = base,
    roi = if (total > 0) base / total else NA_real_,
    mroi = mroi,
    stringsAsFactors = FALSE
  )
}

# marginal_roi() for every channel of a plan, one row each. A channel is
# transformed by its entry of `spec`, or by the shared `adstock` and
# `saturation` when there is no `spec`.
#' @keywords internal
#' @noRd
.mm_marginal_roi_table <- function(spend, coefficient, adstock, saturation,
                                   by, lift, rows, extend, spec, shared,
                                   call = parent.frame()) {
  .mm_check_plan(spend, "spend", call = call)
  if (!is.null(spec)) {
    if (shared) {
      cli::cli_abort(c(
        "Give the transform once: in {.arg spec}, or in {.arg adstock} and \\
         {.arg saturation}.",
        i = "{.arg spec} sets every channel's transform, so the shared \\
             arguments would be ignored."
      ), call = call)
    }
    channels <- .mm_check_spec(spec, call = call)
    source <- "spec"
  } else {
    if (!is.numeric(coefficient) || is.null(names(coefficient))) {
      cli::cli_abort(c(
        "With {.arg spend} as a data frame, {.arg coefficient} must be a \\
         numeric vector named by channel.",
        i = "Its names choose the channels; pass {.arg spec} to give each \\
             channel its own transform."
      ), call = call)
    }
    channels <- gsub("`", "", names(coefficient), fixed = TRUE)
    if (anyNA(channels) || any(!nzchar(channels)) ||
        anyDuplicated(channels) > 0L) {
      cli::cli_abort("The names of {.arg coefficient} must be distinct \\
                      channel names.", call = call)
    }
    entry <- list(adstock = adstock, saturation = saturation)
    spec <- stats::setNames(rep(list(entry), length(channels)), channels)
    source <- "coefficient"
  }
  .mm_check_channels(spend, channels, "spend", values = TRUE, source = source,
                     call = call)
  cf <- .mm_plan_coefficients(coefficient, channels, "coefficient",
                              call = call)
  lift <- .mm_check_scalar(lift, "lift", lower = 0, inclusive = c(FALSE, TRUE),
                           call = call)
  extend <- .mm_check_bounded_count(extend, "extend", min = 0L, max = 1e6,
                                    call = call)
  key <- .mm_plan_by(by, spend, channels, "spend", call = call)
  in_window <- .mm_window_rows(rows, nrow(spend), call = call)

  out <- do.call(rbind, lapply(channels, function(ch) {
    .mm_mroi_row(as.numeric(spend[[ch]]), cf[[ch]],
                 .mm_channel_transformer(spec[[ch]], ch, call), key, lift,
                 in_window, extend)
  }))
  out <- data.frame(channel = channels, out, stringsAsFactors = FALSE)
  rownames(out) <- NULL
  dark <- channels[is.na(out$mroi)]
  if (length(dark) > 0L) {
    cli::cli_inform(c(
      "{.val {dark}} {?has/have} no spend in the window, so {?its/their} \\
       {.field mroi} is {.code NA}.",
      i = "A proportional lift of zero spend is zero. Use {.fn mroi} at \\
           {.code spend_level = 0} to ask what the first unit would return."
    ))
  }
  out
}
