#' Decompose predicted KPI into channel contributions
#'
#' Splits a fitted model's prediction into a per-channel contribution plus a
#' baseline. This is what a marketing mix model deliverable actually is: not the
#' coefficients, but the answer to "how much of last year's revenue did each
#' channel produce, and what would have happened anyway".
#'
#' @param media A data frame of the *transformed* media regressors -- adstocked
#'   and saturated -- exactly as they entered the model. Column names must match
#'   the model's coefficient names.
#' @param model A fitted model, or a named numeric vector of coefficients.
#'   Anything [stats::coef()] works on will do.
#' @param intercept Model intercept. Taken from `model` when it has one; supply
#'   it explicitly when passing a bare coefficient vector without an
#'   `"(Intercept)"` element.
#' @param index Optional vector, the same length as `nrow(media)`, labelling the
#'   periods. Used as the `period` column of the result.
#' @param by Optional grouping vector, the same length as `nrow(media)`.
#'
#' @return A data frame in long format with columns `period`, `group` (present
#'   only when `by` is supplied), `channel` and `contribution`. The baseline
#'   appears as a channel named `"(baseline)"`.
#'
#'   When `model` is a fitted object that [stats::fitted()] understands,
#'   contributions sum exactly to the model's fitted value in every period,
#'   because the baseline is computed as the fitted value minus the media
#'   effect. When `model` is a bare coefficient vector there are no fitted
#'   values to work from, so the baseline is just the intercept and
#'   contributions sum to the media effect plus that intercept -- the function
#'   says so when it happens.
#'
#' @details
#' Contributions are computed as \eqn{\beta_j x_{jt}} for each channel, with
#' everything else -- intercept, trend, price, seasonality, controls -- pooled
#' into `"(baseline)"`. That pooling is a deliberate simplification: it is what
#' makes contributions sum exactly to the fitted value, and it means the
#' baseline is not "organic demand" but "everything this decomposition does not
#' attribute to media". Reporting it as organic demand is the single most common
#' way a marketing mix deliverable overstates its own precision.
#'
#' Contributions inherit whatever the model's identification is worth. If two
#' channels are collinear the split between them is arbitrary even when the
#' total is well estimated, and if the model dropped a coefficient outright
#' this function refuses rather than reporting `NA` contributions that would
#' quietly propagate into an ROI table.
#'
#' On panel data, pass one geography at a time or supply `by`. `index` is used
#' as a label, not a key: with `by` supplied and one date per geography, a
#' `tapply()` over `period` alone silently adds the geographies together.
#'
#' Contributions inherit whatever the model's identification is worth. If two
#' channels are collinear, the split between them is arbitrary even when the
#' total is well estimated -- run [diagnose_media()] before presenting any of
#' this.
#'
#' @seealso [roi()], [response_curve()], [diagnose_media()]
#'
#' @examples
#' data(mm_weekly)
#' north <- mm_weekly[mm_weekly$geo == "north", ]
#' channels <- c("tv", "video", "search", "social", "display")
#'
#' # Transform with the parameters the data was generated from, so the example
#' # shows a correctly specified decomposition rather than a misspecified one.
#' truth <- attr(mm_weekly, "truth")
#' transformed <- as.data.frame(Map(
#'   function(x, d, h, sh) media_transform(
#'     x, adstock = list(decay = d),
#'     saturation = list(half_max = h, shape = sh)),
#'   north[channels], truth$decay[channels], truth$half_max[channels],
#'   truth$shape[channels]
#' ))
#'
#' # Include the controls the data was generated with. Without them, the media
#' # coefficients absorb price, trend and seasonality and two of them flip sign.
#' model_data <- transformed
#' model_data$week <- seq_len(nrow(model_data))
#' model_data$price <- north$price
#' model_data$seasonality <- north$seasonality
#' model_data$holiday <- north$holiday
#'
#' fit <- stats::lm(north$revenue ~ ., data = model_data)
#' contrib <- contributions(transformed, fit, index = north$date)
#' head(contrib)
#'
#' # Contributions sum exactly to the model's fitted value in every period
#' totals <- as.numeric(tapply(contrib$contribution, contrib$period, sum))
#' all.equal(totals, unname(stats::fitted(fit)))
#'
#' # Channel totals over the three years. "(baseline)" is everything the model
#' # predicts that is not attributed to these five columns -- trend, price,
#' # seasonality and the intercept. It is not organic demand.
#' round(tapply(contrib$contribution, contrib$channel, sum))
#' @export
contributions <- function(media, model, intercept = NULL, index = NULL,
                          by = NULL) {
  if (!is.data.frame(media)) {
    if (is.matrix(media)) media <- as.data.frame(media) else
      cli::cli_abort("{.arg media} must be a data frame or matrix of \\
                      transformed regressors.")
  }
  cf <- .mm_coefficients(model)
  if (is.null(intercept)) {
    intercept <- if ("(Intercept)" %in% names(cf)) unname(cf[["(Intercept)"]]) else 0
  }
  intercept <- .mm_check_scalar(intercept, "intercept")
  cf <- cf[setdiff(names(cf), "(Intercept)")]

  known <- intersect(names(media), names(cf))
  if (length(known) == 0L) {
    cli::cli_abort(c(
      "No column of {.arg media} matches a model coefficient.",
      i = "Media columns: {.val {utils::head(names(media), 8)}}.",
      i = "Coefficients: {.val {utils::head(names(cf), 8)}}.",
      i = "Pass the transformed regressors under the names the model was \\
           fitted with."
    ))
  }
  aliased <- known[!is.finite(cf[known])]
  if (length(aliased) > 0L) {
    cli::cli_abort(c(
      "{length(aliased)} media coefficient{?s} {?is/are} missing.",
      x = "Affected: {.val {aliased}}.",
      i = "A model drops a coefficient when its column is an exact linear \\
           combination of the others, so that channel's effect is not \\
           separately identified.",
      i = "Run {.fn diagnose_media} on the untransformed spend to find the \\
           collinear pair."
    ))
  }
  unmatched <- setdiff(names(cf), names(media))
  n <- nrow(media)
  if (is.null(index)) index <- seq_len(n)
  if (length(index) != n) {
    cli::cli_abort("{.arg index} must have {n} element{?s}, not {length(index)}.")
  }
  by <- .mm_check_by(by, n)

  media_effect <- rowSums(vapply(known, function(nm) {
    unname(cf[[nm]]) * .mm_as_number(media[[nm]], nm)
  }, numeric(n)))

  # The baseline is everything the model predicts that this decomposition does
  # not attribute to the supplied media. Taking it as (fitted - media effect)
  # rather than as the bare intercept is what makes the contributions actually
  # sum to the fitted value, which is the property the whole table is read for.
  fit <- suppressWarnings(try(stats::fitted(model), silent = TRUE))
  exact <- !inherits(fit, "try-error") && is.numeric(fit) && length(fit) == n
  if (exact) {
    other <- as.numeric(fit) - media_effect
  } else {
    other <- rep(intercept, n)
    if (length(unmatched) > 0L) {
      cli::cli_inform(c(
        "Contributions sum to the media effect plus the intercept, not to the \\
         model's fitted value.",
        i = "{length(unmatched)} model term{?s} {?is/are} not among the media \\
             columns: {.val {utils::head(unmatched, 6)}}.",
        i = "Pass a fitted model rather than a coefficient vector, and those \\
             terms are pooled into {.val (baseline)} instead."
      ))
    }
  }

  parts <- lapply(known, function(nm) {
    data.frame(period = index, channel = nm,
               contribution = unname(cf[[nm]]) * .mm_as_number(media[[nm]], nm),
               stringsAsFactors = FALSE)
  })
  parts[[length(parts) + 1L]] <- data.frame(
    period = index, channel = "(baseline)", contribution = other,
    stringsAsFactors = FALSE
  )
  out <- do.call(rbind, parts)
  if (!is.null(by)) out$group <- rep(by, length(parts))
  front <- c("period", if (!is.null(by)) "group", "channel", "contribution")
  out <- out[, c(front, setdiff(names(out), front)), drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Return on investment and marginal return
#'
#' `roi()` divides each channel's total contribution by its total spend.
#' `mroi()` asks the different and more useful question of what the *next*
#' currency unit would return at current spend.
#'
#' @param contributions A data frame from [contributions()], or a named numeric
#'   vector of total contribution per channel.
#' @param spend A data frame of raw (untransformed) spend with one column per
#'   channel, or a named numeric vector of total spend per channel.
#' @param spend_level Current spend level for the channel, in the same units the
#'   saturation curve was fitted on.
#' @param coefficient The channel's fitted coefficient.
#' @param delta Increment used for the numerical derivative, as a fraction of
#'   `spend_level`.
#' @param ... Passed to [saturate()], for example `half_max` and `shape`.
#' @param type Saturation curve type.
#'
#' @return `roi()` returns a data frame with columns `channel`, `contribution`,
#'   `spend` and `roi`, ordered by descending ROI, with `roi` set to `NA` where
#'   spend is zero. Channels appearing in only one of the two inputs are
#'   omitted, and reported when that happens. `mroi()` returns a single number.
#'
#' @details
#' Average ROI and marginal ROI answer different questions and are routinely
#' confused. Average ROI is a scorecard: what did this channel return over the
#' period as a whole. Marginal ROI is the decision variable: what would the next
#' unit return. Only the second one should drive a reallocation, and the two can
#' point in opposite directions.
#'
#' Where they sit relative to each other depends on the shape of the curve, and
#' it is worth being precise because the usual summary of this is wrong.
#'
#' For a **concave** response -- `saturate_hill()` with `shape = 1`,
#' Michaelis--Menten, negative exponential, `saturate_power()` -- marginal ROI
#' is below average ROI everywhere, and the gap widens the further up the curve
#' a channel sits. Reallocating on average ROI then systematically over-funds
#' channels that are already saturated, which are exactly the channels that look
#' best on an average-ROI table.
#'
#' For an **S-shaped** response -- `saturate_hill()` with `shape > 1`, the usual
#' way to represent a threshold effect -- the curve is *convex* below its
#' inflection point, and there marginal ROI is **above** average ROI. A channel
#' in that region is under-funded: each extra pound works harder than the pounds
#' already spent, because the channel has not yet reached the pressure at which
#' it starts to pay. Ruling that out by assuming marginal is always lower is how
#' a threshold channel stays starved.
#'
#' `mroi()` differentiates the fitted response curve numerically, so it works
#' for any curve [saturate()] supports, and it is finite and correct at
#' `spend_level = 0`.
#'
#' @section Marginal return when the regressor was adstocked:
#' `mroi()` differentiates the saturation curve with respect to the quantity the
#' coefficient multiplies -- the *transformed* media. Two consequences follow,
#' and missing either of them produces a number that looks right and is not.
#'
#' Evaluate it at the adstocked level, not at raw spend. `mean(spend)` and
#' `mean(adstock_geometric(spend, decay))` are different numbers and sit at
#' different points on the curve.
#'
#' Then apply the chain rule. A unit of spend in the current period contributes
#' only the kernel's first weight to the current period's adstock, so the
#' marginal return on *this period's spend* is `mroi()` multiplied by that
#' weight. For a normalised geometric kernel it is `1 - decay`; for any other
#' kernel it is `adstock_weights(...)[1]`. A channel with a long carryover
#' therefore has a much smaller immediate marginal return than its saturation
#' curve alone suggests -- the rest of the effect arrives in later periods.
#'
#' ```
#' z <- adstock_geometric(spend, decay = 0.85)
#' mroi(mean(z), coefficient = beta, half_max = h, shape = s) * (1 - 0.85)
#' ```
#'
#' @seealso [contributions()], [response_curve()], [spend_for()]
#'
#' @examples
#' data(mm_weekly)
#' north <- mm_weekly[mm_weekly$geo == "north", ]
#' channels <- c("tv", "video", "search", "social", "display")
#' truth <- attr(mm_weekly, "truth")
#'
#' transformed <- as.data.frame(Map(
#'   function(x, d, h, sh) media_transform(
#'     x, adstock = list(decay = d),
#'     saturation = list(half_max = h, shape = sh)),
#'   north[channels], truth$decay[channels], truth$half_max[channels],
#'   truth$shape[channels]
#' ))
#'
#' model_data <- transformed
#' model_data$week <- seq_len(nrow(model_data))
#' model_data$price <- north$price
#' model_data$seasonality <- north$seasonality
#' model_data$holiday <- north$holiday
#'
#' fit <- stats::lm(north$revenue ~ ., data = model_data)
#' contrib <- contributions(transformed, fit, index = north$date)
#'
#' roi(contrib, north[channels])
#'
#' # Average and marginal return are different questions. Television here has
#' # shape = 1.6, an S-curve, and sits *below* its inflection point, so its
#' # marginal return is HIGHER than its average -- the channel is under-funded,
#' # not saturated. Search has shape = 1, a concave curve, where marginal is
#' # always the lower of the two.
#' avg <- roi(contrib, north[channels])
#' for (ch in c("tv", "search")) {
#'   m <- mroi(spend_level = mean(north[[ch]]),
#'             coefficient = stats::coef(fit)[[ch]],
#'             half_max = truth$half_max[[ch]], shape = truth$shape[[ch]])
#'   cat(sprintf("%-7s shape %.1f  average %.4f  marginal %.4f\n",
#'               ch, truth$shape[[ch]], avg$roi[avg$channel == ch], m))
#' }
#' @export
roi <- function(contributions, spend) {
  contrib <- if (is.data.frame(contributions)) {
    if (!all(c("channel", "contribution") %in% names(contributions))) {
      cli::cli_abort("{.arg contributions} needs {.field channel} and \\
                      {.field contribution} columns.")
    }
    tapply(contributions$contribution, contributions$channel, sum)
  } else {
    if (is.null(names(contributions))) {
      cli::cli_abort("{.arg contributions} must be named when given as a vector.")
    }
    contributions
  }
  contrib <- contrib[names(contrib) != "(baseline)"]

  sp <- if (is.data.frame(spend)) {
    vapply(spend, function(z) sum(as.numeric(z), na.rm = TRUE), numeric(1))
  } else {
    if (is.null(names(spend))) {
      cli::cli_abort("{.arg spend} must be named when given as a vector.")
    }
    spend
  }

  common <- intersect(names(contrib), names(sp))
  dropped <- setdiff(union(names(contrib), names(sp)), common)
  if (length(dropped) > 0L && length(common) > 0L) {
    cli::cli_inform(c(
      "{length(dropped)} channel{?s} appear{?s/} in only one of \\
       {.arg contributions} and {.arg spend}, and {?is/are} not in the table.",
      i = "Omitted: {.val {utils::head(dropped, 6)}}."
    ))
  }
  if (length(common) == 0L) {
    cli::cli_abort(c(
      "No channel appears in both {.arg contributions} and {.arg spend}.",
      i = "Contributions: {.val {utils::head(names(contrib), 8)}}.",
      i = "Spend: {.val {utils::head(names(sp), 8)}}."
    ))
  }
  out <- data.frame(
    channel = common,
    contribution = unname(contrib[common]),
    spend = unname(sp[common]),
    stringsAsFactors = FALSE
  )
  out$roi <- ifelse(out$spend > 0, out$contribution / out$spend, NA_real_)
  out <- out[order(-out$roi), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' @rdname roi
#' @export
mroi <- function(spend_level, coefficient, type = "hill", delta = 0.01, ...) {
  spend_level <- .mm_check_scalar(spend_level, "spend_level", lower = 0)
  coefficient <- .mm_check_scalar(coefficient, "coefficient")
  delta <- .mm_check_scalar(delta, "delta", lower = 0, upper = 1,
                            inclusive = c(FALSE, TRUE))
  h <- max(spend_level * delta, .Machine$double.eps^0.25)
  # Clamp the lower point at zero -- spend cannot be negative -- and divide by
  # the interval actually spanned. Dividing by `h` regardless would halve the
  # answer at `spend_level = 0`, which is the single most decision-relevant
  # query: what does the first pound into a dark channel return?
  lo_s <- max(0, spend_level - h / 2)
  hi_s <- spend_level + h / 2
  lo <- saturate(lo_s, type = type, ...)
  hi <- saturate(hi_s, type = type, ...)
  coefficient * (hi - lo) / (hi_s - lo_s)
}

#' Response curve for a channel
#'
#' Predicted response across a range of spend, derived from the fitted
#' saturation parameters. `spend_for()` is the inverse: the spend that achieves
#' a target response.
#'
#' @param spend Numeric vector of spend levels to evaluate. `response_curve()`
#'   only; `spend_for()` has no such argument.
#' @param coefficient The channel's fitted coefficient.
#' @param type Saturation curve type, as in [saturate()].
#' @param target Target response level. `spend_for()` only.
#' @param max_spend Upper bound of the search interval for `spend_for()`.
#'   Defaults to 1000 times the curve's spend-scaled parameter (`half_max`,
#'   `km`, or `log(2)/rate`). `saturate_power()` has no such parameter, so
#'   `max_spend` is required there.
#' @param ... Passed to [saturate()], for example `half_max` and `shape`.
#'
#' @return `response_curve()` returns a data frame with columns `spend`,
#'   `response` and `marginal`, one row per element of `spend` and in the same
#'   order. `spend_for()` returns a single number, or `NA_real_` with a warning
#'   when the target is not reachable within `max_spend`.
#'
#' @details
#' These describe the *response curve the model fitted*, which is not the same
#' thing as what would happen if you actually spent that much. The curve is
#' identified only over the range of spend the data contains; asking a saturated
#' Hill curve what happens at ten times the observed maximum returns a number,
#' and that number is extrapolation. `spend_for()` returns `NA` rather than a
#' fabricated answer when the target lies beyond `max_spend`, but it cannot tell
#' you that a reachable target is outside the data's support. Check the observed
#' spend range yourself.
#'
#' @seealso [saturate()], [mroi()]
#'
#' @examples
#' # A concave curve: marginal return falls from the first pound onward.
#' concave <- response_curve(
#'   spend = seq(0, 100000, by = 20000),
#'   coefficient = 5200, half_max = 45000, shape = 1
#' )
#' concave
#' all(diff(concave$marginal) < 0)
#'
#' # An S-curve: marginal return RISES to the inflection point and only then
#' # falls. Below the peak the channel is under-funded, not saturated.
#' s_curve <- response_curve(
#'   spend = seq(0, 100000, by = 20000),
#'   coefficient = 5200, half_max = 45000, shape = 1.6
#' )
#' s_curve
#' s_curve$spend[which.max(s_curve$marginal)]
#'
#' # What spend achieves a response of 2000?
#' spend_for(target = 2000, coefficient = 5200, half_max = 45000, shape = 1.6)
#'
#' # An unbounded curve needs an explicit search range
#' spend_for(target = 200000, coefficient = 5200, type = "power",
#'           exponent = 0.5, max_spend = 1e6)
#' @export
response_curve <- function(spend, coefficient, type = "hill", ...) {
  spend <- .mm_check_numeric(spend, "spend", allow_na = FALSE, finite = TRUE)
  coefficient <- .mm_check_scalar(coefficient, "coefficient")
  resp <- coefficient * saturate(spend, type = type, ...)
  marg <- vapply(spend, function(s)
    mroi(s, coefficient = coefficient, type = type, ...), numeric(1))
  data.frame(spend = spend, response = resp, marginal = marg,
             stringsAsFactors = FALSE)
}

#' @rdname response_curve
#' @export
spend_for <- function(target, coefficient, type = "hill", max_spend = NULL,
                      ...) {
  target <- .mm_check_scalar(target, "target")
  coefficient <- .mm_check_scalar(coefficient, "coefficient")
  args <- list(...)
  if (is.null(max_spend)) {
    # A sensible upper bracket depends on the curve. Only Hill and
    # Michaelis-Menten carry a spend-scaled parameter; for the others there is
    # nothing to infer a scale from, so say so rather than silently searching
    # up to 1000.
    base <- if (!is.null(args$half_max)) args$half_max else
      if (!is.null(args$km)) args$km else
        if (!is.null(args$rate)) log(2) / args$rate else NULL
    if (is.null(base)) {
      cli::cli_abort(c(
        "{.arg max_spend} is required for saturation type {.val {type}}.",
        i = "This curve has no spend-scaled parameter to infer a search range \\
             from.",
        i = "Give the largest spend worth considering, for example \\
             {.code max_spend = 10 * max(observed_spend)}."
      ))
    }
    max_spend <- base * 1000
  }
  max_spend <- .mm_check_scalar(max_spend, "max_spend", lower = 0,
                                inclusive = c(FALSE, TRUE))

  f <- function(s) coefficient * saturate(s, type = type, ...) - target
  f0 <- f(0)
  fmax <- f(max_spend)
  if (isTRUE(all.equal(f0, 0))) return(0)
  if (!is.finite(f0) || !is.finite(fmax)) return(NA_real_)

  # Works in both directions: a negative coefficient makes the response
  # decreasing, and a decreasing curve can still cross a negative target.
  if (sign(f0) == sign(fmax)) {
    reachable <- coefficient * saturate(max_spend, type = type, ...)
    bounded <- type %in% c("hill", "exponential", "michaelis_menten")
    cli::cli_warn(c(
      "The target response is not reachable within {.arg max_spend}.",
      i = "At {.arg max_spend} = {max_spend} the response is \\
           {signif(reachable, 6)}; the target is {signif(target, 6)}.",
      i = if (bounded) {
        "This curve is bounded, so beyond some point no extra spend helps."
      } else {
        "This curve is unbounded, so a larger {.arg max_spend} may reach it."
      }
    ))
    return(NA_real_)
  }
  stats::uniroot(f, interval = c(0, max_spend))$root
}

#' @keywords internal
#' @noRd
.mm_coefficients <- function(model, call = parent.frame(2)) {
  if (is.numeric(model) && !is.null(names(model))) return(model)
  cf <- try(stats::coef(model), silent = TRUE)
  if (inherits(cf, "try-error") || !is.numeric(cf) || is.null(names(cf))) {
    cli::cli_abort(c(
      "Could not extract named coefficients from {.arg model}.",
      i = "Pass a fitted model that {.fn stats::coef} understands, or a named \\
           numeric vector."
    ), call = call)
  }
  cf
}
