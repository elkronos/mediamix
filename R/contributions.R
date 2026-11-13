#' Decompose predicted KPI into channel contributions
#'
#' Splits a fitted model's prediction into a per-channel contribution plus a
#' baseline. This is what a marketing mix model deliverable actually is: not the
#' coefficients, but the answer to "how much of last year's revenue did each
#' channel produce, and what would have happened anyway".
#'
#' @param media A data frame of the *transformed* media regressors (adstocked
#'   and saturated), exactly as they entered the model. Column names must match
#'   the model's coefficient names. Backticks in a coefficient name are
#'   ignored, so a column named `TV Spend` matches the coefficient
#'   `` `TV Spend` ``. Numeric columns that match no coefficient are left out,
#'   with a message.
#' @param model A fitted model, or a named numeric vector of coefficients.
#'   Anything [stats::coef()] works on will do. For a mixed model from
#'   \pkg{nlme} or \pkg{lme4}, the fixed effects are used, and group-level
#'   deviations from them are part of the baseline.
#' @param intercept Model intercept, used when the baseline is the intercept:
#'   for a bare coefficient vector, or for a fitted model that cannot predict
#'   the periods in `media`. Taken from `model` when it has one; supply it
#'   explicitly when passing a bare coefficient vector without an
#'   `"(Intercept)"` element.
#' @param index Optional vector, the same length as `nrow(media)`, labelling the
#'   periods. Used as the `period` column of the result; defaults to the row
#'   numbers.
#' @param by Optional grouping vector, the same length as `nrow(media)`, or
#'   a data frame of grouping columns with one row per row of `media`. With a
#'   data frame, `group` joins each row's values with `" / "`.
#' @param newdata Optional data frame holding every variable `model` needs, one
#'   row per row of `media`, for periods the model was not fitted on: a holdout,
#'   or a reporting period. The baseline is then `predict(model, newdata)`
#'   minus the media effect. Columns that appear in both `media` and `newdata`
#'   must hold the same values.
#'
#' @return A data frame in long format with columns `period`, `group` (present
#'   only when `by` is supplied), `channel` and `contribution`. The baseline
#'   appears as a channel named `"(baseline)"`. The result has class
#'   `mm_contributions`, with a [plot()][plot.mm_contributions] method.
#'
#'   The baseline is the model's prediction minus the media effect, so the
#'   contributions in each period sum to that prediction. Which prediction is
#'   used depends on the rows of `media`:
#'
#'   * When `media` holds the rows the model was fitted on, it is the model's
#'     fitted value. The rows are recognised by comparing the media columns
#'     with the model frame. A row the model dropped for a missing value has a
#'     `NA` baseline, and a message says how many there are.
#'   * For any other rows, it is `predict(model, newdata)`. Without `newdata`,
#'     `media` itself is used when it holds every variable the model needs;
#'     otherwise the baseline is the intercept alone, and a message says so.
#'     The fitted values of the training rows are never used for other rows.
#'   * For a bare coefficient vector there is nothing to predict with, so the
#'     baseline is the intercept, and contributions sum to the media effect
#'     plus that intercept. The function says so when the model has terms
#'     that are not among the media columns.
#'
#' @details
#' Contributions are computed as \eqn{\beta_j x_{jt}} for each channel, with
#' everything else (intercept, trend, price, seasonality, controls) pooled
#' into `"(baseline)"`. That pooling is a deliberate simplification: it is what
#' makes contributions sum exactly to the fitted value, and it means the
#' baseline is not "organic demand" but "everything this decomposition does not
#' attribute to media". Reporting it as organic demand makes a marketing mix
#' deliverable look more precise than it is.
#'
#' Contributions inherit whatever the model's identification is worth. If two
#' channels are collinear the split between them is arbitrary even when the
#' total is well estimated, and if the model dropped a coefficient outright
#' this function refuses rather than reporting `NA` contributions that would
#' quietly propagate into an ROI table. Run [diagnose_media()] before
#' presenting any of this.
#'
#' On panel data, pass one geography at a time or supply `by`. `index` is used
#' as a label, not a key: with `by` supplied and one date per geography, a
#' `tapply()` over `period` alone silently adds the geographies together.
#'
#' @section Models with a link function:
#' For a generalised linear model whose link is not the identity, such as a
#' Poisson or log-link model, \eqn{\beta_j x_{jt}} is on the scale of the
#' linear predictor, not of the KPI. Contributions are then computed on that
#' scale throughout: the baseline is the linear predictor minus the media
#' effect, each period sums to the linear predictor rather than to the fitted
#' KPI, and a warning says so. Under a log link, `exp()` of a channel's
#' contribution is the factor by which the channel multiplies the predicted
#' KPI. To express a channel's effect in KPI units, compare the model's
#' predictions with predictions from data in which that channel is set to
#' zero.
#'
#' @seealso [roi()], [response_curve()], [diagnose_media()],
#'   [plot.mm_contributions()]
#'
#' @examples
#' data(mm_weekly)
#' north <- mm_weekly[mm_weekly$geo == "north", ]
#' channels <- c("tv", "video", "search", "social", "display")
#'
#' # Transform with the parameters the data was generated from, so the example
#' # shows a correctly specified decomposition rather than a misspecified one.
#' truth <- attr(mm_weekly, "truth")
#' spec <- lapply(stats::setNames(channels, channels), function(ch) list(
#'   adstock = list(decay = truth$decay[[ch]]),
#'   saturation = list(half_max = truth$half_max[[ch]],
#'                     shape = truth$shape[[ch]])
#' ))
#' transformed <- transform_plan(north[channels], spec)
#'
#' # Include the controls the data was simulated with. Without them, the media
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
#' # predicts that is not attributed to these five columns: trend, price,
#' # seasonality, holidays and the intercept. It is not organic demand.
#' round(tapply(contrib$contribution, contrib$channel, sum))
#'
#' # A period the model was not fitted on: fit on the first two years and
#' # decompose the third, with `newdata` holding the controls for those weeks.
#' early <- seq_len(104)
#' fit_early <- stats::lm(revenue ~ ., data = cbind(revenue = north$revenue,
#'                                                  model_data)[early, ])
#' holdout <- contributions(transformed[-early, ], fit_early,
#'                          index = north$date[-early],
#'                          newdata = model_data[-early, ])
#' all.equal(as.numeric(tapply(holdout$contribution, holdout$period, sum)),
#'           unname(stats::predict(fit_early, model_data[-early, ])))
#' @export
contributions <- function(media, model, intercept = NULL, index = NULL,
                          by = NULL, newdata = NULL) {
  if (!is.data.frame(media)) {
    if (is.matrix(media)) media <- as.data.frame(media) else
      cli::cli_abort("{.arg media} must be a data frame or matrix of \\
                      transformed regressors.")
  }
  n <- nrow(media)
  if (n == 0L) cli::cli_abort("{.arg media} has no rows.")
  bare <- is.numeric(model) && !is.null(names(model))
  cf <- .mm_coefficients(model)
  # A coefficient for a name that needed backticks in the formula, such as
  # `TV Spend`, keeps them; the column it came from does not have them.
  names(cf) <- gsub("`", "", names(cf), fixed = TRUE)
  has_intercept <- "(Intercept)" %in% names(cf)
  given_intercept <- !is.null(intercept)
  if (!given_intercept) {
    intercept <- if (has_intercept) unname(cf[["(Intercept)"]]) else 0
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
  numeric_cols <- names(media)[vapply(media, is.numeric, logical(1))]
  spare <- setdiff(numeric_cols, known)
  if (length(spare) > 0L) {
    cli::cli_inform(c(
      "{length(spare)} numeric column{?s} of {.arg media} match{?es/} no model \\
       coefficient and {?is/are} left out: {.val {utils::head(spare, 6)}}.",
      i = if (length(unmatched) > 0L) {
        "Coefficients with no media column: \\
         {.val {utils::head(unmatched, 6)}}."
      },
      i = "Columns are matched to coefficients by exact name, so a term such \\
           as {.code log1p(tv)} needs a column of that name."
    ))
  }
  if (bare && !has_intercept && !given_intercept) {
    cli::cli_inform(c(
      "The coefficients have no {.val (Intercept)} element, so the intercept \\
       is taken as 0.",
      i = "Supply {.arg intercept} if the model had one."
    ))
  }
  if (is.null(index)) index <- seq_len(n)
  if (length(index) != n) {
    cli::cli_abort("{.arg index} must have {n} element{?s}, not {length(index)}.")
  }
  if (!is.null(by)) {
    len <- if (is.data.frame(by)) nrow(by) else length(by)
    if (len != n) {
      cli::cli_abort("{.arg by} must have one element per row of \\
                      {.arg media} ({n}), not {len}.")
    }
  }
  by_frame <- is.data.frame(by)
  by <- .mm_check_by(by, n)
  if (by_frame) by <- .mm_label_key(by)

  link <- if (bare) NULL else .mm_link(model)
  link_scale <- !is.null(link) && !identical(link, "identity")
  if (link_scale) {
    cli::cli_warn(c(
      "{.arg model} has a {.val {link}} link, so contributions are on the \\
       scale of its linear predictor, not of the KPI.",
      i = "Each period sums to the linear predictor. Under a log link, \\
           {.code exp()} of a contribution is the factor by which that \\
           channel multiplies the prediction.",
      i = "See \"Models with a link function\" in {.code ?contributions}."
    ))
  }

  here <- environment()
  values <- lapply(stats::setNames(known, known), function(nm) {
    .mm_as_number(media[[nm]], nm, call = here)
  })
  effects <- lapply(known, function(nm) unname(cf[[nm]]) * values[[nm]])
  names(effects) <- known
  media_effect <- Reduce(`+`, effects, numeric(n))

  # The baseline is everything the model predicts that this decomposition does
  # not attribute to the supplied media. Taking it as (prediction - media
  # effect) rather than as the bare intercept is what makes the contributions
  # sum to the prediction, which is the property the whole table is read for.
  # The prediction has to be for these rows: the fitted values belong to the
  # rows the model was fitted on and to no others.
  baseline <- .mm_contrib_baseline(model, bare, values, media, newdata,
                                   link_scale, n)
  other <- if (is.null(baseline)) {
    rep(intercept, n)
  } else {
    baseline - media_effect
  }
  if (is.null(baseline) && bare && length(unmatched) > 0L) {
    cli::cli_inform(c(
      "Contributions sum to the media effect plus the intercept, not to the \\
       model's fitted value.",
      i = "{length(unmatched)} model term{?s} {?is/are} not among the media \\
           columns: {.val {utils::head(unmatched, 6)}}.",
      i = "Pass a fitted model rather than a coefficient vector, and those \\
           terms are pooled into {.val (baseline)} instead."
    ))
  }
  if (!is.null(baseline) && given_intercept) {
    cli::cli_inform("{.arg intercept} is not used: the baseline comes from the \\
                     model's prediction, which includes its own intercept.")
  }

  parts <- lapply(known, function(nm) {
    data.frame(period = index, channel = nm, contribution = effects[[nm]],
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
  class(out) <- c("mm_contributions", "data.frame")
  out
}

# The model's prediction for the rows of `media`, on the scale contributions
# are computed on, or NULL when there is none and the baseline is the
# intercept.
#' @keywords internal
#' @noRd
.mm_contrib_baseline <- function(model, bare, values, media, newdata,
                                 link_scale, n, call = parent.frame()) {
  if (!is.null(newdata)) {
    if (bare) {
      cli::cli_abort(c(
        "{.arg newdata} needs a fitted model to predict from.",
        i = "{.arg model} is a coefficient vector, which cannot predict."
      ), call = call)
    }
    if (!is.data.frame(newdata)) {
      cli::cli_abort("{.arg newdata} must be a data frame.", call = call)
    }
    if (nrow(newdata) != n) {
      cli::cli_abort("{.arg newdata} must have one row per row of \\
                      {.arg media} ({n}), not {nrow(newdata)}.", call = call)
    }
    both <- intersect(names(values), names(newdata))
    differ <- both[!vapply(both, function(nm) {
      nd <- suppressWarnings(as.numeric(newdata[[nm]]))
      isTRUE(all.equal(values[[nm]], nd, check.attributes = FALSE))
    }, logical(1))]
    if (length(differ) > 0L) {
      cli::cli_abort(c(
        "{.arg media} and {.arg newdata} hold different values for \\
         {.val {differ}}.",
        i = "The media effect comes from {.arg media} and the prediction from \\
             {.arg newdata}, so a column in both must hold the same values."
      ), call = call)
    }
    pred <- tryCatch(.mm_model_predict(model, newdata, link_scale),
                     error = function(e) {
                       cli::cli_abort(c(
                         "{.arg model} could not predict from {.arg newdata}.",
                         x = conditionMessage(e)
                       ), call = call)
                     })
    if (length(pred) != n) {
      cli::cli_abort("{.fn predict} returned {length(pred)} value{?s} for the \\
                      {n} row{?s} of {.arg newdata}.", call = call)
    }
    return(pred)
  }
  if (bare) return(NULL)

  dropped <- .mm_dropped_rows(model)
  fitted_values <- .mm_model_fitted(model, link_scale)
  full <- if (is.null(fitted_values)) NULL else
    .mm_spread_rows(fitted_values, dropped, n)
  same <- if (is.null(full)) FALSE else
    .mm_fitting_rows(model, values, dropped, n)
  if (!isFALSE(same)) {
    if (is.na(same)) {
      cli::cli_inform(c(
        "Could not check that {.arg media} holds the rows {.arg model} was \\
         fitted on, so its fitted values are used as given.",
        i = "For periods the model was not fitted on, pass {.arg newdata}."
      ))
    }
    lost <- sum(is.na(full))
    if (lost > 0L) {
      cli::cli_inform(c(
        "{lost} period{?s} {?was/were} left out of the fit for missing \\
         values, so {?its/their} baseline is {.val {NA}}.",
        i = "Pass {.arg newdata} with the model's variables for every period \\
             to predict {cli::qty(lost)}{?it/them}."
      ))
    }
    return(full)
  }

  vars <- .mm_model_vars(model)
  absent <- if (is.null(vars)) character(0) else setdiff(vars, names(media))
  if (!is.null(vars) && length(absent) == 0L) {
    pred <- tryCatch(.mm_model_predict(model, media, link_scale),
                     error = function(e) NULL)
    if (length(pred) == n) return(pred)
  }
  cli::cli_inform(c(
    "{.arg media} does not hold the rows {.arg model} was fitted on, so the \\
     baseline is the intercept alone.",
    x = "Contributions do not sum to the model's prediction for these periods.",
    i = if (length(absent) > 0L) {
      "Pass {.arg newdata} with the model's other variables for these \\
       periods, such as {.val {utils::head(absent, 6)}}."
    } else {
      "Pass {.arg newdata} with every variable the model needs for these \\
       periods."
    }
  ))
  NULL
}

# The link of a model that has a family, or NULL.
#' @keywords internal
#' @noRd
.mm_link <- function(model) {
  fam <- tryCatch(stats::family(model), error = function(e) NULL)
  if (!inherits(fam, "family")) return(NULL)
  fam$link
}

#' @keywords internal
#' @noRd
.mm_model_predict <- function(model, newdata, link_scale) {
  out <- if (link_scale) {
    stats::predict(model, newdata = newdata, type = "link")
  } else {
    stats::predict(model, newdata = newdata)
  }
  as.numeric(out)
}

# Fitted values on the scale contributions are computed on, or NULL.
#' @keywords internal
#' @noRd
.mm_model_fitted <- function(model, link_scale) {
  out <- tryCatch(suppressWarnings(
    if (link_scale) stats::predict(model, type = "link") else stats::fitted(model)
  ), error = function(e) NULL)
  if (!is.numeric(out)) return(NULL)
  as.numeric(out)
}

# Rows of the original data the model dropped for missing values.
#' @keywords internal
#' @noRd
.mm_dropped_rows <- function(model) {
  na <- tryCatch(stats::na.action(model), error = function(e) NULL)
  if (!is.numeric(na)) return(integer(0))
  as.integer(na)
}

# Values for the rows a model was fitted on, spread back over all `n` rows of
# the data with NA in the rows it dropped. `stats::naresid()` does this only
# for `na.exclude`; under the default `na.omit` it returns the short vector.
# NULL when the lengths cannot be reconciled.
#' @keywords internal
#' @noRd
.mm_spread_rows <- function(v, dropped, n) {
  if (length(v) == n) return(v)
  k <- length(dropped)
  if (k > 0L && length(v) + k == n && all(dropped >= 1L & dropped <= n)) {
    out <- rep(NA_real_, n)
    out[-dropped] <- v
    return(out)
  }
  NULL
}

# TRUE when the media columns found in the model frame hold the same values as
# `values`, so `media` is the data the model was fitted on; FALSE when they
# differ; NA when no column could be compared.
#' @keywords internal
#' @noRd
.mm_fitting_rows <- function(model, values, dropped, n) {
  mf <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  if (!is.data.frame(mf)) return(NA)
  checked <- FALSE
  for (nm in intersect(names(values), names(mf))) {
    col <- mf[[nm]]
    if (!is.numeric(col) || !is.null(dim(col))) next
    full <- .mm_spread_rows(as.numeric(col), dropped, n)
    if (is.null(full)) return(FALSE)
    keep <- !is.na(full)
    if (!isTRUE(all.equal(full[keep], values[[nm]][keep],
                          check.attributes = FALSE))) {
      return(FALSE)
    }
    checked <- TRUE
  }
  if (checked) TRUE else NA
}

# Variables the model's right-hand side uses, or NULL when it has no terms.
#' @keywords internal
#' @noRd
.mm_model_vars <- function(model) {
  tt <- tryCatch(stats::terms(model), error = function(e) NULL)
  if (!inherits(tt, "terms")) return(NULL)
  all.vars(stats::delete.response(tt))
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
#'   channel and one row per period, or a named numeric vector of total spend
#'   per channel. Spend columns are read as numbers: a factor is read by its
#'   labels, and text that is not a number is an error.
#' @param by Optional group of each row of `spend`, for one row per channel
#'   per group: the name of a column of `spend`, or a vector (or a data frame
#'   of key columns) with one value per row of `spend`, as given to
#'   [contributions()]. `contributions` must then come from [contributions()]
#'   with `by`, and its `group` column is matched to these groups.
#' @param index Optional period of each row of `spend`: the name of a column
#'   of `spend`, or a vector with one value per row, as given to
#'   [contributions()]. With `index`, spend and contributions are matched
#'   period by period, within each group when `by` is supplied, and periods
#'   that appear in only one of them are left out, with a message.
#' @param periods Optional vector of the periods to include, compared with the
#'   `period` column of `contributions` and with `index`, so that one year or
#'   one quarter of a longer decomposition can be reported. When `spend` is a
#'   data frame, `periods` needs `index`. When `spend` gives totals,
#'   `periods` selects contributions only, and the totals must cover the same
#'   periods.
#' @param spend_level Current spend level for the channel, in the same units the
#'   saturation curve was fitted on: the adstocked level, for a curve applied
#'   after adstock as in [media_transform()].
#' @param coefficient The channel's fitted coefficient.
#' @param delta Increment used for the numerical derivative, as a fraction of
#'   `spend_level`.
#' @param ... Passed to [saturate()], for example `half_max` and `shape`.
#' @param type Saturation curve type, as in [saturate()], or `"none"` for a
#'   linear response.
#'
#' @return `roi()` returns a data frame with columns `channel`, `contribution`,
#'   `spend` and `roi`, ordered by descending ROI, with `roi` set to `NA` where
#'   spend is zero. Channels appearing in only one of the two inputs are
#'   omitted, and reported when that happens. With `by`, a `group` column
#'   comes first and there is one row per channel per group: groups in the
#'   order they first appear in `contributions`, channels by descending ROI
#'   within each. Groups found in only one of the two inputs are omitted, with
#'   a message. `mroi()` returns a single number.
#'
#'   Each channel's contribution and spend must cover the same periods. When
#'   `contributions` comes from [contributions()] and `spend` is a data frame
#'   without `index`, `roi()` warns if the number of periods decomposed
#'   differs from the number of rows of `spend`, group by group with `by`.
#'   Give `index` to match the two period by period instead.
#'
#' @details
#' Average ROI and marginal ROI answer different questions and are routinely
#' confused. Average ROI is a scorecard: what did this channel return over the
#' period as a whole. Marginal ROI is the decision variable: what would the next
#' unit return. Only the second one should drive a reallocation, and the two can
#' point in opposite directions.
#'
#' Where they sit relative to each other depends on the shape of the curve.
#'
#' For a **concave** response (`saturate_hill()` with `shape <= 1`,
#' Michaelis--Menten, negative exponential, `saturate_power()`), marginal ROI
#' is below average ROI everywhere, and the gap widens the further up the curve
#' a channel sits. Reallocating on average ROI then systematically over-funds
#' channels that are already saturated, which are exactly the channels that look
#' best on an average-ROI table.
#'
#' For an **S-shaped** response (`saturate_hill()` with `shape > 1`, the usual
#' way to represent a threshold effect), the curve is *convex* below its
#' inflection point, and there marginal ROI is *above* average ROI. A channel
#' in that region has not saturated: each extra pound works harder than the
#' pounds already spent, so a cut there loses more per pound than the average
#' return suggests. Whether the channel should get more budget still depends on
#' how its marginal return compares with other channels' and with break-even.
#'
#' `mroi()` differentiates the fitted response curve numerically, so it works
#' for any curve [saturate()] supports, with a step of `delta` times
#' `spend_level` that never falls below a small fraction of the curve's own
#' spend parameter (`half_max`, `km`, or the half-life of `rate`). At
#' `spend_level = 0` it is finite and correct wherever the curve's slope at
#' zero is finite: Hill curves with `shape >= 1`, Michaelis--Menten,
#' exponential and `"none"`. A Hill curve with `shape < 1` and a power curve
#' with `exponent < 1` rise infinitely steeply at zero, and there `mroi()`
#' returns the average slope over a short first step, which is large but not a
#' derivative.
#'
#' @section Marginal return when the regressor was adstocked:
#' `mroi()` is the slope of the coefficient times the saturation curve at a
#' single level of the curve's input, the *adstocked* media. It is a property
#' of the curve, not yet a return on spend, and two things separate the two.
#'
#' Carryover spreads a unit of spend over many periods. Under a normalised
#' kernel the weights sum to one, so the *total* extra response to one more
#' unit of spend is roughly the curve's slope, arriving over the kernel's
#' length. Multiplying by the kernel's first weight (`1 - decay`) gives only
#' the response inside the period of spend; comparing that with an average ROI
#' that counts every period's carryover compares a part with a whole, and
#' understates slow channels several-fold.
#'
#' And the slope at the *mean* adstocked level is not the mean of the slope
#' across periods, which matters for flighted media and S-shaped curves.
#'
#' [marginal_roi()] handles both by re-running the whole transform on a
#' slightly larger budget. Use it for anything that feeds a budget decision,
#' and use `mroi()` to read the shape of a curve.
#'
#' @seealso [marginal_roi()] for marginal return on spend through the full
#'   transform, [contributions()], [response_curve()], [spend_for()]
#'
#' @examples
#' data(mm_weekly)
#' north <- mm_weekly[mm_weekly$geo == "north", ]
#' channels <- c("tv", "video", "search", "social", "display")
#' truth <- attr(mm_weekly, "truth")
#'
#' spec <- lapply(stats::setNames(channels, channels), function(ch) list(
#'   adstock = list(decay = truth$decay[[ch]]),
#'   saturation = list(half_max = truth$half_max[[ch]],
#'                     shape = truth$shape[[ch]])
#' ))
#' transformed <- transform_plan(north[channels], spec)
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
#' # One row per channel per year: give contributions() and roi() the same `by`
#' year <- format(north$date, "%Y")
#' by_year <- contributions(transformed, fit, index = north$date, by = year)
#' roi(by_year, north[channels], by = year)
#'
#' # The last year only, with spend matched to contributions week by week
#' roi(contrib, north[c("date", channels)], index = "date",
#'     periods = north$date[year == "2025"])
#'
#' # Average and marginal return are different questions. marginal_roi()
#' # re-runs the whole transform on a 1% larger budget, so carryover and
#' # flighting are both accounted for; its `roi` column matches
#' # roi(contrib, north[channels]). Display's average pound returns about 1.5,
#' # but its next pound returns less than it costs.
#' marginal_roi(north, stats::coef(fit), spec = spec)
#' @export
roi <- function(contributions, spend, by = NULL, index = NULL,
                periods = NULL) {
  framed <- is.data.frame(contributions)
  if (framed) {
    if (!all(c("channel", "contribution") %in% names(contributions))) {
      cli::cli_abort("{.arg contributions} needs {.field channel} and \\
                      {.field contribution} columns.")
    }
  } else if (is.null(names(contributions))) {
    cli::cli_abort("{.arg contributions} must be named when given as a vector.")
  }
  if (!is.data.frame(spend)) {
    if (is.null(names(spend))) {
      cli::cli_abort("{.arg spend} must be named when given as a vector.")
    }
    if (!is.numeric(spend)) {
      cli::cli_abort("{.arg spend} must be numeric, not \\
                      {.obj_type_friendly {spend}}.")
    }
  }
  sel <- .mm_roi_rows(contributions, spend, by, index, periods)
  groups <- sel$groups

  # Totals per group. Without `by` every row is in the one group "".
  contrib <- if (framed) {
    rows <- which(sel$keep_c)
    ch <- as.character(contributions$channel[rows])
    val <- contributions$contribution[rows]
    g_of <- sel$c_group[rows]
    lapply(groups, function(g) {
      i <- g_of == g
      tapply(val[i], ch[i], sum)
    })
  } else {
    list(contributions)
  }
  contrib <- lapply(contrib, function(v) v[names(v) != "(baseline)"])
  contrib_names <- unique(unlist(lapply(contrib, names), use.names = FALSE))
  # A column that labels the rows of `spend` is not a channel.
  spend_names <- setdiff(names(spend), sel$label_cols)
  common <- intersect(contrib_names, spend_names)
  dropped <- setdiff(union(contrib_names, spend_names), common)
  sp <- if (is.data.frame(spend)) {
    # Only the channel columns are read, so a date or label column in `spend`
    # is reported as unmatched rather than summed.
    here <- environment()
    values <- lapply(stats::setNames(common, common), function(cn) {
      .mm_as_number(spend[[cn]], cn, call = here)
    })
    lapply(groups, function(g) {
      i <- sel$keep_s & sel$s_group == g
      vapply(values, function(v) sum(v[i], na.rm = TRUE), numeric(1))
    })
  } else {
    list(spend)
  }
  if (framed && "period" %in% names(contributions) && is.data.frame(spend) &&
      is.null(index) && length(common) > 0L) {
    .mm_roi_check_counts(contributions$channel, sel, groups,
                         grouped = !is.null(by))
  }
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
      i = "Contributions: {.val {utils::head(contrib_names, 8)}}.",
      i = "Spend: {.val {utils::head(spend_names, 8)}}."
    ))
  }
  out <- do.call(rbind, lapply(seq_along(groups), function(k) {
    data.frame(
      group = groups[[k]],
      channel = common,
      contribution = unname(contrib[[k]][common]),
      spend = unname(sp[[k]][common]),
      stringsAsFactors = FALSE
    )
  }))
  out$roi <- ifelse(out$spend > 0, out$contribution / out$spend, NA_real_)
  out <- out[order(match(out$group, groups), -out$roi), , drop = FALSE]
  if (is.null(by)) out$group <- NULL
  rownames(out) <- NULL
  out
}

# Which rows of `contributions` and `spend` enter the totals, and the group
# of each row. Groups are keys, as contributions() stores them; without `by`
# every row is in the group "". Rows outside `periods`, groups found in only
# one input and, with `index`, periods found in only one input are left out.
#' @keywords internal
#' @noRd
.mm_roi_rows <- function(contributions, spend, by, index, periods,
                         call = parent.frame()) {
  framed <- is.data.frame(contributions)
  if (!framed && (!is.null(by) || !is.null(index) || !is.null(periods))) {
    cli::cli_abort(c(
      "{.arg by}, {.arg index} and {.arg periods} need {.arg contributions} \\
       as a data frame from {.fn contributions}.",
      i = "A vector of totals has no groups or periods to select."
    ), call = call)
  }
  if (!is.data.frame(spend) && (!is.null(by) || !is.null(index))) {
    cli::cli_abort(c(
      "{.arg by} and {.arg index} need {.arg spend} as a data frame with one \\
       row per period.",
      i = "A vector of totals has no rows to group or match."
    ), call = call)
  }
  if (!is.null(by) && !"group" %in% names(contributions)) {
    cli::cli_abort(c(
      "{.arg by} needs the {.field group} column that \\
       {.code contributions(by = )} adds.",
      i = "Decompose with the same {.arg by}, then pass it to {.fn roi}."
    ), call = call)
  }
  dated <- !is.null(index) || !is.null(periods)
  if (dated && !"period" %in% names(contributions)) {
    cli::cli_abort("{.arg index} and {.arg periods} need the {.field period} \\
                    column that {.fn contributions} adds.", call = call)
  }
  if (!is.null(periods) && is.data.frame(spend) && is.null(index)) {
    cli::cli_abort(c(
      "{.arg periods} needs {.arg index}, the period of each row of \\
       {.arg spend}.",
      i = "Without it, the rows of {.arg spend} that fall in {.arg periods} \\
           are not known."
    ), call = call)
  }
  s_by <- .mm_roi_labels(by, spend, "by", frame = TRUE, call = call)
  s_index <- .mm_roi_labels(index, spend, "index", frame = FALSE, call = call)

  n_c <- if (framed) nrow(contributions) else 0L
  n_s <- if (is.data.frame(spend)) nrow(spend) else 0L
  c_group <- if (is.null(by)) rep("", n_c) else
    .mm_key_values(contributions$group)
  s_group <- if (is.null(by)) rep("", n_s) else s_by$keys
  c_period <- if (dated) .mm_key_values(contributions$period) else NULL
  s_period <- s_index$keys
  keep_c <- rep(TRUE, n_c)
  keep_s <- rep(TRUE, n_s)

  if (!is.null(periods)) {
    wanted <- .mm_roi_periods(periods, call = call)
    keep_c <- !is.na(c_period) & c_period %in% wanted
    if (!any(keep_c)) {
      cli::cli_abort(c(
        "No value of {.arg periods} is in the {.field period} column of \\
         {.arg contributions}.",
        i = "Its periods include {.val {utils::head(unique(c_period), 3)}}."
      ), call = call)
    }
    if (!is.null(s_period)) {
      keep_s <- s_period %in% wanted
      if (!any(keep_s)) {
        cli::cli_abort(c(
          "No value of {.arg periods} is in {.arg index}.",
          i = "{.arg index} includes {.val {utils::head(unique(s_period), 3)}}."
        ), call = call)
      }
    }
  }

  if (!is.null(by)) {
    in_c <- unique(c_group[keep_c])
    in_s <- unique(s_group[keep_s])
    both <- intersect(in_c, in_s)
    if (length(both) == 0L) {
      cli::cli_abort(c(
        "No group appears in both {.arg contributions} and {.arg spend}.",
        i = "Contributions: {.val {utils::head(.mm_show_key(in_c), 6)}}.",
        i = "Spend: {.val {utils::head(.mm_show_key(in_s), 6)}}."
      ), call = call)
    }
    lone <- setdiff(union(in_c, in_s), both)
    if (length(lone) > 0L) {
      cli::cli_inform(c(
        "{length(lone)} group{?s} appear{?s/} in only one of \\
         {.arg contributions} and {.arg spend}, and {?is/are} not in the \\
         table.",
        i = "Omitted: {.val {utils::head(.mm_show_key(lone), 6)}}."
      ))
    }
    keep_c <- keep_c & c_group %in% both
    keep_s <- keep_s & s_group %in% both
  }

  if (!is.null(s_period)) {
    sep <- .mm_key_sep()
    kc <- paste(c_group, c_period, sep = sep)
    ks <- paste(s_group, s_period, sep = sep)
    # A period can repeat within a group on either side: spend kept at a finer
    # grain, or contributions from several series that `by` does not separate.
    # Both are pooled within the period. That is right when the two sides
    # cover the same series and wrong when one covers only some of them, so
    # say so whenever the number of rows per period differs.
    n_s <- table(ks[keep_s])
    n_c <- tapply(kc[keep_c], contributions$channel[keep_c],
                  function(k) table(k), simplify = FALSE)
    n_c <- if (length(n_c) > 0L) n_c[[1L]] else n_s[0L]
    shared <- intersect(names(n_s), names(n_c))
    differs <- shared[as.integer(n_s[shared]) != as.integer(n_c[shared])]
    if (length(differs) > 0L) {
      k1 <- differs[1L]
      p1 <- c_period[match(k1, kc)]
      cli::cli_inform(c(
        "{length(differs)} period{?s} {?has/have} a different number of rows \
         in {.arg contributions} and {.arg spend}, and each side is summed \
         within the period.",
        i = "For example {.val {p1}}: {as.integer(n_c[[k1]])} per channel in \
             {.arg contributions}, {as.integer(n_s[[k1]])} in {.arg spend}.",
        i = if (is.null(by)) {
          "If one of them covers only some of the series, pass {.arg by}."
        } else {
          "If that is not intended, aggregate to one row per period and \
           group first."
        }
      ))
    }
    matched_c <- kc %in% ks[keep_s]
    matched_s <- ks %in% kc[keep_c]
    lone_c <- keep_c & !matched_c
    lone_s <- keep_s & !matched_s
    if (!any(keep_c & matched_c)) {
      cli::cli_abort(c(
        "No period appears in both {.arg contributions} and {.arg spend}.",
        i = "Contributions: {.val {utils::head(unique(c_period[keep_c]), 3)}}.",
        i = "Spend: {.val {utils::head(unique(s_period[keep_s]), 3)}}.",
        i = "Give {.arg index} as the same kind of value as the {.arg index} \\
             passed to {.fn contributions}."
      ), call = call)
    }
    if (any(lone_c) || any(lone_s)) {
      n_lone_c <- length(unique(c_period[lone_c]))
      n_lone_s <- length(unique(s_period[lone_s]))
      eg_c <- utils::head(unique(c_period[lone_c]), 3)
      eg_s <- utils::head(unique(s_period[lone_s]), 3)
      cli::cli_inform(c(
        "Periods in only one of {.arg contributions} and {.arg spend} are \\
         left out.",
        i = if (n_lone_c > 0L) {
          "{n_lone_c} period{?s} of {.arg contributions} {?has/have} no row \\
           in {.arg spend}, for example {.val {eg_c}}."
        },
        i = if (n_lone_s > 0L) {
          "{n_lone_s} period{?s} of {.arg spend} {?is/are} not in \\
           {.arg contributions}, for example {.val {eg_s}}."
        }
      ))
    }
    keep_c <- keep_c & matched_c
    keep_s <- keep_s & matched_s
  }

  list(keep_c = keep_c, keep_s = keep_s, c_group = c_group,
       s_group = s_group,
       groups = if (framed) unique(c_group[keep_c]) else "",
       label_cols = c(s_by$col, s_index$col))
}

# Labels for the rows of `spend`, given as `by` or `index`: a column of `spend`
# named by a string, a vector with one value per row or, for `by`, a data
# frame of key columns. Returns the labels as keys built the way
# contributions() builds them, and the column used, if any.
#' @keywords internal
#' @noRd
.mm_roi_labels <- function(x, spend, arg, frame, call = parent.frame()) {
  if (is.null(x)) return(NULL)
  n <- nrow(spend)
  col <- NULL
  if (.mm_is_string(x) && (n != 1L || x %in% names(spend))) {
    .mm_assert_cols(spend, x, call = call)
    col <- x
    x <- spend[[x]]
  }
  if (frame && is.data.frame(x)) {
    if (ncol(x) == 0L || nrow(x) != n) {
      cli::cli_abort("{.arg {arg}} must have one row per row of \\
                      {.arg spend} ({n}), not {nrow(x)}.", call = call)
    }
    .mm_check_key_columns(x, arg, call = call)
    keys <- .mm_label_key(.mm_paste_key(x, call = call), arg, call = call)
    return(list(keys = keys, col = col))
  }
  if (!is.atomic(x) || !is.null(dim(x)) || length(x) != n) {
    cli::cli_abort(c(
      "{.arg {arg}} must name a column of {.arg spend} or give one value per \\
       row of {.arg spend} ({n}).",
      x = "It has {length(x)} element{?s}."
    ), call = call)
  }
  if (anyNA(x)) {
    cli::cli_abort("{.arg {arg}} must not contain missing values.", call = call)
  }
  list(keys = .mm_key_values(x), col = col)
}

# `periods` as keys comparable with .mm_key_values() of a period column.
#' @keywords internal
#' @noRd
.mm_roi_periods <- function(periods, call = parent.frame()) {
  if (!is.atomic(periods) || !is.null(dim(periods)) || length(periods) == 0L) {
    cli::cli_abort("{.arg periods} must be a vector of one or more periods, \\
                    not {.obj_type_friendly {periods}}.", call = call)
  }
  if (anyNA(periods)) {
    cli::cli_abort("{.arg periods} must not contain missing values.",
                   call = call)
  }
  unique(.mm_key_values(periods))
}

# Without `index`, each group of `spend` must have one row per period the
# contributions cover in that group.
#' @keywords internal
#' @noRd
.mm_roi_check_counts <- function(channel, sel, groups, grouped) {
  channel <- as.character(channel)
  counts <- vapply(groups, function(g) {
    i <- sel$keep_c & sel$c_group == g
    decomposed <- if (any(i)) max(table(channel[i])) else 0L
    c(decomposed, sum(sel$keep_s & sel$s_group == g))
  }, numeric(2), USE.NAMES = FALSE)
  off <- counts[1L, ] != counts[2L, ]
  if (!any(off)) return(invisible(NULL))
  why <- c(
    i = "ROI divides each channel's total contribution by its total spend, so \\
         both must cover the same periods.",
    i = "Pass the rows of {.arg spend} for the periods that were decomposed, \\
         or give {.arg index} to match them period by period."
  )
  if (!grouped) {
    periods <- counts[1L, 1L]
    rows <- counts[2L, 1L]
    cli::cli_warn(c(
      "{.arg contributions} cover {periods} period{?s}, but {.arg spend} has \\
       {rows} row{?s}.",
      why
    ))
    return(invisible(NULL))
  }
  shown <- sprintf("%s (%d periods, %d rows)", .mm_show_key(groups[off]),
                   as.integer(counts[1L, off]), as.integer(counts[2L, off]))
  cli::cli_warn(c(
    "In {sum(off)} group{?s}, {.arg contributions} and {.arg spend} cover \\
     different numbers of periods.",
    i = "Affected: {.val {utils::head(shown, 5)}}.",
    why
  ))
  invisible(NULL)
}

#' @rdname roi
#' @export
mroi <- function(spend_level, coefficient, type = "hill", delta = 0.01, ...) {
  spend_level <- .mm_check_scalar(spend_level, "spend_level", lower = 0)
  coefficient <- .mm_check_scalar(coefficient, "coefficient")
  delta <- .mm_check_scalar(delta, "delta", lower = 0, upper = 1,
                            inclusive = c(FALSE, TRUE))
  type <- .mm_curve_type(type)
  .mm_slope(spend_level, coefficient, type, delta, ...)
}

# Central-difference slope of `coefficient * saturate(x)`, vectorised over x.
#' @keywords internal
#' @noRd
.mm_slope <- function(x, coefficient, type, delta, ...) {
  # The step has a floor so that the slope at zero spend is defined. The floor
  # is relative to the curve's spend scale, since an absolute one is far too
  # wide for a curve fitted on tiny spend values. For a Hill curve with
  # shape 1 it gives a relative error of about 3e-6 at zero, at any scale.
  scale <- .mm_curve_scale(type, list(...), default = 1)
  h <- pmax(x * delta, .Machine$double.eps^(1 / 3) * scale)
  # Clamp the lower point at zero, since spend cannot be negative, and divide
  # by the interval spanned. Dividing by `h` regardless would halve the answer
  # at `spend_level = 0`, the query that asks what the first pound into a dark
  # channel returns.
  lo_s <- pmax(0, x - h / 2)
  hi_s <- x + h / 2
  lo <- .mm_curve(lo_s, type, ...)
  hi <- .mm_curve(hi_s, type, ...)
  coefficient * (hi - lo) / (hi_s - lo_s)
}

#' @keywords internal
#' @noRd
.mm_curve_type <- function(type, call = parent.frame()) {
  types <- c("hill", "exponential", "michaelis_menten", "power", "none")
  if (!.mm_is_string(type) || !type %in% types) {
    cli::cli_abort("{.arg type} must be one of {.val {types}}.", call = call)
  }
  type
}

# The saturation curve, with "none" as the identity, as in media_transform().
#' @keywords internal
#' @noRd
.mm_curve <- function(x, type, ...) {
  if (identical(type, "none")) return(x)
  saturate(x, type = type, ...)
}

# The spend level that sets a curve's scale: `half_max`, `km`, or the
# half-life of `rate`. `default` when the curve has none.
#' @keywords internal
#' @noRd
.mm_curve_scale <- function(type, args, default = NULL) {
  s <- switch(type,
              hill = args$half_max,
              michaelis_menten = args$km,
              exponential = if (is.numeric(args$rate)) log(2) / args$rate,
              NULL)
  if (is.numeric(s) && length(s) == 1L && is.finite(s) && s > 0) s else default
}

#' Response curve for a channel
#'
#' Predicted response across a range of adstocked spend, derived from the
#' fitted saturation parameters. `spend_for()` is the inverse: the adstocked
#' spend that achieves a target response.
#'
#' @param spend Numeric vector of adstocked spend levels to evaluate, in the
#'   units the saturation curve was fitted on (see *Details*), finite and
#'   non-negative. `response_curve()` only; `spend_for()` has no such
#'   argument.
#' @param coefficient The channel's fitted coefficient.
#' @param type Saturation curve type, as in [saturate()], or `"none"` for a
#'   linear response.
#' @param target Target response level. `spend_for()` only. It must lie on
#'   the side of the response at zero spend that spending moves towards:
#'   above it for a positive coefficient, below it for a negative one.
#' @param max_spend Upper bound of the search interval for `spend_for()`.
#'   Defaults to 1000 times the curve's spend-scaled parameter (`half_max`,
#'   `km`, or `log(2)/rate`). `saturate_power()` and `"none"` have no such
#'   parameter, so `max_spend` is required there.
#' @param ... Passed to [saturate()], for example `half_max` and `shape`.
#'
#' @return `response_curve()` returns a data frame with columns `spend` (the
#'   levels given), `response` (the coefficient times the curve at each level)
#'   and `marginal` (the slope of `response` there, as [mroi()] computes it),
#'   one row per element of `spend` and in the same order. It has class
#'   `mm_response_curve`, with a [plot()][plot.mm_response_curve] method.
#'   `spend_for()` returns a single adstocked spend level, or `NA_real_` with a
#'   warning when the target is not reachable within `max_spend`. A target
#'   that no amount of spend can reach, such as a negative response from a
#'   channel with a positive coefficient, is an error.
#'
#' @details
#' `spend` is the saturation curve's input. When the regressor came from
#' [media_transform()], that is adstocked spend: the media pressure in a
#' period, carryover included, rather than the spend of that period alone.
#' Under a normalised kernel, the default, a steady spend per period settles
#' at the same adstocked level, so a level can be read as the steady spend
#' per period that holds the channel there.
#'
#' Both functions describe the *response curve the model fitted*, which is not
#' the same thing as what would happen if you actually spent that much. The
#' curve is identified only over the range of spend the data contains; asking
#' a saturated Hill curve what happens at ten times the observed maximum
#' returns a number, and that number is extrapolation. `spend_for()` returns
#' `NA` rather than a fabricated answer when the target lies beyond
#' `max_spend`, but it cannot tell you that a reachable target is outside the
#' data's support. Check the range of the adstocked series yourself;
#' [plot()][plot.mm_response_curve] shades the levels outside it when given
#' the series as `observed`.
#'
#' @seealso [plot.mm_response_curve()], [saturate()], [mroi()], and
#'   [marginal_roi()] for the marginal return on raw spend through the full
#'   transform
#'
#' @examples
#' # The curve mm_weekly's television was simulated with: coefficient 5200,
#' # half_max 2250 and shape 1.6, on adstocked weekly spend.
#' data(mm_weekly)
#' truth <- attr(mm_weekly, "truth")
#' c(truth$beta[["tv"]], truth$half_max[["tv"]], truth$shape[["tv"]])
#'
#' # A concave curve: marginal return falls from the first pound onward.
#' concave <- response_curve(
#'   spend = seq(0, 5000, by = 1000),
#'   coefficient = 5200, half_max = 2250, shape = 1
#' )
#' concave
#' all(diff(concave$marginal) < 0)
#'
#' # An S-curve: marginal return rises up to the curve's steepest point, near
#' # 900 here, and falls after it. Below that point each extra pound returns
#' # more than the one before, so the curve is not yet saturated there; whether
#' # the channel deserves more budget depends on how its marginal return
#' # compares with other channels' and with break-even.
#' s_curve <- response_curve(
#'   spend = seq(0, 5000, by = 500),
#'   coefficient = 5200, half_max = 2250, shape = 1.6
#' )
#' s_curve
#' s_curve$spend[which.max(s_curve$marginal)]
#'
#' # What adstocked spend achieves a response of 2000?
#' spend_for(target = 2000, coefficient = 5200, half_max = 2250, shape = 1.6)
#'
#' # An unbounded curve needs an explicit search range
#' spend_for(target = 200000, coefficient = 5200, type = "power",
#'           exponent = 0.5, max_spend = 1e6)
#' @export
response_curve <- function(spend, coefficient, type = "hill", ...) {
  spend <- .mm_check_numeric(spend, "spend", allow_na = FALSE, finite = TRUE)
  if (any(spend < 0)) cli::cli_abort("{.arg spend} must be non-negative.")
  coefficient <- .mm_check_scalar(coefficient, "coefficient")
  type <- .mm_curve_type(type)
  resp <- coefficient * .mm_curve(spend, type, ...)
  marg <- .mm_slope(spend, coefficient, type, delta = 0.01, ...)
  out <- data.frame(spend = spend, response = resp, marginal = marg,
                    stringsAsFactors = FALSE)
  class(out) <- c("mm_response_curve", "data.frame")
  out
}

#' @rdname response_curve
#' @export
spend_for <- function(target, coefficient, type = "hill", max_spend = NULL,
                      ...) {
  target <- .mm_check_scalar(target, "target")
  coefficient <- .mm_check_scalar(coefficient, "coefficient")
  type <- .mm_curve_type(type)
  args <- list(...)
  if (is.null(max_spend)) {
    # A sensible upper bracket depends on the curve. Only Hill,
    # Michaelis-Menten and the exponential carry a spend-scaled parameter; for
    # the others there is nothing to infer a scale from, so say so rather than
    # silently searching up to 1000.
    base <- .mm_curve_scale(type, args)
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

  f <- function(s) coefficient * .mm_curve(s, type, ...) - target
  f0 <- f(0)
  if (!is.finite(f0)) return(NA_real_)
  if (f0 == 0) return(0)

  # Spend moves the response away from its value at zero spend in one
  # direction only: up for a positive coefficient, down for a negative one.
  # A target on the other side is out of reach at any spend.
  at_zero <- f0 + target
  if (coefficient == 0 || sign(target - at_zero) != sign(coefficient)) {
    cli::cli_abort(c(
      "No amount of spend reaches the target response.",
      x = if (coefficient == 0) {
        "A coefficient of 0 gives a response of {signif(at_zero, 6)} at any \\
         spend; the target is {signif(target, 6)}."
      } else {
        "The response at zero spend is {signif(at_zero, 6)}, and spend can \\
         only {if (coefficient > 0) 'raise' else 'lower'} it; the target is \\
         {signif(target, 6)}."
      }
    ))
  }

  # Works in both directions: a negative coefficient makes the response
  # decreasing, and a decreasing curve can still cross a negative target.
  fmax <- f(max_spend)
  if (!is.finite(fmax)) return(NA_real_)
  if (sign(f0) == sign(fmax)) {
    reachable <- coefficient * .mm_curve(max_spend, type, ...)
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
  # uniroot()'s default tolerance is absolute, about 1e-4, which loses a root
  # that small spend levels or a small target put near zero. A tolerance
  # relative to the search interval keeps the root's relative accuracy.
  stats::uniroot(f, interval = c(0, max_spend),
                 tol = .Machine$double.eps * max_spend)$root
}

#' @keywords internal
#' @noRd
.mm_coefficients <- function(model, call = parent.frame()) {
  if (is.numeric(model) && !is.null(names(model))) return(model)
  named <- function(cf) is.numeric(cf) && !is.null(names(cf))
  cf <- tryCatch(stats::coef(model), error = function(e) NULL)
  # For a mixed model coef() gives a table of per-group coefficients; the
  # population-level ones are the fixed effects.
  if (!named(cf)) cf <- .mm_fixef(model)
  if (!named(cf)) {
    cli::cli_abort(c(
      "Could not extract named coefficients from {.arg model}.",
      i = "Pass a fitted model that {.fn stats::coef} understands, or a named \\
           numeric vector."
    ), call = call)
  }
  cf
}

# Fixed effects through the generic that nlme defines and lme4 extends. A
# model of either class can only exist once its package, and so nlme, is
# loaded, which is why nothing is loaded here.
#' @keywords internal
#' @noRd
.mm_fixef <- function(model) {
  if (!isNamespaceLoaded("nlme")) return(NULL)
  tryCatch(getExportedValue("nlme", "fixef")(model), error = function(e) NULL)
}
