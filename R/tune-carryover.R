#' Select carryover parameters by cross-validation against a KPI
#'
#' Searches a grid of geometric adstock parameters and returns the pair that
#' best predicts a held-out key performance indicator, using forward-only
#' resampling and a model you supply.
#'
#' @param x Numeric vector of media spend, in time order unless `index` is
#'   given. It must be finite and must vary: a constant series carries no
#'   information about carryover.
#' @param y Numeric vector of the KPI, the same length as `x`.
#' @param fit_fn A function of `(x_adstocked, y)` returning a fitted model.
#'   Defaults to [fit_ols()]. When `controls` are supplied it receives a data
#'   frame instead -- the adstocked media as column `media`, then the
#'   controls -- and the default becomes least squares on all of them. For
#'   several channels at once, see [tune_carryover_joint()].
#' @param predict_fn A function of `(model, newdata)` returning predictions,
#'   where `newdata` is the adstocked media for the assessment rows. Defaults to
#'   [stats::predict()].
#' @param max_lags Integer vector of candidate kernel lengths. `Inf` is allowed
#'   and denotes the infinite recursive kernel.
#' @param decays Numeric vector of candidate decay coefficients in `[0, 1]`.
#' @param normalise Should the adstock kernel be normalised to sum to 1? See
#'   [adstock_geometric()].
#' @param scheme Resampling scheme. `"rolling_origin"` (the default) grows the
#'   training window one step at a time and assesses on the periods immediately
#'   following. `"k_fold_forward"` splits the series into `k` contiguous forward
#'   blocks. Both are forward-only; neither ever trains on data that follows
#'   what it assesses.
#' @param metric_fn A function of `(actual, predicted)` returning a single
#'   number to be minimised. Defaults to [rmse()].
#' @param initial Size of the first training window, in periods. Defaults to
#'   half the series for `scheme = "rolling_origin"`, and to
#'   `floor(n / (k + 1))` for `scheme = "k_fold_forward"`, in both cases
#'   rounded down.
#' @param assess Number of periods assessed per split. Used by
#'   `scheme = "rolling_origin"` only; the forward-block scheme's assessment
#'   sizes are determined by `k` and the series length.
#' @param skip Number of splits to skip between evaluations, to make a long
#'   series cheaper. `0` evaluates every split. Used by
#'   `scheme = "rolling_origin"` only.
#' @param k Number of forward blocks when `scheme = "k_fold_forward"`.
#'   Ignored otherwise.
#' @param controls Optional data frame of control variables (trend, price,
#'   seasonality, ...) entered into the model untransformed, so carryover is
#'   judged with them accounted for. The rows are kept aligned with every split
#'   automatically. Factor, character and logical columns are expanded to
#'   treatment dummies by [stats::model.matrix()] and every name is made
#'   syntactic, so `fit_fn` sees columns such as `seasonQ2` rather than
#'   `season`. A column with a single value adds nothing beside an intercept
#'   and is dropped with a message. Missing or non-finite values are an error
#'   that names the column and rows: impute them, or drop those periods from
#'   every input.
#' @param warm_start Seed the adstock with [adstock_steady_state()] rather
#'   than zero, removing the start-up bias of a long carryover. The seed
#'   averages spend over no more than the first training window, so it uses
#'   only media that every split trains on.
#' @param cores Number of cores. Values above 1 use [parallel::mclapply()] on
#'   Unix-alikes and a socket cluster elsewhere. The workers of a socket
#'   cluster are new R sessions that load \pkg{mediamix} and nothing else, so
#'   there `fit_fn`, `predict_fn` and `metric_fn` must call other packages with
#'   `pkg::fun()` and must not rely on helpers defined in your session.
#' @param aggregate How to turn the assessment sets into one score.
#'   `"pooled"` (the default) collects every out-of-sample prediction across
#'   splits and evaluates `metric_fn` once on the lot, so `rmse` really is the
#'   root mean squared forecast error. `"mean"` evaluates `metric_fn` on each
#'   split and averages, which is the \pkg{tune} convention. With
#'   `assess = 1` a per-split RMSE is an absolute error, so the mean of them is
#'   a mean absolute error whatever `metric_fn` is called.
#' @param by,index Panel data. `by` assigns each row to a series, as a vector
#'   or a data frame of key columns, and `index` gives each row's period, as a
#'   number, [Date] or [POSIXct]. With `index` the rows may come in any order.
#'   `by` needs `index`. See *Panel data*.
#' @param se_block Number of consecutive splits the jackknife leaves out
#'   together when it computes `std_err` and the one-standard-error rule. `1`,
#'   the default, treats the splits as independent. Successive forecasts share
#'   most of their training data, so their errors are usually correlated and
#'   independence understates the standard error; a block of a few splits
#'   allows for that. `best` does not depend on it.
#'
#' @return An object of class `mm_carryover`: a list with elements
#'   \describe{
#'     \item{`best`}{One-row data frame with the selected `max_lag` and `decay`,
#'       plus the corresponding `half_life`, `metric`, `std_err` and
#'       `n_splits`.}
#'     \item{`best_1se`}{The same, for the candidate chosen by the
#'       one-standard-error rule. See *Choosing among near-ties*.}
#'     \item{`results`}{Data frame of the full grid: `max_lag`, `decay`,
#'       `half_life`, `metric`, `std_err` and `n_splits`. `half_life` is `NA`
#'       where the kernel carries nothing over (`decay = 0` or `max_lag = 1`)
#'       or never decays (`decay = 1`). `std_err` is the jackknife standard
#'       error of `metric` over splits; for `aggregate = "mean"` with
#'       `se_block = 1` it is `sd / sqrt(n_splits)` of the per-split values.
#'       `n_splits` is the number of splits every candidate was scored on.}
#'     \item{`metric`}{The cross-validated metric of `best`.}
#'     \item{`metric_name`}{Name of the metric function used.}
#'     \item{`aggregate`}{How the splits were combined.}
#'     \item{`scheme`}{The resampling scheme used.}
#'     \item{`n_splits`}{Number of resampling splits formed.}
#'     \item{`n_ok`}{Number of splits every candidate was scored on. See
#'       *Failed fits*.}
#'     \item{`n_series`}{Number of series, more than 1 only with `by`.}
#'     \item{`normalise`}{Whether the adstock kernel was normalised, carried
#'       through so the selected decay can be reapplied on the same footing.}
#'   }
#'   `as.data.frame()` and `tidy()` return `results` with two logical columns,
#'   `best` and `best_1se`, marking the two selections.
#'
#' @section What this fixes:
#' It is tempting to choose carryover parameters by asking which transformed
#' series is best-behaved -- smoothest, most trend-like, easiest to extrapolate.
#' That criterion has no dependent variable in it, and it has a defect that is
#' easy to miss: the objective is monotone in how hard the filter smooths, so it
#' drifts toward the longest lag and highest decay in whatever grid it is given.
#' The chosen parameters then say more about the grid's upper corner than about
#' the media.
#'
#' The fix is to put the KPI in the loop. Here every candidate `(max_lag, decay)`
#' is scored by how well a model fitted on adstocked media predicts *held-out
#' KPI*, so the winner is the carryover structure that actually improves
#' forecasts of the thing being modelled. It also means the answer depends on
#' the model you intend to fit, which is a feature: carryover and the rest of
#' the specification are not separable, and pretending otherwise is how a
#' three-week half-life turns into a nine-week one once seasonality is added.
#'
#' @section Why full-series adstock is safe here:
#' The media series is adstocked once per grid point and then sliced into folds,
#' rather than being re-adstocked inside each fold. That is valid because the
#' filter is causal: the adstock value at time \eqn{t} depends only on media at
#' times \eqn{\le t}. Assessment-period regressors therefore draw on training
#' -period *media*, which is information genuinely available at prediction time,
#' and never on future media. No KPI information crosses the boundary at any
#' point. The `adstock(x)[1:m] == adstock(x[1:m])` invariant is enforced by the
#' test suite precisely because this shortcut depends on it.
#'
#' @section Choosing among near-ties:
#' Carryover is weakly identified in most media data: the cross-validation
#' curve is often nearly flat across a wide band of decay, and the minimum
#' moves around from sample to sample. `best` reports the minimum; `best_1se`
#' applies the one-standard-error rule (Breiman et al., 1984; Hastie,
#' Tibshirani and Friedman, 2009, section 7.10), choosing the *shortest*
#' carryover -- lowest half-life, then shortest kernel -- that the
#' resampling cannot distinguish from the best candidate. When the two
#' disagree, the shorter carryover is the more conservative claim.
#'
#' The comparison is *paired* and is made on the quantity `best` minimises.
#' Every candidate is scored on the same splits, and a candidate qualifies when
#' its `metric` exceeds that of `best` by no more than one standard error of
#' the *difference*. The standard error comes from a jackknife over splits:
#' both metrics are recomputed with each block of `se_block` consecutive
#' splits left out. For `aggregate = "mean"` with `se_block = 1` it is the
#' standard error of the mean per-split difference; for the pooled RMSE with
#' one-period assessment sets it agrees, to first order, with a paired
#' comparison of the per-forecast squared errors. The textbook unpaired
#' version compares against the standard error of the best candidate's own
#' metric, which on time-series splits is dominated by how hard each period is
#' to forecast, a cost common to every candidate, and so admits nearly the
#' whole grid. The `std_err` column reports that unpaired standard error, as a
#' measure of forecast noise.
#'
#' @section Failed fits:
#' When `fit_fn`, `predict_fn` or `metric_fn` fails on a split for some
#' candidates, or returns missing predictions, that split is left out for
#' every candidate, so all of them are compared on the same forecasts. A
#' warning quotes the first failure and `n_ok` counts the splits that remain.
#' Splits whose assessment periods have no KPI are left out without a warning.
#' When every candidate fails, the error quotes the first failure and names
#' the function that raised it.
#'
#' @section Start-up bias:
#' Adstock is cold-started at zero, so the first few periods of a
#' long-carryover channel are understated: with `decay = 0.85` a constant spend
#' reaches only 15% of its steady-state level in week one and 90% after
#' `effective_window(0.85)` = 15 weeks. With `initial` at half the series this
#' rarely decides the answer, but it biases the fit towards short carryover on
#' short series. `warm_start = TRUE` seeds every candidate with
#' [adstock_steady_state()], averaging spend over `effective_window(decay)`
#' periods or the first training window, whichever is shorter, so the seed
#' uses only media that every split trains on. Better still, pass real
#' pre-period spend through [adstock_state()] and your own `fit_fn` when you
#' have it.
#'
#' @section Panel data:
#' With `by` and `index`, `x`, `y` and `controls` hold several series stacked,
#' such as the geographies of [mm_weekly]. Each series is adstocked on its own
#' in `index` order, so carryover never runs from one series into the next.
#' Splits are formed on the distinct values of `index`: `initial`, `assess`,
#' `skip` and `k` count periods rather than rows, every split trains on the
#' same periods of every series, and it assesses the periods that follow. One
#' model is fitted to all the series at once and the metric pools their
#' forecasts; pass the series as a control to give each its own intercept.
#' A series has at most one row per period, and its consecutive rows are
#' taken to be consecutive periods. Series may start and end in different
#' periods; with `warm_start`, a series that starts after the first training
#' window is cold-started.
#'
#' @references
#' Breiman, L., Friedman, J., Olshen, R. and Stone, C. (1984).
#' *Classification and Regression Trees*. Wadsworth.
#'
#' Hastie, T., Tibshirani, R. and Friedman, J. (2009). *The Elements of
#' Statistical Learning* (2nd ed.). Springer.
#'
#' Bergmeir, C. and Benitez, J. M. (2012). On the use of cross-validation for
#' time series predictor evaluation. *Information Sciences*, 191, 192--213.
#'
#' @seealso [tune_carryover_joint()] for several channels at once,
#'   [adstock_geometric()], [fit_ols()], [effective_window()]
#'
#' @examples
#' # Recover a known decay from data simulated with it
#' set.seed(42)
#' n <- 120
#' spend <- pmax(0, rnorm(n, 500, 250))
#' truth <- 0.7
#' kpi <- 200 + 0.5 * adstock_geometric(spend, decay = truth) + rnorm(n, sd = 8)
#'
#' tuned <- tune_carryover(
#'   spend, kpi,
#'   max_lags = Inf,
#'   decays = seq(0.1, 0.9, by = 0.1)
#' )
#' tuned
#' tuned$best$decay
#'
#' # The shortest carryover the data cannot distinguish from the best
#' tuned$best_1se$decay
#'
#' # Tune against a model with controls rather than the bare default. Passed
#' # as `controls`, the trend and seasonality are fitted inside every training
#' # window, so no split learns from KPI values that come after it.
#' season <- sin(2 * pi * seq_len(n) / 52)
#' trend <- seq_len(n)
#' kpi2 <- kpi + 40 * trend + 300 * season
#'
#' bare <- suppressWarnings(tune_carryover(
#'   spend, kpi2, max_lags = Inf, decays = seq(0.1, 0.9, by = 0.1)
#' ))
#' with_controls <- tune_carryover(
#'   spend, kpi2, max_lags = Inf, decays = seq(0.1, 0.9, by = 0.1),
#'   controls = data.frame(trend, season)
#' )
#'
#' # The truth is 0.7. Ignoring the trend and seasonality moves the answer.
#' c(truth = 0.7, bare = bare$best$decay, controls = with_controls$best$decay)
#' @export
tune_carryover <- function(x, y,
                           fit_fn = fit_ols,
                           predict_fn = stats::predict,
                           max_lags = Inf,
                           decays = seq(0.1, 0.9, by = 0.05),
                           normalise = TRUE,
                           scheme = c("rolling_origin", "k_fold_forward"),
                           metric_fn = rmse,
                           initial = NULL,
                           assess = 1L,
                           skip = 0L,
                           k = 5L,
                           cores = 1L,
                           aggregate = c("pooled", "mean"),
                           controls = NULL,
                           warm_start = FALSE,
                           by = NULL,
                           index = NULL,
                           se_block = 1L) {
  scheme <- match.arg(scheme)
  aggregate <- match.arg(aggregate)
  metric_name <- .mm_fn_label(substitute(metric_fn))

  x <- .mm_check_numeric(x, "x", allow_na = FALSE, finite = TRUE)
  y <- .mm_check_numeric(y, "y")
  if (length(x) != length(y)) {
    cli::cli_abort("{.arg x} ({length(x)}) and {.arg y} ({length(y)}) must be \\
                    the same length.")
  }
  if (!is.function(fit_fn)) cli::cli_abort("{.arg fit_fn} must be a function.")
  if (!is.function(predict_fn)) cli::cli_abort("{.arg predict_fn} must be a function.")
  if (!is.function(metric_fn)) cli::cli_abort("{.arg metric_fn} must be a function.")
  .mm_check_flag(normalise, "normalise")
  .mm_check_flag(warm_start, "warm_start")
  n <- length(x)
  if (all(x == x[1L])) {
    cli::cli_abort(c(
      "{.arg x} is constant.",
      i = "A series that never changes carries no information about carryover."
    ))
  }
  layout <- .mm_tune_layout(n, by, index)
  ctrl <- .mm_control_matrix(controls, n, reserved = "media")

  max_lags <- .mm_check_lag_grid(max_lags)
  decays <- .mm_check_decay_grid(decays)
  se_block <- .mm_check_count(se_block, "se_block", min = 1L)

  periods <- .mm_make_splits(layout$n_periods, scheme = scheme,
                             initial = initial, assess = assess, skip = skip,
                             k = k, call = environment())
  if (length(periods) == 0L) {
    cli::cli_abort(c(
      "No usable resampling splits.",
      i = "The series has {layout$n_periods} period{?s}; {.arg initial} and \\
           {.arg assess} leave nothing to assess on."
    ))
  }
  .mm_check_se_block(se_block, length(periods))
  splits <- .mm_split_rows(periods, layout)
  warm_periods <- length(periods[[1L]]$train)
  grid <- .mm_tuning_grid(max_lags, decays, normalise, warm_start)

  default_model <- identical(fit_fn, fit_ols) &&
    identical(predict_fn, stats::predict)
  fast <- default_model && is.null(ctrl)
  y_cv <- .mm_cv_rows(y, layout)
  ctrl_cv <- if (is.null(ctrl)) NULL else .mm_cv_rows(ctrl, layout)
  ctrl_mat <- if (is.null(ctrl)) NULL else as.matrix(ctrl_cv)

  eval_one <- function(i) {
    z <- .mm_adstock_cv(x, grid$decay[i], grid$max_lag[i], normalise,
                        warm_start, warm_periods, layout)
    folds <- if (fast) .mm_cv_fast(z, y_cv, splits) else NULL
    if (!is.null(folds)) return(folds)
    # The fast path declined this grid point as numerically unsafe, or a
    # model other than the bare default is in use.
    if (default_model && !is.null(ctrl)) {
      return(.mm_cv_ols(cbind(media = z, ctrl_mat), y_cv, splits))
    }
    design <- if (is.null(ctrl)) z else cbind(data.frame(media = z), ctrl_cv)
    .mm_cv_general(design, y_cv, splits, fit_fn, predict_fn)
  }

  raw <- .mm_maybe_parallel(seq_len(nrow(grid)), eval_one, cores,
                            call = environment())
  sets <- .mm_worker_failures(raw, length(splits))
  .mm_warn_worker_failures(sets)
  sc <- .mm_cv_compare(sets$folds, y_cv, splits, metric_fn, aggregate,
                       se_block)
  .mm_check_scores(sc)
  .mm_warn_lost_splits(sc$lost, sc$n_ok, sc$first_error)

  results <- data.frame(
    max_lag = grid$max_lag,
    decay = grid$decay,
    half_life = .mm_grid_half_life(grid$decay, grid$max_lag),
    metric = sc$metric,
    std_err = sc$std_err,
    n_splits = sc$n_ok,
    stringsAsFactors = FALSE
  )
  ord <- order(results$metric, results$max_lag, results$decay)
  results <- results[ord, , drop = FALSE]
  rownames(results) <- NULL
  jack <- if (is.null(sc$jack)) NULL else sc$jack[, ord, drop = FALSE]

  best <- results[1L, , drop = FALSE]
  best_1se <- results[.mm_one_se_pick(results, jack, anchor = 1L), ,
                      drop = FALSE]
  rownames(best_1se) <- NULL

  .mm_warn_grid_edge(list(.mm_edge_notes(best$max_lag, best$decay, grid)))

  structure(
    list(best = best, best_1se = best_1se, results = results,
         metric = best$metric, metric_name = metric_name,
         aggregate = aggregate, scheme = scheme,
         n_splits = length(splits), n_ok = sc$n_ok,
         n_series = layout$n_series, normalise = normalise),
    class = "mm_carryover"
  )
}

#' @export
print.mm_carryover <- function(x, ...) {
  cli::cli_h3("Carryover tuning")
  scored <- if (isTRUE(x$n_ok < x$n_splits)) {
    sprintf(" (%d scored)", x$n_ok)
  } else ""
  series <- if (isTRUE(x$n_series > 1L)) {
    sprintf(" over %d series", x$n_series)
  } else ""
  cli::cli_text("{x$scheme} resampling{series}, {x$n_splits} split{?s}\\
                 {scored}, {nrow(x$results)} grid point{?s}")
  b <- x$best
  b_note <- .mm_carry_note(b)
  b_lag <- .mm_lag_text(b)
  cli::cli_text("Best: {.field decay} = {round(b$decay, 4)}{b_note}, \\
                 {.field max_lag} = {b_lag}")
  se_txt <- if (is.finite(b$std_err)) {
    sprintf(", std. error %s", format(signif(b$std_err, 3)))
  } else ""
  cli::cli_text("{x$metric_name} ({x$aggregate}) = {signif(b$metric, 6)}\\
                 {se_txt}")
  o <- x$best_1se
  if (!is.null(o) && (o$decay != b$decay || o$max_lag != b$max_lag)) {
    o_note <- .mm_carry_note(o)
    o_lag <- .mm_lag_text(o)
    cli::cli_text("One-SE rule: {.field decay} = {round(o$decay, 4)}{o_note}, \\
                   {.field max_lag} = {o_lag}. The data cannot separate the \\
                   two.")
  }
  invisible(x)
}

#' @export
as.data.frame.mm_carryover <- function(x, row.names = NULL, optional = FALSE,
                                       ...) {
  out <- x$results
  out$best <- .mm_same_choice(out, x$best)
  out$best_1se <- .mm_same_choice(out, x$best_1se)
  if (!is.null(row.names)) rownames(out) <- row.names
  out
}

#' @exportS3Method generics::tidy
tidy.mm_carryover <- function(x, ...) {
  as.data.frame(x)
}

# ---- printing helpers ----------------------------------------------------------

#' @keywords internal
#' @noRd
.mm_lag_text <- function(row) {
  if (is.infinite(row$max_lag)) "infinite" else format(row$max_lag)
}

# The parenthesis after a selected decay: its half-life, or a plain statement
# when the kernel carries nothing over.
#' @keywords internal
#' @noRd
.mm_carry_note <- function(row) {
  if (row$max_lag == 1 || row$decay == 0) return(" (no carryover)")
  if (!is.finite(row$half_life)) return("")
  sprintf(" (half-life %s periods)", format(round(row$half_life, 2)))
}

# Which rows of a table share a selection's (channel,) max_lag and decay.
#' @keywords internal
#' @noRd
.mm_same_choice <- function(table, chosen) {
  key <- function(d) {
    parts <- list(d$max_lag, d$decay)
    if (!is.null(d$channel)) parts <- c(list(d$channel), parts)
    do.call(paste, c(parts, list(sep = "\r")))
  }
  key(table) %in% key(chosen)
}

# ---- resampling --------------------------------------------------------------

#' @keywords internal
#' @noRd
.mm_make_splits <- function(n, scheme, initial, assess, skip, k,
                            call = parent.frame()) {
  assess <- .mm_check_count(assess, "assess", min = 1L, call = call)
  skip <- .mm_check_count(skip, "skip", min = 0L, call = call)

  if (scheme == "rolling_origin") {
    if (is.null(initial)) initial <- max(2L, floor(n / 2))
    initial <- .mm_check_count(initial, "initial", min = 2L, call = call)
    if (initial >= n) {
      cli::cli_abort(c(
        "{.arg initial} ({initial}) must be less than the series length ({n}).",
        i = "There would be nothing left to assess on."
      ), call = call)
    }
    if (initial + assess > n) {
      cli::cli_abort(c(
        "The series is too short for this resampling scheme.",
        x = "{.arg initial} ({initial}) plus {.arg assess} ({assess}) exceeds \\
             the series length ({n}).",
        i = "Lower {.arg initial}, lower {.arg assess}, or use a longer series."
      ), call = call)
    }
    ends <- seq(from = initial, to = n - assess, by = skip + 1L)
    return(lapply(ends, function(e) list(train = seq_len(e),
                                         test = (e + 1L):(e + assess))))
  }

  k <- .mm_check_count(k, "k", min = 1L, call = call)
  if (is.null(initial)) initial <- max(2L, floor(n / (k + 1L)))
  initial <- .mm_check_count(initial, "initial", min = 2L, call = call)
  if (initial >= n) {
    cli::cli_abort(c(
      "{.arg initial} ({initial}) must be less than the series length ({n}).",
      i = "There would be nothing left to assess on."
    ), call = call)
  }
  edges <- unique(as.integer(floor(seq(initial, n, length.out = k + 1L))))
  out <- list()
  for (i in seq_len(length(edges) - 1L)) {
    ts <- edges[i] + 1L
    te <- edges[i + 1L]
    if (ts <= te) {
      out[[length(out) + 1L]] <- list(train = seq_len(edges[i]), test = ts:te)
    }
  }
  out
}

#' @keywords internal
#' @noRd
.mm_check_se_block <- function(se_block, n_splits, call = parent.frame()) {
  if (se_block > 1L && n_splits %/% se_block < 2L) {
    cli::cli_abort(c(
      "{.arg se_block} ({se_block}) leaves fewer than two blocks of splits.",
      i = "{n_splits} split{?s} {?was/were} formed; use a block of at most \\
           {max(1L, n_splits %/% 2L)}."
    ), call = call)
  }
  invisible(se_block)
}

# Rows of each split once the data are in `.mm_cv_rows()` order. Splits are
# formed on periods; this turns each one into the rows of every series that
# fall in those periods. Rows are sorted by period, so a training set is
# always a prefix of the rows, which the prefix-sum fast path relies on.
#' @keywords internal
#' @noRd
.mm_split_rows <- function(splits, layout) {
  if (is.null(layout$period)) return(splits)
  last <- cumsum(tabulate(layout$period, nbins = layout$n_periods))
  lapply(splits, function(s) {
    first_test <- s$test[1L]
    from <- if (first_test > 1L) last[first_test - 1L] + 1L else 1L
    list(train = seq_len(last[s$train[length(s$train)]]),
         test = from:last[s$test[length(s$test)]])
  })
}

# How the rows of a single series or a panel line up in time. Without `index`
# the rows are the periods. With it, `ad_order` sorts each series by period for
# the adstock, `cv_order` sorts all rows by period for resampling, and
# `period` is the rank of each row's period in `cv_order`.
#' @keywords internal
#' @noRd
.mm_tune_layout <- function(n, by, index, call = parent.frame()) {
  if (is.null(by) && is.null(index)) {
    return(list(n_periods = n, n_series = 1L, by = NULL, ad_order = NULL,
                cv_order = NULL, period = NULL))
  }
  if (is.null(index)) {
    cli::cli_abort(c(
      "{.arg by} needs {.arg index}.",
      i = "Splits are formed on the periods every series shares, so the \\
           tuner has to know which rows fall in the same period."
    ), call = call)
  }
  if (!is.null(by)) {
    if (is.data.frame(by)) {
      by <- do.call(paste, c(lapply(by, as.character), list(sep = "\x1f")))
    }
    if (!is.atomic(by) || length(by) != n) {
      cli::cli_abort("{.arg by} must have one value per row ({n}), not \\
                      {length(by)}.", call = call)
    }
    if (anyNA(by)) {
      cli::cli_abort("{.arg by} must not contain missing values.", call = call)
    }
    by <- as.character(by)
  }
  if (!(is.numeric(index) || inherits(index, c("Date", "POSIXct"))) ||
      is.matrix(index)) {
    cli::cli_abort("{.arg index} must be a numeric, {.cls Date} or \\
                    {.cls POSIXct} vector, not {.obj_type_friendly {index}}.",
                   call = call)
  }
  if (length(index) != n) {
    cli::cli_abort("{.arg index} must have one value per row ({n}), not \\
                    {length(index)}.", call = call)
  }
  t <- as.numeric(index)
  if (anyNA(t) || any(!is.finite(t))) {
    cli::cli_abort("{.arg index} must not contain missing or infinite values.",
                   call = call)
  }
  key <- if (is.null(by)) character(n) else by
  ad_order <- order(key, t)
  ks <- key[ad_order]
  ts <- t[ad_order]
  same <- which(c(FALSE, ks[-1L] == ks[-n] & ts[-1L] == ts[-n]))
  if (length(same) > 0L) {
    row <- ad_order[same[1L]]
    where <- if (is.null(by)) "" else
      sprintf(" of series %s", gsub("\x1f", " / ", key[row], fixed = TRUE))
    cli::cli_abort(c(
      "{.arg index} repeats a period{where}.",
      x = "Row {row} has the same period as an earlier row.",
      i = "Each series needs one row per period. Aggregate to one row per \\
           period, or separate the series with {.arg by}."
    ), call = call)
  }
  cv_order <- order(t, key)
  periods <- sort(unique(t))
  list(n_periods = length(periods), n_series = length(unique(key)), by = by,
       ad_order = ad_order, cv_order = cv_order,
       period = match(t[cv_order], periods),
       ad_period = match(t[ad_order], periods))
}

# Rows of a vector or data frame in resampling order.
#' @keywords internal
#' @noRd
.mm_cv_rows <- function(v, layout) {
  o <- layout$cv_order
  if (is.null(o)) return(v)
  if (is.data.frame(v)) {
    out <- v[o, , drop = FALSE]
    rownames(out) <- NULL
    return(out)
  }
  v[o]
}

# One candidate's adstocked media, in resampling order. Each series is
# filtered on its own, in time order, so carryover never crosses from one
# series into another.
#' @keywords internal
#' @noRd
.mm_adstock_cv <- function(x, decay, max_lag, normalise, warm_start,
                           warm_periods, layout) {
  o <- layout$ad_order
  xs <- if (is.null(o)) x else x[o]
  bys <- if (is.null(layout$by)) NULL else layout$by[o]
  st <- if (warm_start) {
    .mm_warm_state(xs, decay, max_lag, warm_periods, by = bys,
                   period = layout$ad_period)
  } else 0
  z <- adstock_geometric(xs, decay = decay, max_lag = max_lag,
                         normalise = normalise, state = st, by = bys)
  if (is.null(o)) return(z)
  out <- numeric(length(z))
  out[o] <- z
  out[layout$cv_order]
}

# A steady-state seed that looks no further ahead than the first training
# window, which every split trains on. adstock_steady_state() alone averages
# the first effective window, which can reach past it on a short series. For
# a panel, `period` gives each row's period, so a series that starts late is
# seeded only from its rows inside that window, and cold-started when it has
# none.
#' @keywords internal
#' @noRd
.mm_warm_state <- function(x, decay, max_lag, horizon, by = NULL,
                           period = NULL) {
  window <- if (decay == 0) 1 else if (decay < 1) {
    ceiling(log(1 - 0.9) / log(decay))
  } else Inf
  if (is.null(by)) {
    return(adstock_steady_state(x, decay, max_lag = max_lag,
                                periods = min(window, horizon)))
  }
  groups <- unique(by)
  out <- lapply(groups, function(g) {
    rows <- which(by == g)
    inside <- sum(period[rows] <= horizon)
    if (inside == 0L) return(0)
    adstock_steady_state(x[rows], decay, max_lag = max_lag,
                         periods = min(window, inside))
  })
  names(out) <- groups
  out
}

# The candidate grid, without the points that cannot be evaluated: an
# infinite kernel at decay = 1 has infinite mass, so it can neither be
# normalised nor reach a steady state to warm-start from.
#' @keywords internal
#' @noRd
.mm_tuning_grid <- function(max_lags, decays, normalise, warm_start,
                            call = parent.frame()) {
  grid <- expand.grid(max_lag = max_lags, decay = decays,
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  drop <- is.infinite(grid$max_lag) & grid$decay == 1 &
    (normalise || warm_start)
  if (any(drop)) {
    why <- if (normalise) "cannot be normalised" else
      "has no steady state to warm-start from"
    cli::cli_inform(
      "Dropping {sum(drop)} grid point{?s} with an infinite kernel at \\
       {.code decay = 1}, which {why}."
    )
    grid <- grid[!drop, , drop = FALSE]
  }
  if (nrow(grid) == 0L) {
    cli::cli_abort("The parameter grid is empty.", call = call)
  }
  rownames(grid) <- NULL
  grid
}

#' @keywords internal
#' @noRd
.mm_grid_half_life <- function(decay, max_lag) {
  out <- rep(NA_real_, length(decay))
  live <- decay > 0 & decay < 1 & max_lag > 1
  out[live] <- log(0.5) / log(decay[live])
  out
}

# ---- fitting each split ---------------------------------------------------------

# Every split's predictions from a user-supplied model. A split that fails
# carries the first line of the reason, naming the function that raised it.
#' @keywords internal
#' @noRd
.mm_cv_general <- function(z, y, splits, fit_fn, predict_fn) {
  rows <- if (is.data.frame(z)) {
    function(i) z[i, , drop = FALSE]
  } else {
    function(i) z[i]
  }
  lapply(splits, function(s) {
    fit <- tryCatch(fit_fn(rows(s$train), y[s$train]),
                    error = function(e) e)
    if (inherits(fit, "error")) {
      return(list(error = paste0("`fit_fn` failed: ", conditionMessage(fit))))
    }
    pred <- tryCatch(predict_fn(fit, rows(s$test)), error = function(e) e)
    if (inherits(pred, "error")) {
      return(list(error = paste0("`predict_fn` failed: ",
                                 conditionMessage(pred))))
    }
    if (length(pred) != length(s$test)) {
      return(list(error = sprintf(
        "`predict_fn` returned %d value%s for %d row%s.", length(pred),
        if (length(pred) == 1L) "" else "s", length(s$test),
        if (length(s$test) == 1L) "" else "s")))
    }
    list(actual = y[s$test], pred = as.numeric(pred))
  })
}

# The default model: least squares with an intercept on the adstocked media
# and the controls, given as one numeric matrix. Media and controls are
# validated finite before they get here, so only a missing KPI drops a row.
# A coefficient that cannot be estimated contributes nothing to the forecast.
#' @keywords internal
#' @noRd
.mm_cv_ols <- function(X, y, splits) {
  X <- cbind(`(Intercept)` = 1, X)
  has_y <- is.finite(y)
  lapply(splits, function(s) {
    rows <- s$train[has_y[s$train]]
    fit <- tryCatch(stats::lm.fit(X[rows, , drop = FALSE], y[rows]),
                    error = function(e) e)
    if (inherits(fit, "error")) {
      return(list(error = paste0("Least squares failed: ",
                                 conditionMessage(fit))))
    }
    cf <- fit$coefficients
    cf[!is.finite(cf)] <- 0
    list(actual = y[s$test],
         pred = as.numeric(X[s$test, , drop = FALSE] %*% cf))
  })
}

# Prefix-sum path for the default OLS fitter: every expanding-window fit is
# O(1) given cumulative sums, so the whole sweep is linear in the series length
# rather than quadratic. Measured at 2x faster on 156 periods and 9x on 2080.
#
# It must be numerically indistinguishable from `.mm_cv_general()` with
# `fit_ols`, or the answer would depend on which fitter the user happened to
# pass. Two things secure that: the coefficients come from the same
# `.mm_ols_from_moments()` the direct fitter uses, and the one place prefix sums
# can lose precision -- a regressor whose variance is tiny next to its mean
# square, where `sum(z^2) - sum(z)^2/n` cancels catastrophically -- is detected
# and handed back to the general path instead of being answered badly.
#' @keywords internal
#' @noRd
.mm_cv_fast <- function(z, y, splits) {
  ok <- is.finite(z) & is.finite(y)
  zz <- ifelse(ok, z, 0)
  yy <- ifelse(ok, y, 0)
  c_n <- cumsum(as.numeric(ok))
  c_z <- cumsum(zz)
  c_y <- cumsum(yy)
  c_zz <- cumsum(zz * zz)
  c_zy <- cumsum(zz * yy)

  n_ok <- c_n[length(c_n)]
  if (n_ok >= 2) {
    spread <- c_zz[length(c_zz)] - c_z[length(c_z)]^2 / n_ok
    if (!is.finite(spread) || spread < 1e-8 * c_zz[length(c_zz)]) {
      return(NULL)
    }
  }

  lapply(splits, function(s) {
    e <- s$train[length(s$train)]
    m <- c_n[e]
    sz <- c_z[e]; sy <- c_y[e]; szz <- c_zz[e]; szy <- c_zy[e]
    sxx <- if (m >= 1) szz - sz * sz / m else NA_real_
    sxy <- if (m >= 1) szy - sz * sy / m else NA_real_
    fit <- .mm_ols_from_moments(m, sz, sy, sxx, sxy, szz)
    list(actual = y[s$test], pred = fit$intercept + fit$slope * z[s$test])
  })
}

# ---- scoring -------------------------------------------------------------------

# Score candidates on the splits all of them completed.
#
# `fold_sets` holds, for each candidate, one entry per split: either
# list(actual, pred) or list(error = <reason>). A split counts for a
# candidate when its predictions are finite wherever the KPI is, and, for
# `aggregate = "mean"`, when its per-split metric is finite. A split with no
# finite KPI to train on or to assess carries no information and is left out
# for everyone. A candidate that completes no split is scored NA; the rest are
# scored only on the splits every one of them completed, so no candidate is
# compared on easier splits than another. `lost` counts the informative
# splits left out that way.
#
# With `se = TRUE`, the metric is also recomputed with each block of
# `se_block` consecutive splits left out. `jack` holds those leave-one-block-out
# values, one row per block and one column per candidate; the jackknife
# standard error of a metric, or of the difference between two, comes from
# their spread.
#' @keywords internal
#' @noRd
.mm_cv_compare <- function(fold_sets, y, splits, metric_fn, aggregate,
                           se_block = 1L, se = TRUE) {
  actuals <- lapply(splits, function(s) y[s$test])
  n_sp <- length(splits)
  n_cand <- length(fold_sets)
  first_error <- NULL
  note <- function(msg) {
    if (is.null(first_error)) first_error <<- msg
  }
  score <- function(a, p) {
    v <- tryCatch(metric_fn(a, p), error = function(e) e)
    if (inherits(v, "error")) {
      note(paste0("`metric_fn` failed: ", conditionMessage(v)))
      return(NA_real_)
    }
    if (!is.numeric(v) || length(v) != 1L) {
      note("`metric_fn` did not return a single number.")
      return(NA_real_)
    }
    as.numeric(v)
  }

  informative <- vapply(splits, function(s) {
    any(is.finite(y[s$test])) && any(is.finite(y[s$train]))
  }, logical(1))
  ok <- matrix(FALSE, n_sp, n_cand)
  vals <- matrix(NA_real_, n_sp, n_cand)
  for (j in seq_len(n_cand)) {
    for (s in which(informative)) {
      f <- fold_sets[[j]][[s]]
      if (!is.null(f$error)) {
        note(f$error)
        next
      }
      if (!all(is.finite(f$pred[is.finite(actuals[[s]])]))) {
        note("`predict_fn` returned missing or non-finite predictions.")
        next
      }
      if (aggregate == "mean") {
        vals[s, j] <- score(actuals[[s]], f$pred)
        if (!is.finite(vals[s, j])) next
      }
      ok[s, j] <- TRUE
    }
  }

  live <- colSums(ok) > 0L
  common <- informative & rowSums(!ok[, live, drop = FALSE]) == 0L
  if (!any(live)) common <- logical(n_sp)
  n_ok <- sum(common)
  out <- list(metric = rep(NA_real_, n_cand), std_err = rep(NA_real_, n_cand),
              jack = NULL, n_ok = n_ok,
              lost = if (any(live)) sum(informative & !common) else 0L,
              n_informative = sum(informative), first_error = first_error)
  if (n_ok == 0L) return(out)

  idx <- which(common)
  n_blocks <- if (se) n_ok %/% se_block else 0L
  blk <- if (n_blocks >= 2L) ceiling(seq_len(n_ok) * n_blocks / n_ok) else NULL
  jack <- if (is.null(blk)) NULL else matrix(NA_real_, n_blocks, n_cand)
  if (aggregate == "pooled") {
    act <- unlist(actuals[idx], use.names = FALSE)
    split_of <- rep(seq_len(n_ok), lengths(actuals[idx]))
  }
  for (j in which(live)) {
    if (aggregate == "mean") {
      v <- vals[idx, j]
      out$metric[j] <- mean(v)
      if (!is.null(blk)) {
        jack[, j] <- (sum(v) - rowsum(v, blk)[, 1L]) / (n_ok - tabulate(blk))
      }
    } else {
      p <- unlist(lapply(fold_sets[[j]][idx], `[[`, "pred"), use.names = FALSE)
      out$metric[j] <- score(act, p)
      if (!is.null(blk) && is.finite(out$metric[j])) {
        b_of <- blk[split_of]
        jack[, j] <- vapply(seq_len(n_blocks), function(b) {
          keep <- b_of != b
          score(act[keep], p[keep])
        }, numeric(1))
      }
    }
    if (!is.null(blk)) out$std_err[j] <- .mm_jack_se(jack[, j])
  }
  out$jack <- jack
  out$first_error <- first_error
  out
}

# Jackknife standard error from leave-one-block-out values.
#' @keywords internal
#' @noRd
.mm_jack_se <- function(theta) {
  g <- length(theta)
  if (g < 2L || anyNA(theta)) return(NA_real_)
  sqrt((g - 1) / g * sum((theta - mean(theta))^2))
}

# Abort when nothing could be scored, quoting the first failure.
#' @keywords internal
#' @noRd
.mm_check_scores <- function(sc, channel = NULL, call = parent.frame()) {
  if (any(is.finite(sc$metric))) return(invisible(sc))
  what <- if (is.null(channel)) "" else sprintf(" for channel %s", channel)
  first <- sc$first_error
  if (sc$n_informative == 0L) {
    cli::cli_abort(c(
      "No split has a finite value of {.arg y} both to train on and to assess.",
      i = "Check that {.arg y} is not missing over most of the series."
    ), call = call)
  }
  if (is.null(first)) {
    cli::cli_abort(c(
      "Every grid point produced a missing metric{what}.",
      i = "Check that {.arg metric_fn} returns a finite number for these \\
           forecasts."
    ), call = call)
  }
  if (sc$lost > 0L && sc$n_ok == 0L) {
    cli::cli_abort(c(
      "No split was completed by every grid point{what}.",
      x = "First failure: {first}",
      i = "Candidates are only compared on splits all of them completed. \\
           Narrow the grid to the candidates the model can fit."
    ), call = call)
  }
  cli::cli_abort(c(
    "Every grid point produced a missing metric{what}.",
    x = "First failure: {first}",
    i = "Check that {.arg fit_fn} and {.arg predict_fn} work on a single \\
         fold."
  ), call = call)
}

#' @keywords internal
#' @noRd
.mm_warn_lost_splits <- function(lost, n_ok, first) {
  if (lost == 0L) return(invisible(NULL))
  cli::cli_warn(c(
    "{lost} split{?s} {?was/were} left out because a fit failed on \\
     {?it/them}.",
    i = "Every candidate is scored on the {n_ok} split{?s} all of them \\
         completed.",
    i = "First failure: {first}"
  ))
  invisible(NULL)
}

# One-standard-error rule, paired and anchored on the reported best. A
# candidate qualifies when its metric exceeds the anchor's by no more than the
# jackknife standard error of the difference between the two; candidates are
# scored on the same splits, so that difference is far less noisy than either
# metric. Among those that qualify, the least carryover wins: lowest decay,
# then shortest kernel, with a one-period kernel counted as no carryover.
# Returns a row index.
#' @keywords internal
#' @noRd
.mm_one_se_pick <- function(results, jack, anchor) {
  m <- results$metric
  if (is.null(jack) || !is.finite(m[anchor])) return(anchor)
  within <- vapply(seq_along(m), function(j) {
    if (j == anchor) return(TRUE)
    if (!is.finite(m[j])) return(FALSE)
    se <- .mm_jack_se(jack[, j] - jack[, anchor])
    is.finite(se) && m[j] - m[anchor] <= se
  }, logical(1))
  cand <- which(within)
  carry <- ifelse(results$max_lag[cand] == 1, 0, results$decay[cand])
  cand[order(carry, results$max_lag[cand], results$decay[cand])][1L]
}

# ---- grid helpers ------------------------------------------------------------

#' @keywords internal
#' @noRd
.mm_check_lag_grid <- function(max_lags, call = parent.frame()) {
  if (!is.numeric(max_lags) || length(max_lags) == 0L || anyNA(max_lags)) {
    cli::cli_abort("{.arg max_lags} must be a non-empty numeric vector without \\
                    missing values.", call = call)
  }
  bad <- is.finite(max_lags) & (max_lags %% 1 != 0 | max_lags < 1)
  if (any(bad)) {
    cli::cli_abort("{.arg max_lags} must contain positive whole numbers or \\
                    {.code Inf}.", call = call)
  }
  unique(as.numeric(max_lags))
}

#' @keywords internal
#' @noRd
.mm_check_decay_grid <- function(decays, call = parent.frame()) {
  if (!is.numeric(decays) || length(decays) == 0L || anyNA(decays)) {
    cli::cli_abort("{.arg decays} must be a non-empty numeric vector without \\
                    missing values.", call = call)
  }
  if (any(decays < 0 | decays > 1)) {
    cli::cli_abort("{.arg decays} must lie in {.code [0, 1]}.", call = call)
  }
  unique(as.numeric(decays))
}

# Which parts of a selection sit on an edge of the grid that was searched.
# Only the values tried at the selected point count, so a decay of 1
# dropped from an infinite kernel does not hide an upper edge. Decay 0 and 1,
# a one-period kernel and an infinite one are bounds of the parameter itself,
# so a choice there says nothing about the grid. Two candidates have no
# interior, so a parameter needs three before its edge means anything. Decay
# has no effect on a one-period kernel, and kernel length none at decay 0.
#' @keywords internal
#' @noRd
.mm_edge_notes <- function(max_lag, decay, grid) {
  notes <- character(0)
  if (max_lag != 1) {
    d <- sort(unique(grid$decay[grid$max_lag == max_lag]))
    if (length(d) >= 3L && decay == d[length(d)] && decay < 1) {
      notes <- c(notes, sprintf(
        "The selected decay (%s) is the largest value searched.",
        format(decay)))
    }
    if (length(d) >= 3L && decay == d[1L] && decay > 0) {
      notes <- c(notes, sprintf(
        "The selected decay (%s) is the smallest value searched.",
        format(decay)))
    }
  }
  if (decay != 0) {
    l <- sort(unique(grid$max_lag[grid$decay == decay]))
    if (length(l) >= 3L && max_lag == l[length(l)] && is.finite(max_lag)) {
      notes <- c(notes, sprintf(
        "The selected max_lag (%s) is the largest value searched.",
        format(max_lag)))
    }
    if (length(l) >= 3L && max_lag == l[1L] && max_lag > 1) {
      notes <- c(notes, sprintf(
        "The selected max_lag (%s) is the smallest value searched.",
        format(max_lag)))
    }
  }
  notes
}

# Warn once for every selection on an edge. `notes` is a list of character
# vectors from .mm_edge_notes(), named by channel for the joint tuner.
#' @keywords internal
#' @noRd
.mm_warn_grid_edge <- function(notes) {
  labels <- names(notes)
  msgs <- character(0)
  for (i in seq_along(notes)) {
    v <- notes[[i]]
    if (length(v) == 0L) next
    if (!is.null(labels) && nzchar(labels[i])) {
      v <- paste0(labels[i], ": ", tolower(substr(v, 1L, 1L)), substring(v, 2L))
    }
    msgs <- c(msgs, v)
  }
  if (length(msgs) == 0L) return(invisible(NULL))
  cli::cli_warn(c(
    "The chosen parameters sit on the edge of the grid.",
    stats::setNames(msgs, rep("*", length(msgs))),
    i = "Widen the grid and re-run. A parameter pinned to a boundary usually \\
         means the optimum is outside it, or that the series is too short to \\
         identify carryover this long."
  ))
  invisible(NULL)
}

# ---- parallel ----------------------------------------------------------------

#' @keywords internal
#' @noRd
.mm_maybe_parallel <- function(idx, fun, cores, call = parent.frame()) {
  cores <- .mm_check_count(cores, "cores", min = 1L, call = call)
  if (cores == 1L) return(lapply(idx, fun))
  avail <- parallel::detectCores(logical = FALSE)
  if (is.finite(avail)) cores <- min(cores, avail)
  if (cores <= 1L) return(lapply(idx, fun))
  each <- .mm_try_each(fun)
  if (.Platform$OS.type == "unix") {
    return(parallel::mclapply(idx, each, mc.cores = cores))
  }
  cl <- parallel::makePSOCKcluster(cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterEvalQ(cl, requireNamespace("mediamix", quietly = TRUE))
  parallel::parLapply(cl, idx, each)
}

# Wrap `fun` so an error in one element comes back as that element's
# try-error. Without it, mclapply()'s prescheduling turns one error into an
# error for every element sent to the same worker. The wrapper's enclosure
# holds `fun` alone, so a socket cluster serialises nothing else.
#' @keywords internal
#' @noRd
.mm_try_each <- function(fun) {
  force(fun)
  function(i) try(fun(i), silent = TRUE)
}

# Turn grid points that failed in a parallel worker into candidates that
# failed on every split, keeping the count and first message for a warning.
#' @keywords internal
#' @noRd
.mm_worker_failures <- function(raw, n_splits) {
  failed <- vapply(raw, function(z) inherits(z, "try-error"), logical(1))
  first <- NULL
  if (any(failed)) {
    msgs <- vapply(raw[failed], function(z) {
      cond <- attr(z, "condition")
      if (is.null(cond)) as.character(z) else conditionMessage(cond)
    }, character(1))
    first <- msgs[1L]
    raw[failed] <- lapply(msgs, function(m) {
      rep(list(list(error = paste0("A parallel worker failed: ", m))),
          n_splits)
    })
  }
  list(folds = raw, n_failed = sum(failed), first = first)
}

#' @keywords internal
#' @noRd
.mm_warn_worker_failures <- function(sets) {
  if (sets$n_failed == 0L) return(invisible(NULL))
  first <- sets$first
  cli::cli_warn(c(
    "{sets$n_failed} grid point{?s} failed in a parallel worker.",
    i = "First error: {first}",
    i = "Re-run with {.code cores = 1} to see the full traceback."
  ))
  invisible(NULL)
}
