#' Select carryover parameters by cross-validation against a KPI
#'
#' Searches a grid of geometric adstock parameters and returns the pair that
#' best predicts a held-out key performance indicator, using forward-only
#' resampling and a model you supply.
#'
#' @param x Numeric vector of media spend in time order.
#' @param y Numeric vector of the KPI, the same length as `x`.
#' @param fit_fn A function of `(x_adstocked, y)` returning a fitted model.
#'   Defaults to [fit_ols()]. It receives only the adstocked media and the
#'   response, so control variables must reach it another way: residualise `y`
#'   against the controls before calling, or capture them from the enclosing
#'   environment. To tune carryover jointly with controls and with the model's
#'   own hyperparameters, use [step_adstock()] inside a \pkg{recipes} pipeline
#'   instead.
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
#' @param initial Size of the first training window. Defaults to half the
#'   series for `scheme = "rolling_origin"`, and to `floor(n / (k + 1))` for
#'   `scheme = "k_fold_forward"`, in both cases rounded down.
#' @param assess Number of periods assessed per split. Used by
#'   `scheme = "rolling_origin"` only; the forward-block scheme's assessment
#'   sizes are determined by `k` and the series length.
#' @param skip Number of splits to skip between evaluations, to make a long
#'   series cheaper. `0` evaluates every split. Used by
#'   `scheme = "rolling_origin"` only.
#' @param k Number of forward blocks when `scheme = "k_fold_forward"`.
#'   Ignored otherwise.
#' @param cores Number of cores. Values above 1 use [parallel::mclapply()] on
#'   Unix-alikes and a socket cluster elsewhere.
#'
#' @return An object of class `mm_carryover`: a list with elements
#'   \describe{
#'     \item{`best`}{One-row data frame with the selected `max_lag` and `decay`,
#'       plus the corresponding `half_life`, `metric` and `n_splits`.}
#'     \item{`results`}{Data frame of the full grid: `max_lag`, `decay`,
#'       `half_life`, `metric` and `n_splits`.}
#'     \item{`metric`}{Name of the metric function used.}
#'     \item{`scheme`}{The resampling scheme used.}
#'     \item{`n_splits`}{Number of resampling splits evaluated.}
#'     \item{`normalise`}{Whether the adstock kernel was normalised, carried
#'       through so the selected decay can be reapplied on the same footing.}
#'   }
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
#' @seealso [adstock_geometric()], [fit_ols()], [effective_window()]
#'
#' @examples
#' # Recover a known decay from data generated with it
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
#' # Tune against a model with controls rather than the bare default. The
#' # cleanest way is to residualise the KPI first, which needs no row
#' # bookkeeping: `tune_carryover()` then sees a response the controls have
#' # already explained away.
#' season <- sin(2 * pi * seq_len(n) / 52)
#' trend <- seq_len(n)
#' kpi2 <- kpi + 40 * trend + 300 * season
#'
#' bare <- suppressWarnings(tune_carryover(
#'   spend, kpi2, max_lags = Inf, decays = seq(0.1, 0.9, by = 0.1)
#' ))
#'
#' controls <- stats::lm(kpi2 ~ trend + season)
#' residualised <- suppressWarnings(tune_carryover(
#'   spend, stats::residuals(controls),
#'   max_lags = Inf, decays = seq(0.1, 0.9, by = 0.1)
#' ))
#'
#' # The truth is 0.7. Ignoring the trend and seasonality moves the answer.
#' c(truth = 0.7, bare = bare$best$decay, residualised = residualised$best$decay)
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
                           cores = 1L) {
  scheme <- match.arg(scheme)
  metric_name <- .mm_fn_label(substitute(metric_fn))

  x <- .mm_check_numeric(x, "x", allow_na = FALSE)
  y <- .mm_check_numeric(y, "y")
  if (length(x) != length(y)) {
    cli::cli_abort("{.arg x} ({length(x)}) and {.arg y} ({length(y)}) must be \\
                    the same length.")
  }
  if (!is.function(fit_fn)) cli::cli_abort("{.arg fit_fn} must be a function.")
  if (!is.function(predict_fn)) cli::cli_abort("{.arg predict_fn} must be a function.")
  if (!is.function(metric_fn)) cli::cli_abort("{.arg metric_fn} must be a function.")
  .mm_check_flag(normalise, "normalise")

  max_lags <- .mm_check_lag_grid(max_lags)
  decays <- .mm_check_decay_grid(decays)
  n <- length(x)

  splits <- .mm_make_splits(n, scheme = scheme, initial = initial,
                            assess = assess, skip = skip, k = k,
                            call = environment())
  if (length(splits) == 0L) {
    cli::cli_abort(c(
      "No usable resampling splits.",
      i = "The series has {n} observation{?s}; {.arg initial} and \\
           {.arg assess} leave nothing to assess on."
    ))
  }

  grid <- expand.grid(max_lag = max_lags, decay = decays,
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  # An infinite kernel with decay = 1 cannot be normalised; drop rather than error.
  drop <- normalise & is.infinite(grid$max_lag) & grid$decay == 1
  if (any(drop)) {
    cli::cli_inform(
      "Dropping {sum(drop)} grid point{?s} with an infinite kernel at \\
       {.code decay = 1}, which cannot be normalised."
    )
    grid <- grid[!drop, , drop = FALSE]
  }
  if (nrow(grid) == 0L) cli::cli_abort("The parameter grid is empty.")

  fast <- identical(fit_fn, fit_ols) && identical(predict_fn, stats::predict)

  eval_one <- function(i) {
    z <- adstock_geometric(x, decay = grid$decay[i], max_lag = grid$max_lag[i],
                           normalise = normalise)
    out <- if (fast) .mm_cv_fast(z, y, splits, metric_fn) else NULL
    # NULL means the fast path declined this grid point as numerically unsafe.
    if (is.null(out)) {
      out <- .mm_cv_general(z, y, splits, fit_fn, predict_fn, metric_fn)
    }
    out
  }

  metrics <- .mm_maybe_parallel(seq_len(nrow(grid)), eval_one, cores,
                                call = environment())
  failed <- vapply(metrics, function(z) inherits(z, "try-error"), logical(1))
  if (any(failed)) {
    first <- conditionMessage(attr(metrics[[which(failed)[1L]]], "condition"))
    cli::cli_warn(c(
      "{sum(failed)} grid point{?s} failed in a parallel worker.",
      i = "First error: {first}",
      i = "Re-run with {.code cores = 1} to see the full traceback."
    ))
    metrics[failed] <- NA_real_
  }
  metrics <- vapply(metrics, function(z) {
    if (length(z) == 1L && is.numeric(z)) as.numeric(z) else NA_real_
  }, numeric(1))

  if (all(is.na(metrics))) {
    cli::cli_abort(c(
      "Every grid point produced a missing metric.",
      i = "Check that {.arg fit_fn} and {.arg predict_fn} work on a single \\
           fold, and that {.arg y} is not constant."
    ))
  }

  results <- data.frame(
    max_lag = grid$max_lag,
    decay = grid$decay,
    half_life = ifelse(grid$decay > 0 & grid$decay < 1,
                       log(0.5) / log(pmin(pmax(grid$decay, 1e-12), 1 - 1e-12)),
                       NA_real_),
    metric = metrics,
    n_splits = length(splits),
    stringsAsFactors = FALSE
  )
  results <- results[order(results$metric, results$max_lag, results$decay), ,
                     drop = FALSE]
  rownames(results) <- NULL

  best <- results[which.min(results$metric), , drop = FALSE]
  rownames(best) <- NULL

  .mm_warn_grid_edge(best, max_lags, decays)

  structure(
    list(best = best, results = results, metric = metric_name,
         scheme = scheme, n_splits = length(splits), normalise = normalise),
    class = "mm_carryover"
  )
}

#' @export
print.mm_carryover <- function(x, ...) {
  cli::cli_h3("Carryover tuning")
  cli::cli_text("{x$scheme} resampling, {x$n_splits} split{?s}, \\
                 {nrow(x$results)} grid point{?s}")
  b <- x$best
  lag_txt <- if (is.infinite(b$max_lag)) "infinite" else format(b$max_lag)
  cli::cli_text("Best: {.field decay} = {round(b$decay, 4)} \\
                 ({.field half-life} {round(b$half_life, 2)} periods), \\
                 {.field max_lag} = {lag_txt}")
  cli::cli_text("{x$metric} = {signif(b$metric, 6)}")
  invisible(x)
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
.mm_cv_general <- function(z, y, splits, fit_fn, predict_fn, metric_fn) {
  vals <- vapply(splits, function(s) {
    fit <- try(fit_fn(z[s$train], y[s$train]), silent = TRUE)
    if (inherits(fit, "try-error")) return(NA_real_)
    pred <- try(predict_fn(fit, z[s$test]), silent = TRUE)
    if (inherits(pred, "try-error") || length(pred) != length(s$test)) {
      return(NA_real_)
    }
    val <- try(metric_fn(y[s$test], as.numeric(pred)), silent = TRUE)
    if (inherits(val, "try-error") || length(val) != 1L) return(NA_real_)
    as.numeric(val)
  }, numeric(1))
  if (all(is.na(vals))) NA_real_ else mean(vals, na.rm = TRUE)
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
.mm_cv_fast <- function(z, y, splits, metric_fn) {
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

  vals <- vapply(splits, function(s) {
    e <- s$train[length(s$train)]
    m <- c_n[e]
    sz <- c_z[e]; sy <- c_y[e]; szz <- c_zz[e]; szy <- c_zy[e]
    sxx <- if (m >= 1) szz - sz * sz / m else NA_real_
    sxy <- if (m >= 1) szy - sz * sy / m else NA_real_
    fit <- .mm_ols_from_moments(m, sz, sy, sxx, sxy, szz)
    pred <- fit$intercept + fit$slope * z[s$test]
    val <- try(metric_fn(y[s$test], pred), silent = TRUE)
    if (inherits(val, "try-error") || length(val) != 1L || !is.numeric(val)) {
      return(NA_real_)
    }
    as.numeric(val)
  }, numeric(1))

  if (all(is.na(vals))) NA_real_ else mean(vals, na.rm = TRUE)
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

#' @keywords internal
#' @noRd
.mm_warn_grid_edge <- function(best, max_lags, decays) {
  finite_lags <- max_lags[is.finite(max_lags)]
  msgs <- character(0)
  if (length(decays) > 1L && isTRUE(best$decay == max(decays))) {
    msgs <- c(msgs, "decay" = sprintf(
      "The selected decay (%s) is the largest value in `decays`.",
      format(best$decay)))
  }
  if (length(finite_lags) > 1L && is.finite(best$max_lag) &&
      isTRUE(best$max_lag == max(finite_lags))) {
    msgs <- c(msgs, "max_lag" = sprintf(
      "The selected max_lag (%s) is the largest value in `max_lags`.",
      format(best$max_lag)))
  }
  if (length(msgs) == 0L) return(invisible(NULL))
  cli::cli_warn(c(
    "The chosen parameters sit on the edge of the grid.",
    stats::setNames(unname(msgs), rep("*", length(msgs))),
    i = "Widen the grid and re-run. A parameter pinned to a boundary usually \\
         means the optimum is outside it, or that the series is too short to \\
         identify carryover this long."
  ))
  invisible(NULL)
}

#' @keywords internal
#' @noRd
.mm_maybe_parallel <- function(idx, fun, cores, call = parent.frame()) {
  cores <- .mm_check_count(cores, "cores", min = 1L, call = call)
  if (cores == 1L) return(lapply(idx, fun))
  avail <- parallel::detectCores(logical = FALSE)
  if (is.finite(avail)) cores <- min(cores, avail)
  if (cores <= 1L) return(lapply(idx, fun))
  if (.Platform$OS.type == "unix") {
    return(parallel::mclapply(idx, fun, mc.cores = cores))
  }
  cl <- parallel::makePSOCKcluster(cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterEvalQ(cl, requireNamespace("mediamix", quietly = TRUE))
  parallel::parLapply(cl, idx, fun)
}
