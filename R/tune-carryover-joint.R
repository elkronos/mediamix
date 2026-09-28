#' Tune every channel's carryover jointly, inside one model
#'
#' The multi-channel counterpart of [tune_carryover()]. Each channel's
#' `(max_lag, decay)` is chosen by forward-only cross-validation of a model
#' containing *all* the channels and any control variables, so a channel's
#' carryover is judged with the others' effects accounted for rather than
#' absorbed.
#'
#' @param media A data frame (or matrix with column names) of raw media, one
#'   column per channel, rows in time order.
#' @param y Numeric vector of the KPI, one value per row of `media`.
#' @param controls Optional data frame of control variables -- trend, price,
#'   seasonality, holidays -- entered into the model untransformed. Character
#'   and factor columns are expanded to dummies.
#' @param fit_fn,predict_fn Optional model. `fit_fn(X, y)` receives a data frame
#'   of the adstocked media followed by the controls; `predict_fn(model, X)`
#'   returns predictions for new rows of the same shape. Both `NULL` (the
#'   default) fits ordinary least squares with an intercept.
#' @param max_lags,decays Candidate kernel lengths and decays, shared by every
#'   channel. See [tune_carryover()].
#' @param start Optional named numeric vector of starting decays, one per
#'   channel. Defaults to the middle of `decays`.
#' @param max_iter Maximum number of full sweeps over the channels.
#' @param warm_start Seed each adstocked series with
#'   [adstock_steady_state()] instead of starting from zero, removing the
#'   start-up bias of long-carryover channels.
#' @inheritParams tune_carryover
#'
#' @return An object of class `mm_carryover_joint`: a list with
#'   \describe{
#'     \item{`best`}{One row per channel: `channel`, `max_lag`, `decay`,
#'       `half_life`.}
#'     \item{`best_1se`}{The same, with each channel's decay chosen by the
#'       paired one-standard-error rule on its final profile.}
#'     \item{`profiles`}{Each channel's cross-validation profile from the
#'       final sweep, with the other channels held at their selected values:
#'       `channel`, `max_lag`, `decay`, `half_life`, `metric`, `std_err`.}
#'     \item{`metric`}{The cross-validated metric at the selected values.}
#'     \item{`iterations`, `converged`}{Sweeps run, and whether the last
#'       sweep changed nothing.}
#'   }
#'   plus `metric_name`, `scheme`, `aggregate`, `n_splits` and `normalise`.
#'
#' @details
#' The search is coordinate descent: each sweep visits the channels in turn
#' and sets each one to its best grid point with the others held fixed,
#' repeating until a sweep changes nothing. Each step can only lower the
#' cross-validated error, so the search terminates, but like any coordinate
#' method it finds a local optimum of the grid, not necessarily the global
#' one. A different `start` is a cheap check. The cost is roughly
#' `channels x grid size x sweeps` cross-validated fits, far fewer than the
#' full grid's `grid size ^ channels`.
#'
#' Saturation is not tuned here. For carryover, saturation and a model
#' penalty tuned jointly, use [step_adstock()] and [step_saturation()] with
#' \pkg{tune}; see `vignette("tidymodels")`.
#'
#' @seealso [tune_carryover()] for one channel, [adstock_steady_state()]
#'
#' @examples
#' data(mm_weekly)
#' north <- mm_weekly[mm_weekly$geo == "north", ]
#' channels <- c("tv", "video", "search", "social", "display")
#' controls <- data.frame(week = seq_len(nrow(north)), price = north$price,
#'                        seasonality = north$seasonality,
#'                        holiday = north$holiday)
#'
#' joint <- tune_carryover_joint(north[channels], north$revenue,
#'                               controls = controls,
#'                               decays = seq(0.05, 0.95, by = 0.1),
#'                               skip = 3)
#' joint
#' rbind(truth = attr(mm_weekly, "truth")$decay[channels],
#'       joint = joint$best$decay)
#' @export
tune_carryover_joint <- function(media, y,
                                 controls = NULL,
                                 fit_fn = NULL,
                                 predict_fn = NULL,
                                 max_lags = Inf,
                                 decays = seq(0.05, 0.95, by = 0.05),
                                 normalise = TRUE,
                                 scheme = c("rolling_origin", "k_fold_forward"),
                                 metric_fn = rmse,
                                 initial = NULL,
                                 assess = 1L,
                                 skip = 0L,
                                 k = 5L,
                                 aggregate = c("pooled", "mean"),
                                 start = NULL,
                                 max_iter = 10L,
                                 warm_start = FALSE) {
  scheme <- match.arg(scheme)
  aggregate <- match.arg(aggregate)
  metric_name <- .mm_fn_label(substitute(metric_fn))
  if (is.matrix(media)) media <- as.data.frame(media)
  if (!is.data.frame(media) || ncol(media) == 0L || is.null(names(media))) {
    cli::cli_abort("{.arg media} must be a data frame with one named column \\
                    per channel.")
  }
  channels <- names(media)
  if (anyDuplicated(channels)) cli::cli_abort("{.arg media} has duplicated column names.")
  media <- lapply(media, function(z) .mm_check_numeric(z, "media",
                                                      allow_na = FALSE))
  y <- .mm_check_numeric(y, "y")
  n <- length(y)
  if (any(lengths(media) != n)) {
    cli::cli_abort("Every column of {.arg media} must have {n} rows, like {.arg y}.")
  }
  ctrl <- .mm_control_matrix(controls, n)
  model <- .mm_resolve_model(fit_fn, predict_fn)
  if (!is.function(metric_fn)) cli::cli_abort("{.arg metric_fn} must be a function.")
  .mm_check_flag(normalise, "normalise")
  .mm_check_flag(warm_start, "warm_start")
  max_iter <- .mm_check_count(max_iter, "max_iter", min = 1L)
  max_lags <- .mm_check_lag_grid(max_lags)
  decays <- .mm_check_decay_grid(decays)

  splits <- .mm_make_splits(n, scheme = scheme, initial = initial,
                            assess = assess, skip = skip, k = k,
                            call = environment())
  if (length(splits) == 0L) cli::cli_abort("No usable resampling splits.")

  grid <- expand.grid(max_lag = max_lags, decay = decays,
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  grid <- grid[!(normalise & is.infinite(grid$max_lag) & grid$decay == 1), ,
               drop = FALSE]
  if (nrow(grid) == 0L) cli::cli_abort("The parameter grid is empty.")
  rownames(grid) <- NULL

  # Every channel's adstocked series at every grid point, computed once.
  cache <- lapply(media, function(x) {
    lapply(seq_len(nrow(grid)), function(g) {
      st <- if (warm_start) {
        adstock_steady_state(x, grid$decay[g], max_lag = grid$max_lag[g])
      } else 0
      adstock_geometric(x, decay = grid$decay[g], max_lag = grid$max_lag[g],
                        normalise = normalise, state = st)
    })
  })

  # Starting point: the middle of the grid, or the user's decays.
  mid <- which.min(abs(grid$decay - stats::median(decays)))
  cur <- stats::setNames(rep(mid, length(channels)), channels)
  if (!is.null(start)) {
    if (!is.numeric(start) || is.null(names(start)) ||
        !all(names(start) %in% channels)) {
      cli::cli_abort("{.arg start} must be a numeric vector named by channel.")
    }
    for (ch in names(start)) cur[[ch]] <- which.min(abs(grid$decay - start[[ch]]))
  }

  score <- function(sel) {
    X <- as.data.frame(lapply(stats::setNames(channels, channels),
                              function(ch) cache[[ch]][[sel[[ch]]]]))
    if (!is.null(ctrl)) X <- cbind(X, ctrl)
    folds <- lapply(splits, function(s) {
      fit <- try(model$fit(X[s$train, , drop = FALSE], y[s$train]),
                 silent = TRUE)
      if (inherits(fit, "try-error")) return(NULL)
      pred <- try(model$predict(fit, X[s$test, , drop = FALSE]), silent = TRUE)
      if (inherits(pred, "try-error") || length(pred) != length(s$test)) {
        return(NULL)
      }
      list(actual = y[s$test], pred = as.numeric(pred))
    })
    .mm_cv_score(folds, metric_fn, aggregate)
  }

  profiles <- list()
  converged <- FALSE
  iter <- 0L
  current <- score(cur)
  while (iter < max_iter) {
    iter <- iter + 1L
    changed <- FALSE
    for (ch in channels) {
      trial <- lapply(seq_len(nrow(grid)), function(g) {
        sel <- cur
        sel[[ch]] <- g
        score(sel)
      })
      m <- vapply(trial, `[[`, numeric(1), "metric")
      if (all(is.na(m))) {
        cli::cli_abort("Every grid point produced a missing metric for \\
                        channel {.val {ch}}.")
      }
      g_best <- which.min(m)
      profiles[[ch]] <- list(metric = m,
                             std_err = vapply(trial, `[[`, numeric(1), "std_err"),
                             vals = vapply(trial, `[[`, numeric(length(splits)),
                                           "vals"))
      if (g_best != cur[[ch]] && m[g_best] < current$metric - 1e-12) {
        cur[[ch]] <- g_best
        current <- trial[[g_best]]
        changed <- TRUE
      }
    }
    if (!changed) {
      converged <- TRUE
      break
    }
  }
  if (!converged) {
    cli::cli_warn(c(
      "Coordinate descent did not settle within {max_iter} sweep{?s}.",
      i = "Raise {.arg max_iter}, or coarsen the grid."
    ))
  }

  hl <- function(d) ifelse(d > 0 & d < 1, log(0.5) / log(d), NA_real_)
  best <- data.frame(channel = channels, max_lag = grid$max_lag[cur],
                     decay = grid$decay[cur], half_life = hl(grid$decay[cur]),
                     stringsAsFactors = FALSE)
  prof <- do.call(rbind, lapply(channels, function(ch) {
    data.frame(channel = ch, max_lag = grid$max_lag, decay = grid$decay,
               half_life = hl(grid$decay), metric = profiles[[ch]]$metric,
               std_err = profiles[[ch]]$std_err, stringsAsFactors = FALSE)
  }))
  best_1se <- do.call(rbind, lapply(channels, function(ch) {
    r <- data.frame(max_lag = grid$max_lag, decay = grid$decay,
                    half_life = hl(grid$decay),
                    std_err = profiles[[ch]]$std_err)
    v <- profiles[[ch]]$vals
    if (!is.matrix(v)) v <- matrix(v, nrow = length(splits))
    pick <- .mm_one_se(r, v)
    data.frame(channel = ch, pick[, c("max_lag", "decay", "half_life")],
               stringsAsFactors = FALSE)
  }))
  rownames(best_1se) <- NULL

  structure(
    list(best = best, best_1se = best_1se, profiles = prof,
         metric = current$metric, metric_name = metric_name,
         iterations = iter, converged = converged, scheme = scheme,
         aggregate = aggregate, n_splits = length(splits),
         normalise = normalise),
    class = "mm_carryover_joint"
  )
}

#' @export
print.mm_carryover_joint <- function(x, ...) {
  cli::cli_h3("Joint carryover tuning")
  cli::cli_text("{nrow(x$best)} channel{?s}, {x$scheme} resampling, \\
                 {x$n_splits} split{?s}; {x$iterations} sweep{?s}, \\
                 {if (x$converged) 'converged' else 'not converged'}")
  cli::cli_text("{x$metric_name} ({x$aggregate}) = {signif(x$metric, 6)}")
  tab <- x$best
  tab$decay_1se <- x$best_1se$decay
  print(tab, row.names = FALSE, digits = 3)
  invisible(x)
}

# ---- shared model plumbing -----------------------------------------------------

# Controls as a numeric data frame, factors and characters expanded.
#' @keywords internal
#' @noRd
.mm_control_matrix <- function(controls, n, call = parent.frame()) {
  if (is.null(controls)) return(NULL)
  if (is.matrix(controls)) controls <- as.data.frame(controls)
  if (is.numeric(controls) && is.null(dim(controls))) {
    controls <- data.frame(control = controls)
  }
  if (!is.data.frame(controls)) {
    cli::cli_abort("{.arg controls} must be a data frame.", call = call)
  }
  if (nrow(controls) != n) {
    cli::cli_abort("{.arg controls} must have {n} rows, not {nrow(controls)}.",
                   call = call)
  }
  mm <- stats::model.matrix(~ ., data = controls,
                            na.action = stats::na.pass)[, -1L, drop = FALSE]
  out <- as.data.frame(mm)
  names(out) <- make.names(colnames(mm), unique = TRUE)
  out
}

# Default multivariate least squares, or the user's model.
#' @keywords internal
#' @noRd
.mm_resolve_model <- function(fit_fn, predict_fn, call = parent.frame()) {
  if (is.null(fit_fn) && is.null(predict_fn)) {
    return(list(fit = .mm_ols_multi, predict = .mm_ols_multi_predict,
                default = TRUE))
  }
  if (!is.function(fit_fn) || !is.function(predict_fn)) {
    cli::cli_abort("Supply both {.arg fit_fn} and {.arg predict_fn}, or \\
                    neither.", call = call)
  }
  list(fit = fit_fn, predict = predict_fn, default = FALSE)
}

#' @keywords internal
#' @noRd
.mm_ols_multi <- function(X, y) {
  X <- cbind(`(Intercept)` = 1, as.matrix(X))
  ok <- stats::complete.cases(X) & is.finite(y)
  fit <- stats::lm.fit(X[ok, , drop = FALSE], y[ok])
  cf <- fit$coefficients
  cf[!is.finite(cf)] <- 0
  cf
}

#' @keywords internal
#' @noRd
.mm_ols_multi_predict <- function(model, X) {
  as.numeric(cbind(1, as.matrix(X)) %*% model)
}
