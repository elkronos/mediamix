#' Tune every channel's carryover jointly, inside one model
#'
#' The multi-channel counterpart of [tune_carryover()]. Each channel's
#' `(max_lag, decay)` is chosen by forward-only cross-validation of a model
#' containing *all* the channels and any control variables, so a channel's
#' carryover is judged with the others' effects accounted for rather than
#' absorbed.
#'
#' @param media A data frame (or matrix with column names) of raw media, one
#'   column per channel, rows in time order. Every column must be finite and
#'   must vary.
#' @param y Numeric vector of the KPI, one value per row of `media`.
#' @param controls Optional data frame of control variables, such as trend,
#'   price, seasonality and holidays, entered into the model untransformed.
#'   Character,
#'   factor and logical columns are expanded to treatment dummies by
#'   [stats::model.matrix()] and every name is made syntactic. A column with a
#'   single value is dropped with a message, and missing or non-finite values
#'   are an error that names the column and rows. No control may share a name
#'   with a channel.
#' @param fit_fn,predict_fn Optional model. `fit_fn(X, y)` receives a data frame
#'   of the adstocked media followed by the controls; `predict_fn(model, X)`
#'   returns predictions for new rows of the same shape. Both `NULL` (the
#'   default) fits ordinary least squares with an intercept.
#' @param max_lags,decays Candidate kernel lengths and decays, shared by every
#'   channel. See [tune_carryover()].
#' @param start Optional named numeric vector of starting decays in `[0, 1]`,
#'   one per channel. Defaults to the middle of `decays`.
#' @param max_iter Maximum number of full sweeps over the channels.
#' @param warm_start Seed each adstocked series with
#'   [adstock_steady_state()] instead of starting from zero, removing the
#'   start-up bias of long-carryover channels. As in [tune_carryover()], the
#'   seed uses no media from beyond the first training window.
#' @param cores Number of cores over which each channel's grid points are
#'   scored. See [tune_carryover()].
#' @inheritParams tune_carryover
#'
#' @return An object of class `mm_carryover_joint`: a list with
#'   \describe{
#'     \item{`best`}{One row per channel: `channel`, `max_lag`, `decay`,
#'       `half_life`.}
#'     \item{`best_1se`}{The same, with each channel's value chosen by the
#'       paired one-standard-error rule on its own final profile, anchored on
#'       its value in `best`. See *The one-standard-error set*.}
#'     \item{`best_1se_metric`}{The cross-validated metric of the `best_1se`
#'       values taken together.}
#'     \item{`profiles`}{Each channel's cross-validation profile from the
#'       final sweep, with the other channels held at their selected values:
#'       `channel`, `max_lag`, `decay`, `half_life`, `metric`, `std_err`.}
#'     \item{`metric`}{The cross-validated metric at the selected values.}
#'     \item{`metric_name`}{Name of the metric function used.}
#'     \item{`iterations`, `converged`}{Sweeps run, and whether the last
#'       sweep changed nothing.}
#'   }
#'   plus `scheme`, `aggregate`, `n_splits`, `n_ok`, `n_series` and
#'   `normalise`, as in [tune_carryover()]. `as.data.frame()` and `tidy()`
#'   return `profiles` with logical columns `best` and `best_1se` marking each
#'   channel's selections.
#'
#' @details
#' The search is coordinate descent: each sweep visits the channels in turn
#' and sets each one to its best grid point with the others held fixed,
#' repeating until a sweep changes nothing. Each step can only lower the
#' cross-validated error, so the search terminates, but like any coordinate
#' method it finds a local optimum of the grid, not necessarily the global
#' one. A different `start` is a cheap check. The cost is roughly
#' `channels x grid size x sweeps` cross-validated fits, far fewer than the
#' full grid's `grid size ^ channels`; `cores` spreads each channel's grid
#' points over several processes.
#'
#' A grid point whose fit fails on some splits is handled as in
#' [tune_carryover()]: within each step every grid point is scored on the
#' splits all of them completed. A warning reports the most splits any step
#' left out, and a selection on the edge of the grid is reported for each
#' channel.
#'
#' Saturation is not tuned here. For carryover, saturation and a model
#' penalty tuned jointly, use [step_adstock()] and [step_saturation()] with
#' \pkg{tune}; see `vignette("tidymodels")`.
#'
#' @section The one-standard-error set:
#' Each channel's one-standard-error choice is made on its own profile, with
#' the other channels held at `best`, so the rule is applied channel by
#' channel. Shortening several channels at once can cost more than any one of
#' them does alone, and the combination was not part of the search. The set
#' is therefore scored as a whole after the search, and `best_1se_metric`
#' reports that score next to `metric`. When the gap between the two is large
#' against the profiles' `std_err`, the channels' near-ties do not add up to a
#' near-tie for the set, and `best_1se` is better read one channel at a time.
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
                                 warm_start = FALSE,
                                 cores = 1L,
                                 by = NULL,
                                 index = NULL,
                                 se_block = 1L) {
  scheme <- match.arg(scheme)
  aggregate <- match.arg(aggregate)
  metric_name <- .mm_fn_label(substitute(metric_fn))
  media <- .mm_check_media(media)
  channels <- names(media)
  y <- .mm_check_numeric(y, "y")
  n <- length(y)
  if (any(lengths(media) != n)) {
    cli::cli_abort("Every column of {.arg media} must have {n} rows, like {.arg y}.")
  }
  layout <- .mm_tune_layout(n, by, index)
  ctrl <- .mm_control_matrix(controls, n, reserved = make.names(channels))
  model <- .mm_resolve_model(fit_fn, predict_fn)
  if (!is.function(metric_fn)) cli::cli_abort("{.arg metric_fn} must be a function.")
  .mm_check_flag(normalise, "normalise")
  .mm_check_flag(warm_start, "warm_start")
  max_iter <- .mm_check_count(max_iter, "max_iter", min = 1L)
  max_lags <- .mm_check_lag_grid(max_lags)
  decays <- .mm_check_decay_grid(decays)
  se_block <- .mm_check_count(se_block, "se_block", min = 1L)
  cores <- .mm_check_count(cores, "cores", min = 1L)

  periods <- .mm_make_splits(layout$n_periods, scheme = scheme,
                             initial = initial, assess = assess, skip = skip,
                             k = k, call = environment())
  if (length(periods) == 0L) cli::cli_abort("No usable resampling splits.")
  .mm_check_se_block(se_block, length(periods))
  splits <- .mm_split_rows(periods, layout)
  warm_periods <- length(periods[[1L]]$train)
  grid <- .mm_tuning_grid(max_lags, decays, normalise, warm_start)
  n_grid <- nrow(grid)

  y_cv <- .mm_cv_rows(y, layout)
  ctrl_cv <- if (is.null(ctrl)) NULL else .mm_cv_rows(ctrl, layout)
  ctrl_mat <- if (is.null(ctrl)) NULL else as.matrix(ctrl_cv)

  # Every channel's adstocked series at every grid point, computed once.
  cache <- lapply(media, function(x) {
    lapply(seq_len(n_grid), function(g) {
      .mm_adstock_cv(x, grid$decay[g], grid$max_lag[g], normalise,
                     warm_start, warm_periods, layout)
    })
  })

  # Starting point: the middle of the grid, or the user's decays.
  mid <- which.min(abs(grid$decay - stats::median(decays)))
  cur <- stats::setNames(rep(mid, length(channels)), channels)
  start <- .mm_check_start(start, channels)
  for (ch in names(start)) cur[[ch]] <- which.min(abs(grid$decay - start[[ch]]))

  run <- function(sel) {
    cols <- lapply(stats::setNames(channels, channels),
                   function(ch) cache[[ch]][[sel[[ch]]]])
    if (model$default) {
      X <- do.call(cbind, cols)
      if (!is.null(ctrl_mat)) X <- cbind(X, ctrl_mat)
      return(.mm_cv_ols(X, y_cv, splits))
    }
    X <- as.data.frame(cols)
    if (!is.null(ctrl_cv)) X <- cbind(X, ctrl_cv)
    .mm_cv_general(X, y_cv, splits, model$fit, model$predict)
  }
  compare <- function(sets, se = FALSE) {
    .mm_cv_compare(sets, y_cv, splits, metric_fn, aggregate, se_block,
                   se = se)
  }

  profiles <- list()
  converged <- FALSE
  iter <- 0L
  lost <- 0L
  n_failed <- 0L
  first_error <- NULL
  first_worker <- NULL
  while (iter < max_iter) {
    iter <- iter + 1L
    changed <- FALSE
    for (ch in channels) {
      raw <- .mm_maybe_parallel(seq_len(n_grid), function(g) {
        sel <- cur
        sel[[ch]] <- g
        run(sel)
      }, cores)
      sets <- .mm_worker_failures(raw, length(splits))
      if (sets$n_failed > n_failed) {
        n_failed <- sets$n_failed
        first_worker <- sets$first
      }
      sc <- compare(sets$folds)
      .mm_check_scores(sc, channel = ch)
      if (sc$lost > lost) lost <- sc$lost
      if (is.null(first_error)) first_error <- sc$first_error
      profiles[[ch]] <- sets$folds
      m <- sc$metric
      g_best <- which.min(m)
      m_cur <- m[cur[[ch]]]
      better <- !is.finite(m_cur) ||
        m[g_best] < m_cur - 1e-10 * max(1, abs(m_cur))
      if (g_best != cur[[ch]] && better) {
        cur[[ch]] <- g_best
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
  .mm_warn_worker_failures(list(n_failed = n_failed, first = first_worker))

  # Final profiles: standard errors, and each channel's one-SE choice anchored
  # on the value the search selected for it.
  one_se <- cur
  prof <- vector("list", length(channels))
  names(prof) <- channels
  for (ch in channels) {
    sc <- compare(profiles[[ch]], se = TRUE)
    r <- data.frame(max_lag = grid$max_lag, decay = grid$decay,
                    half_life = .mm_grid_half_life(grid$decay, grid$max_lag),
                    metric = sc$metric, std_err = sc$std_err)
    one_se[[ch]] <- .mm_one_se_pick(r, sc$jack, anchor = cur[[ch]])
    prof[[ch]] <- data.frame(channel = ch, r, stringsAsFactors = FALSE)
    metric <- sc$metric[cur[[ch]]]
    n_ok <- sc$n_ok
  }
  prof <- do.call(rbind, prof)
  rownames(prof) <- NULL
  if (lost > 0L) {
    cli::cli_warn(c(
      "Up to {lost} split{?s} {?was/were} left out of a step of the search \\
       because a fit failed on {?it/them}.",
      i = "Within each step, every grid point is scored on the splits all of \\
           them completed; the selected values are scored on {n_ok}.",
      i = "First failure: {first_error}"
    ))
  }

  # The per-channel choices taken together were never part of the search, so
  # score them as a set.
  best_1se_metric <- metric
  if (!identical(one_se, cur)) {
    both <- compare(list(run(cur), run(one_se)))
    best_1se_metric <- both$metric[2L]
  }

  pick <- function(sel) {
    sel <- unname(sel)
    data.frame(channel = channels, max_lag = grid$max_lag[sel],
               decay = grid$decay[sel],
               half_life = .mm_grid_half_life(grid$decay[sel],
                                              grid$max_lag[sel]),
               stringsAsFactors = FALSE)
  }
  best <- pick(cur)

  notes <- lapply(channels, function(ch) {
    .mm_edge_notes(grid$max_lag[cur[[ch]]], grid$decay[cur[[ch]]], grid)
  })
  names(notes) <- channels
  .mm_warn_grid_edge(notes)

  structure(
    list(best = best, best_1se = pick(one_se),
         best_1se_metric = best_1se_metric, profiles = prof,
         metric = metric, metric_name = metric_name,
         iterations = iter, converged = converged, scheme = scheme,
         aggregate = aggregate, n_splits = length(splits), n_ok = n_ok,
         n_series = layout$n_series, normalise = normalise),
    class = "mm_carryover_joint"
  )
}

#' @export
print.mm_carryover_joint <- function(x, ...) {
  cli::cli_h3("Joint carryover tuning")
  series <- if (isTRUE(x$n_series > 1L)) {
    sprintf(" over %d series", x$n_series)
  } else ""
  scored <- if (isTRUE(x$n_ok < x$n_splits)) {
    sprintf(" (%d scored)", x$n_ok)
  } else ""
  cli::cli_text("{nrow(x$best)} channel{?s}, {x$scheme} resampling{series}, \\
                 {x$n_splits} split{?s}{scored}; {x$iterations} sweep{?s}, \\
                 {if (x$converged) 'converged' else 'not converged'}")
  cli::cli_text("{x$metric_name} ({x$aggregate}) = {signif(x$metric, 6)}")
  same <- identical(x$best_1se[c("max_lag", "decay")],
                    x$best[c("max_lag", "decay")])
  if (!is.null(x$best_1se_metric) && !same) {
    cli::cli_text("One-SE set, chosen channel by channel: \\
                   {x$metric_name} = {signif(x$best_1se_metric, 6)}")
  }
  tab <- x$best
  tab$decay_1se <- x$best_1se$decay
  print(tab, row.names = FALSE, digits = 3)
  invisible(x)
}

#' @export
as.data.frame.mm_carryover_joint <- function(x, row.names = NULL,
                                             optional = FALSE, ...) {
  out <- x$profiles
  out$best <- .mm_same_choice(out, x$best)
  out$best_1se <- .mm_same_choice(out, x$best_1se)
  if (!is.null(row.names)) rownames(out) <- row.names
  out
}

#' @exportS3Method generics::tidy
tidy.mm_carryover_joint <- function(x, ...) {
  as.data.frame(x)
}

# ---- shared model plumbing -----------------------------------------------------

# Media columns as a named list of finite, varying numeric vectors.
#' @keywords internal
#' @noRd
.mm_check_media <- function(media, call = parent.frame()) {
  if (is.matrix(media)) {
    if (is.null(colnames(media))) {
      cli::cli_abort("{.arg media} must have column names, one per channel.",
                     call = call)
    }
    media <- as.data.frame(media)
  }
  if (!is.data.frame(media) || ncol(media) == 0L) {
    cli::cli_abort("{.arg media} must be a data frame with one named column \\
                    per channel.", call = call)
  }
  channels <- names(media)
  if (is.null(channels) || anyNA(channels) || any(!nzchar(channels))) {
    cli::cli_abort("Every column of {.arg media} must be named.", call = call)
  }
  if (anyDuplicated(channels)) {
    dup <- unique(channels[duplicated(channels)])
    cli::cli_abort("{.arg media} has duplicated column names: {.val {dup}}.",
                   call = call)
  }
  cols <- lapply(channels, function(ch) {
    v <- media[[ch]]
    if (!is.numeric(v) || !is.null(dim(v))) {
      cli::cli_abort("Column {.val {ch}} of {.arg media} must be numeric, not \\
                      {.obj_type_friendly {v}}.", call = call)
    }
    if (anyNA(v)) {
      cli::cli_abort("Column {.val {ch}} of {.arg media} has missing values.",
                     call = call)
    }
    if (any(!is.finite(v))) {
      cli::cli_abort("Column {.val {ch}} of {.arg media} has infinite values.",
                     call = call)
    }
    as.numeric(v)
  })
  names(cols) <- channels
  const <- channels[vapply(cols, function(v) all(v == v[1L]), logical(1))]
  if (length(const) > 0L) {
    cli::cli_abort(c(
      "{cli::qty(length(const))}Column{?s} {.val {const}} of {.arg media} \\
       {?is/are} constant.",
      i = "A channel that never varies carries no information about \\
           carryover. Drop it before tuning."
    ), call = call)
  }
  cols
}

#' @keywords internal
#' @noRd
.mm_check_start <- function(start, channels, call = parent.frame()) {
  if (is.null(start)) return(numeric(0))
  if (!is.numeric(start) || is.null(names(start)) || anyNA(names(start)) ||
      anyNA(start)) {
    cli::cli_abort("{.arg start} must be a numeric vector of decays named by \\
                    channel, without missing values.", call = call)
  }
  unknown <- setdiff(names(start), channels)
  if (length(unknown) > 0L) {
    cli::cli_abort("{.arg start} names {cli::qty(length(unknown))}\\
                    {?a channel/channels} not in {.arg media}: \\
                    {.val {unknown}}.", call = call)
  }
  if (anyDuplicated(names(start))) {
    cli::cli_abort("{.arg start} names a channel more than once.", call = call)
  }
  if (any(start < 0 | start > 1)) {
    cli::cli_abort("{.arg start} must lie in {.code [0, 1]}.", call = call)
  }
  start
}

# Controls as a numeric data frame, factors, characters and logicals expanded
# to treatment dummies. Rows are never dropped or recycled: missing and
# non-finite values are refused up front, and the result is checked to have
# one row per period. `reserved` names the design columns a control may not
# take.
#' @keywords internal
#' @noRd
.mm_control_matrix <- function(controls, n, reserved = character(0),
                               call = parent.frame()) {
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
  controls <- as.data.frame(controls)
  if (ncol(controls) == 0L) return(NULL)
  listed <- names(controls)[vapply(controls, is.list, logical(1))]
  if (length(listed) > 0L) {
    cli::cli_abort("{cli::qty(length(listed))}{.arg controls} column{?s} \\
                    {.val {listed}} {?is a list column/are list columns}.",
                   call = call)
  }

  bad <- vapply(controls, function(v) {
    miss <- if (is.numeric(v)) !is.finite(v) else is.na(v)
    if (is.matrix(miss)) rowSums(miss) > 0 else miss
  }, logical(n))
  bad <- matrix(bad, nrow = n)
  if (any(bad)) {
    cols <- names(controls)[colSums(bad) > 0]
    rows <- which(rowSums(bad) > 0)
    shown <- utils::head(rows, 8L)
    more <- if (length(rows) > 8L) sprintf(" and %d more", length(rows) - 8L)
      else ""
    cli::cli_abort(c(
      "{.arg controls} has missing or non-finite values.",
      x = "{cli::qty(length(cols))}Column{?s} {.val {cols}}, \\
           {cli::qty(length(rows))}row{?s} {shown}{more}.",
      i = "Impute them, or drop those periods from every input before \\
           tuning."
    ), call = call)
  }

  controls[] <- lapply(controls, function(v) {
    if (is.factor(v)) droplevels(v) else v
  })
  flat <- vapply(controls, function(v) {
    nrow(unique(as.data.frame(v))) < 2L
  }, logical(1))
  if (any(flat)) {
    gone <- names(controls)[flat]
    cli::cli_inform(c(
      "Dropping {cli::qty(length(gone))}control{?s} {.val {gone}}, which \\
       {?has/have} a single value.",
      i = "A constant column adds nothing beside the model's intercept."
    ))
    controls <- controls[!flat]
    if (ncol(controls) == 0L) return(NULL)
  }

  mf <- stats::model.frame(~ ., data = controls, na.action = stats::na.pass)
  mm <- stats::model.matrix(attr(mf, "terms"), data = mf)
  if (nrow(mm) != n) {
    cli::cli_abort("{.arg controls} produced {nrow(mm)} model rows for {n} \\
                    periods.", call = call)
  }
  mm <- mm[, colnames(mm) != "(Intercept)", drop = FALSE]
  out <- as.data.frame(mm)
  names(out) <- make.names(colnames(mm), unique = TRUE)
  clash <- intersect(names(out), reserved)
  if (length(clash) > 0L) {
    cli::cli_abort(c(
      "{cli::qty(length(clash))}Control{?s} {.val {clash}} {?has/have} the \\
       name of a media column.",
      i = "Rename the control so the model can tell the two apart."
    ), call = call)
  }
  out
}

# The default least squares (fitted by .mm_cv_ols()), or the user's model.
#' @keywords internal
#' @noRd
.mm_resolve_model <- function(fit_fn, predict_fn, call = parent.frame()) {
  if (is.null(fit_fn) && is.null(predict_fn)) {
    return(list(fit = NULL, predict = NULL, default = TRUE))
  }
  if (!is.function(fit_fn) || !is.function(predict_fn)) {
    cli::cli_abort("Supply both {.arg fit_fn} and {.arg predict_fn}, or \\
                    neither.", call = call)
  }
  list(fit = fit_fn, predict = predict_fn, default = FALSE)
}
