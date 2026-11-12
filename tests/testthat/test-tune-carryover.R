# Tests for R/tune-carryover.R: tune_carryover() and its resampling schemes.
#
# The recovery test below is, per the package's design doc, the single most
# important test in the suite: it guards the corrected CV objective (put the
# KPI in the loop) and the fast/general path equivalence that makes that
# objective cheap to evaluate at scale.

# ---- recovery: the central test --------------------------------------------

test_that("tune_carryover() recovers a known decay from data simulated with it", {
  grid_step <- 0.02
  decay_grid <- seq(0.02, 0.98, by = grid_step)

  recover_decay <- function(truth) {
    set.seed(2)
    n <- 150
    spend <- pmax(0, rnorm(n, 500, 250))
    kpi <- 200 + 0.5 * adstock_geometric(spend, decay = truth) + rnorm(n, sd = 8)
    tuned <- tune_carryover(spend, kpi, max_lags = Inf, decays = decay_grid)
    tuned
  }

  for (truth in c(0.3, 0.5, 0.7)) {
    tuned <- recover_decay(truth)
    expect_s3_class(tuned, "mm_carryover")
    recovered <- tuned$best$decay
    expect_lte(abs(recovered - truth), grid_step + 1e-8,
               label = sprintf("|recovered (%s) - truth (%s)|", recovered, truth))
  }
})

test_that("tune_carryover() results are sorted with the best (lowest metric) row first", {
  set.seed(2)
  n <- 150
  spend <- pmax(0, rnorm(n, 500, 250))
  kpi <- 200 + 0.5 * adstock_geometric(spend, decay = 0.5) + rnorm(n, sd = 8)
  tuned <- tune_carryover(spend, kpi, max_lags = Inf,
                          decays = seq(0.1, 0.9, by = 0.1))

  expect_true(all(diff(tuned$results$metric) >= -1e-12))  # non-decreasing
  expect_equal(tuned$best$metric, min(tuned$results$metric), tolerance = 1e-12)
  expect_equal(tuned$best$decay, tuned$results$decay[1])
  expect_setequal(names(tuned$best),
                  c("max_lag", "decay", "half_life", "metric", "std_err",
                    "n_splits"))
})

# ---- fast OLS path vs the general path ---------------------------------------

test_that("the fast OLS path and the general path produce identical metrics", {
  set.seed(7)
  n <- 60
  spend <- pmax(0, rnorm(n, 300, 100))
  kpi <- 50 + 0.3 * adstock_geometric(spend, decay = 0.5) + rnorm(n, sd = 5)

  max_lags <- c(5, Inf)
  decays <- seq(0.1, 0.9, by = 0.2)

  # Explicit defaults: identical(fit_ols) && identical(stats::predict) is
  # TRUE, so this takes the O(1)-per-split cumulative-sum shortcut.
  fast_res <- tune_carryover(spend, kpi, fit_fn = fit_ols,
                             predict_fn = stats::predict,
                             max_lags = max_lags, decays = decays)

  # Wrappers that are NOT identical() to fit_ols / stats::predict, so this
  # is forced onto the general (try/fit/predict-per-split) path even though
  # it computes the exact same model.
  wrap_fit <- function(x, y) fit_ols(x, y)
  wrap_predict <- function(object, newdata) stats::predict(object, newdata)
  expect_false(identical(wrap_fit, fit_ols))
  expect_false(identical(wrap_predict, stats::predict))

  general_res <- tune_carryover(spend, kpi, fit_fn = wrap_fit,
                                predict_fn = wrap_predict,
                                max_lags = max_lags, decays = decays)

  # Same grid order, same metrics, same winner.
  expect_equal(fast_res$results$max_lag, general_res$results$max_lag)
  expect_equal(fast_res$results$decay, general_res$results$decay)
  expect_equal(fast_res$results$metric, general_res$results$metric,
               tolerance = 1e-8)
  expect_equal(fast_res$best$decay, general_res$best$decay)
  expect_equal(fast_res$best$max_lag, general_res$best$max_lag)
})

# ---- grid-edge warning ---------------------------------------------------------

test_that("a warning fires when the selected decay is pinned to the grid boundary", {
  set.seed(3)
  n <- 80
  spend <- pmax(0, rnorm(n, 400, 150))
  # True decay (0.9) lies far outside the candidate grid, so the best-fitting
  # candidate should be the largest one offered.
  kpi <- 100 + 0.6 * adstock_geometric(spend, decay = 0.9) + rnorm(n, sd = 5)

  expect_warning(
    tuned <- tune_carryover(spend, kpi, max_lags = Inf, decays = c(0.1, 0.2, 0.3)),
    regexp = "edge of the grid"
  )
  expect_equal(tuned$best$decay, 0.3)
})

test_that("a warning fires when the selected max_lag is pinned to the grid boundary", {
  set.seed(11)
  n <- 100
  spend <- pmax(0, rnorm(n, 400, 150))
  kpi <- 50 + 0.4 * adstock_geometric(spend, decay = 0.85, max_lag = 30) +
    rnorm(n, sd = 3)

  expect_warning(
    tuned <- tune_carryover(spend, kpi, max_lags = c(2, 3, 4), decays = 0.85),
    regexp = "edge of the grid"
  )
  expect_equal(tuned$best$max_lag, 4)
})

test_that("no grid-edge warning when the optimum is comfortably interior", {
  set.seed(2)
  n <- 150
  spend <- pmax(0, rnorm(n, 500, 250))
  kpi <- 200 + 0.5 * adstock_geometric(spend, decay = 0.5) + rnorm(n, sd = 8)
  expect_no_warning(
    tune_carryover(spend, kpi, max_lags = Inf, decays = seq(0.1, 0.9, by = 0.1))
  )
})

# ---- resampling schemes ---------------------------------------------------------

test_that("scheme = 'k_fold_forward' runs and produces a forward-only split count", {
  set.seed(4)
  n <- 90
  spend <- pmax(0, rnorm(n, 300, 100))
  kpi <- 30 + 0.4 * adstock_geometric(spend, decay = 0.4) + rnorm(n, sd = 4)

  tuned <- suppressWarnings(tune_carryover(
    spend, kpi, max_lags = Inf, decays = seq(0.1, 0.9, by = 0.2),
    scheme = "k_fold_forward", k = 4
  ))
  expect_s3_class(tuned, "mm_carryover")
  expect_identical(tuned$scheme, "k_fold_forward")
  expect_lte(tuned$n_splits, 4)
  expect_gt(tuned$n_splits, 0)
  # print.mm_carryover() reports through cli, which signals its output as
  # `message` conditions (stderr), not plain stdout -- capture accordingly
  # rather than with expect_output(), which only sees stdout.
  printed <- paste(capture.output(print(tuned), type = "message"),
                   collapse = "\n")
  expect_match(printed, "Carryover tuning")
})

test_that("rolling_origin is the default scheme", {
  set.seed(4)
  n <- 90
  spend <- pmax(0, rnorm(n, 300, 100))
  kpi <- 30 + 0.4 * adstock_geometric(spend, decay = 0.4) + rnorm(n, sd = 4)
  tuned <- tune_carryover(spend, kpi, max_lags = Inf, decays = c(0.3, 0.5))
  expect_identical(tuned$scheme, "rolling_origin")
})

# ---- user-supplied fit_fn / predict_fn -----------------------------------------

test_that("a user-supplied fit_fn/predict_fn wrapping stats::lm works end to end", {
  set.seed(11)
  n <- 100
  spend <- pmax(0, rnorm(n, 400, 150))
  kpi <- 50 + 0.4 * adstock_geometric(spend, decay = 0.85, max_lag = 30) +
    rnorm(n, sd = 3)

  my_fit <- function(x, y) stats::lm(y ~ x)
  my_predict <- function(object, newdata) {
    stats::predict(object, newdata = data.frame(x = newdata))
  }

  tuned <- suppressWarnings(tune_carryover(
    spend, kpi, fit_fn = my_fit, predict_fn = my_predict,
    max_lags = Inf, decays = seq(0.1, 0.9, by = 0.2)
  ))
  expect_s3_class(tuned, "mm_carryover")
  expect_true(is.numeric(tuned$best$metric))
  expect_false(is.na(tuned$best$metric))
})

# ---- input validation -------------------------------------------------------------

test_that("tune_carryover() validates its core arguments", {
  # A fixed y (rather than a random draw) is all these checks need: each one
  # is rejected by shape/type validation before y's values are ever used.
  y <- as.numeric(1:10)
  expect_error(tune_carryover(1:5, 1:4), class = "rlang_error",
               regexp = "same length")
  expect_error(tune_carryover(1:10, y, decays = 1.5),
               class = "rlang_error", regexp = "\\[0, 1\\]")
  expect_error(tune_carryover(1:10, y, fit_fn = "not a function"),
               class = "rlang_error")
  expect_error(tune_carryover(1:10, y, max_lags = c(0, 2)),
               class = "rlang_error")
})

test_that("x must be finite and must vary", {
  y <- rnorm(30)
  expect_error(tune_carryover(c(Inf, rnorm(29)), y, decays = 0.5),
               class = "rlang_error", regexp = "finite")
  expect_error(tune_carryover(rep(5, 30), y, decays = 0.5),
               class = "rlang_error", regexp = "constant")
})

# ---- result fields -----------------------------------------------------------------

test_that("metric holds the value and metric_name the name", {
  set.seed(2)
  spend <- pmax(0, rnorm(100, 500, 250))
  kpi <- 200 + 0.5 * adstock_geometric(spend, decay = 0.5) + rnorm(100, sd = 8)
  tuned <- tune_carryover(spend, kpi, decays = seq(0.1, 0.9, by = 0.1))
  expect_identical(tuned$metric_name, "rmse")
  expect_equal(tuned$metric, tuned$best$metric)
})

test_that("a one-period kernel reports no half-life", {
  set.seed(5)
  x <- pmax(0, rnorm(80, 500, 200))
  y <- 10 + 0.4 * x + rnorm(80)
  tuned <- tune_carryover(x, y, max_lags = c(1, Inf), decays = c(0.2, 0.5))
  expect_equal(tuned$best$max_lag, 1)
  expect_true(all(is.na(tuned$results$half_life[tuned$results$max_lag == 1])))
  printed <- paste(capture.output(print(tuned), type = "message"),
                   collapse = " ")
  expect_match(printed, "no carryover")
  expect_no_match(printed, "half-life")
})

# ---- the one-standard-error rule ---------------------------------------------------

test_that("the one-SE rule is anchored on the reported best", {
  # With this seed the old rule, anchored on the best mean per-split error,
  # chose more carryover than `best`.
  set.seed(13)
  n <- 104
  sp <- pmax(0, rnorm(n, 500, 250))
  kpi <- 200 + 0.5 * adstock_geometric(sp, decay = 0.5) + rnorm(n, sd = 40)
  tuned <- suppressWarnings(tune_carryover(sp, kpi,
                                           decays = seq(0.05, 0.95, 0.05)))
  expect_lte(tuned$best_1se$decay, tuned$best$decay)

  # The choice passes the paired test against best on the pooled RMSE, with
  # the standard error of the difference from leave-one-split-out values.
  errs <- function(d) {
    z <- adstock_geometric(sp, d)
    vapply(52:103, function(k) {
      m <- fit_ols(z[1:k], kpi[1:k])
      kpi[k + 1] - predict(m, z[k + 1])
    }, numeric(1))
  }
  loo <- function(e) {
    vapply(seq_along(e), function(i) sqrt(mean(e[-i]^2)), numeric(1))
  }
  eb <- errs(tuned$best$decay)
  e1 <- errs(tuned$best_1se$decay)
  d <- loo(e1) - loo(eb)
  se <- sqrt((length(d) - 1) / length(d) * sum((d - mean(d))^2))
  expect_lte(sqrt(mean(e1^2)) - sqrt(mean(eb^2)), se)
})

test_that("se_block gives a block jackknife and leaves best alone", {
  set.seed(42)
  n <- 120
  sp <- pmax(0, rnorm(n, 500, 250))
  kpi <- 200 + 0.5 * adstock_geometric(sp, decay = 0.7) + rnorm(n, sd = 8)
  one <- tune_carryover(sp, kpi, decays = 0.7)
  four <- tune_carryover(sp, kpi, decays = 0.7, se_block = 4)
  expect_equal(four$best$metric, one$best$metric)

  z <- adstock_geometric(sp, 0.7)
  e <- vapply(60:119, function(k) {
    m <- fit_ols(z[1:k], kpi[1:k])
    kpi[k + 1] - predict(m, z[k + 1])
  }, numeric(1))
  blk <- rep(1:15, each = 4)
  loo <- vapply(1:15, function(b) sqrt(mean(e[blk != b]^2)), numeric(1))
  expect_equal(four$best$std_err, sqrt(14 / 15 * sum((loo - mean(loo))^2)))
  expect_error(tune_carryover(sp, kpi, decays = 0.7, se_block = 40),
               class = "rlang_error", regexp = "se_block")
})

# ---- grid edges ----------------------------------------------------------------------

test_that("a decay on the lower edge of the grid warns, but decay 0 does not", {
  set.seed(3)
  n <- 100
  spend <- pmax(0, rnorm(n, 400, 150))
  kpi <- 100 + 0.6 * adstock_geometric(spend, decay = 0.1) + rnorm(n, sd = 5)
  expect_warning(
    tuned <- tune_carryover(spend, kpi, decays = c(0.5, 0.6, 0.7)),
    "smallest value searched"
  )
  expect_equal(tuned$best$decay, 0.5)
  expect_no_warning(
    zero <- tune_carryover(spend, kpi, decays = c(0, 0.5, 0.6))
  )
  expect_equal(zero$best$decay, 0)
})

test_that("the edge is judged on the grid that was searched", {
  set.seed(3)
  n <- 80
  spend <- pmax(0, rnorm(n, 400, 150))
  kpi <- 100 + 0.6 * adstock_geometric(spend, decay = 0.9) + rnorm(n, sd = 5)
  # decay = 1 cannot be normalised on an infinite kernel, so 0.3 is the top of
  # what was searched.
  expect_warning(
    expect_message(
      tuned <- tune_carryover(spend, kpi, decays = c(0.1, 0.2, 0.3, 1)),
      "Dropping"
    ),
    "largest value searched"
  )
  expect_equal(tuned$best$decay, 0.3)

  # An infinite kernel that was searched and lost leaves 4 inside the grid.
  set.seed(11)
  n <- 100
  spend <- pmax(0, rnorm(n, 400, 150))
  kpi <- 50 + 0.4 * adstock_geometric(spend, decay = 0.85, max_lag = 4) +
    rnorm(n, sd = 3)
  expect_no_warning(
    lag4 <- tune_carryover(spend, kpi, max_lags = c(2, 4, Inf), decays = 0.85)
  )
  expect_equal(lag4$best$max_lag, 4)
})

test_that("the bounds of the parameters themselves do not warn", {
  set.seed(5)
  x <- pmax(0, rnorm(80, 500, 200))
  # No carryover at all: a one-period kernel wins.
  y <- 10 + 0.4 * x + rnorm(80)
  expect_no_warning(
    one <- tune_carryover(x, y, max_lags = c(1, 2, 3), decays = c(0.2, 0.5, 0.8))
  )
  expect_equal(one$best$max_lag, 1)
  # An undecaying three-period window: decay 1 wins on a finite kernel.
  kp <- 10 + 0.4 * adstock_geometric(x, 1, max_lag = 3, normalise = FALSE) +
    rnorm(80)
  expect_no_warning(
    flat <- tune_carryover(x, kp, max_lags = 3, decays = c(0.8, 0.9, 1))
  )
  expect_equal(flat$best$decay, 1)
})

# ---- controls ------------------------------------------------------------------------

test_that("a missing control is an error naming the column and rows", {
  set.seed(42)
  n <- 120
  spend <- pmax(0, rnorm(n, 500, 250))
  price <- 20 + rnorm(n)
  kpi <- 200 + 0.5 * adstock_geometric(spend, 0.6) - 3 * price +
    rnorm(n, sd = 8)
  ctrl <- data.frame(price = price)
  ctrl$price[c(30, 31)] <- NA
  err <- tryCatch(
    tune_carryover(spend, kpi, decays = c(0.3, 0.6), controls = ctrl),
    error = function(e) e
  )
  expect_s3_class(err, "rlang_error")
  expect_match(conditionMessage(err), "price")
  expect_match(conditionMessage(err), "rows 30 and 31")
  # Half the series missing used to be recycled silently, because the rows
  # left over divided the series length.
  half <- data.frame(price = replace(price, 1:60, NA))
  expect_error(tune_carryover(spend, kpi, decays = c(0.3, 0.6), controls = half),
               class = "rlang_error", regexp = "missing")
  inf <- data.frame(price = replace(price, 5, Inf))
  expect_error(tune_carryover(spend, kpi, decays = 0.6, controls = inf),
               class = "rlang_error", regexp = "non-finite")
})

test_that("constant controls are dropped and dummies reach fit_fn as documented", {
  set.seed(1)
  n <- 104
  spend <- pmax(0, rnorm(n, 500, 250))
  season <- factor(rep(paste0("Q", 1:4), each = 26))
  kpi <- 100 + 0.5 * adstock_geometric(spend, 0.5) + 30 * (season == "Q2") +
    rnorm(n, sd = 5)
  seen <- NULL
  fit <- function(X, y) {
    seen <<- names(X)
    stats::lm.fit(cbind(1, as.matrix(X)), y)$coefficients
  }
  pred <- function(m, X) {
    m[!is.finite(m)] <- 0
    as.numeric(cbind(1, as.matrix(X)) %*% m)
  }
  # A single-level character column used to stop model.matrix() outright.
  ctrl <- data.frame(season = season, region = "north", flat = 3)
  expect_message(
    tune_carryover(spend, kpi, fit_fn = fit, predict_fn = pred,
                   decays = seq(0.1, 0.9, by = 0.1), controls = ctrl),
    "region"
  )
  expect_identical(seen, c("media", "seasonQ2", "seasonQ3", "seasonQ4"))
})

test_that("a control may not take the name of the media column", {
  set.seed(1)
  x <- pmax(0, rnorm(60, 500, 200))
  y <- 10 + 0.4 * adstock_geometric(x, 0.5) + rnorm(60)
  expect_error(
    tune_carryover(x, y, decays = 0.5, controls = data.frame(media = rnorm(60))),
    class = "rlang_error", regexp = "media"
  )
})

# ---- failed fits -----------------------------------------------------------------------

test_that("a fit that fails for one candidate leaves every candidate on the same splits", {
  set.seed(8)
  n <- 100
  spend <- pmax(0, rnorm(n, 500, 200))
  kpi <- 50 + 0.4 * adstock_geometric(spend, 0.4) + rnorm(n, sd = 5)
  # Only the 0.4 candidate starts at 0.6 * spend[1]; it fails on long windows.
  flaky <- function(x, y) {
    if (length(y) > 80 && abs(x[1] - 0.6 * spend[1]) < 1e-9) {
      stop("window too long")
    }
    fit_ols(x, y)
  }
  decays <- c(0.2, 0.3, 0.4, 0.5, 0.6)
  expect_warning(
    tuned <- tune_carryover(spend, kpi, fit_fn = flaky,
                            predict_fn = stats::predict, decays = decays),
    "window too long"
  )
  # Training windows of 50 to 80 periods: 31 of the 50 splits.
  expect_equal(tuned$n_splits, 50)
  expect_equal(tuned$n_ok, 31)
  expect_true(all(tuned$results$n_splits == 31))
  rmse_on_common <- function(d) {
    z <- adstock_geometric(spend, d)
    e <- vapply(50:80, function(k) {
      m <- fit_ols(z[1:k], kpi[1:k])
      kpi[k + 1] - predict(m, z[k + 1])
    }, numeric(1))
    sqrt(mean(e^2))
  }
  for (d in decays) {
    expect_equal(tuned$results$metric[tuned$results$decay == d],
                 rmse_on_common(d))
  }
  printed <- paste(capture.output(print(tuned), type = "message"),
                   collapse = " ")
  expect_match(printed, "31 scored")
})

test_that("a failure in a user function is quoted and attributed", {
  set.seed(2)
  x <- pmax(0, rnorm(40, 500, 200))
  y <- 10 + 0.4 * adstock_geometric(x, 0.5) + rnorm(40)
  bad_fit <- function(x, y) stop("singular design here")
  expect_error(tune_carryover(x, y, fit_fn = bad_fit, decays = 0.5),
               "`fit_fn` failed: singular design here")
  bad_predict <- function(m, x) stop("no such column")
  expect_error(tune_carryover(x, y, predict_fn = bad_predict, decays = 0.5),
               "`predict_fn` failed: no such column")
  bad_metric <- function(a, p) stop("metric exploded")
  expect_error(tune_carryover(x, y, metric_fn = bad_metric, decays = 0.5),
               "`metric_fn` failed: metric exploded")
})

# ---- warm start ----------------------------------------------------------------------

test_that("the warm-start seed uses no media from beyond the first training window", {
  set.seed(6)
  n <- 40
  x <- pmax(0, rnorm(n, 500, 200))
  y <- 50 + 0.3 * adstock_geometric(x, 0.95) + rnorm(n)
  first_window <- function(x) {
    seen <- NULL
    fit <- function(z, y) {
      if (is.null(seen)) seen <<- z
      fit_ols(z, y)
    }
    tune_carryover(x, y, fit_fn = fit, predict_fn = stats::predict,
                   decays = 0.95, initial = 10, warm_start = TRUE)
    seen
  }
  later <- x
  later[30] <- later[30] + 5000
  expect_length(first_window(x), 10)
  expect_equal(first_window(later), first_window(x))
})

test_that("a grid point that cannot be warm-started is dropped up front", {
  set.seed(1)
  n <- 120
  x <- pmax(0, rnorm(n, 500, 200))
  y <- 100 + 0.5 * adstock_geometric(x, 0.3) + rnorm(n, sd = 5)
  expect_message(
    tuned <- tune_carryover(x, y, decays = c(0.3, 0.5, 1), normalise = FALSE,
                            warm_start = TRUE),
    "steady state"
  )
  expect_equal(tuned$best$decay, 0.3)
  expect_false(anyNA(tuned$results$metric))
})

# ---- parallel ------------------------------------------------------------------------

test_that("one failing element does not fail its neighbours in a parallel worker", {
  skip_on_os("windows")
  skip_on_cran()
  skip_if_not(isTRUE(parallel::detectCores(logical = FALSE) >= 2))
  out <- .mm_maybe_parallel(1:4, function(i) if (i == 3) stop("boom") else i,
                            cores = 2)
  expect_equal(out[c(1, 2, 4)], list(1L, 2L, 4L))
  expect_s3_class(out[[3]], "try-error")

  set.seed(1)
  x <- pmax(0, rnorm(120, 500, 200))
  y <- 100 + 0.5 * adstock_geometric(x, 0.3) + rnorm(120, sd = 5)
  serial <- tune_carryover(x, y, decays = c(0.2, 0.3, 0.5))
  forked <- tune_carryover(x, y, decays = c(0.2, 0.3, 0.5), cores = 2)
  expect_equal(forked$results, serial$results)
})
