# Tests for R/tune-carryover.R: tune_carryover() and its resampling schemes.
#
# The recovery test below is, per the package's design doc, the single most
# important test in the suite: it guards the corrected CV objective (put the
# KPI in the loop) and the fast/general path equivalence that makes that
# objective cheap to evaluate at scale.

# ---- recovery: the central test --------------------------------------------

test_that("tune_carryover() recovers a known decay from data generated with it", {
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
                  c("max_lag", "decay", "half_life", "metric", "n_splits"))
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
