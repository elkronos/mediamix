# Tests for R/metrics.R (rmse(), mae()) and R/fit-ols.R (fit_ols(),
# predict.mm_ols()). fit_ols is the default fitter tune_carryover() uses, so
# its round-trip correctness is tested here alongside the metrics that score it.

# ---- rmse() / mae(): hand-computed values --------------------------------------

test_that("rmse() matches a hand-computed value", {
  actual <- c(1, 2, 3)
  predicted <- c(1.1, 1.9, 3.2)
  # diffs: -0.1, 0.1, -0.2; squares: 0.01, 0.01, 0.04; mean = 0.02; sqrt(0.02)
  expect_equal(rmse(actual, predicted), sqrt(0.02), tolerance = 1e-12)
})

test_that("mae() matches a hand-computed value", {
  actual <- c(1, 2, 3)
  predicted <- c(1.1, 1.9, 3.2)
  # abs diffs: 0.1, 0.1, 0.2; mean = 0.1333...
  expect_equal(mae(actual, predicted), (0.1 + 0.1 + 0.2) / 3, tolerance = 1e-12)
})

test_that("rmse() of a perfect prediction is exactly 0", {
  x <- c(3, -1, 7, 22.5)
  expect_equal(rmse(x, x), 0)
  expect_equal(mae(x, x), 0)
})

test_that("rmse() is at least as large as mae() (Jensen / power-mean inequality)", {
  set.seed(55)
  actual <- rnorm(30)
  predicted <- actual + rnorm(30, sd = 2)
  expect_gte(rmse(actual, predicted), mae(actual, predicted))
})

# ---- NA / non-finite handling ---------------------------------------------------

test_that("na_rm = TRUE (the default) drops incomplete pairs before scoring", {
  actual <- c(1, NA_real_, 3)
  predicted <- c(1, 2, 4)
  # Only the pairs (1,1) and (3,4) survive: diffs 0 and -1.
  expect_equal(mae(actual, predicted), mean(c(0, 1)), tolerance = 1e-12)
  expect_equal(rmse(actual, predicted), sqrt(mean(c(0, 1)^2)), tolerance = 1e-12)
})

test_that("non-finite values (Inf/-Inf) are treated like missing under na_rm = TRUE", {
  actual <- c(1, Inf, 3)
  predicted <- c(1, 2, 4)
  expect_equal(mae(actual, predicted), mean(c(0, 1)), tolerance = 1e-12)
})

test_that("na_rm = FALSE lets a single NA propagate to an NA result", {
  actual <- c(1, NA_real_, 3)
  predicted <- c(1, 2, 4)
  expect_true(is.na(rmse(actual, predicted, na_rm = FALSE)))
  expect_true(is.na(mae(actual, predicted, na_rm = FALSE)))
})

test_that("an all-missing input returns NA_real_ rather than erroring or NaN", {
  out <- rmse(c(NA_real_, NA_real_), c(1, 2))
  expect_true(is.na(out))
  expect_identical(out, NA_real_)
  expect_identical(mae(c(NA_real_, NA_real_), c(1, 2)), NA_real_)
})

# ---- input validation -------------------------------------------------------------

test_that("rmse()/mae() require actual and predicted to be the same length", {
  expect_error(rmse(1:3, 1:4), class = "rlang_error", regexp = "same length")
  expect_error(mae(1:3, 1:4), class = "rlang_error", regexp = "same length")
})

test_that("rmse()/mae() require numeric inputs", {
  expect_error(rmse(letters[1:3], 1:3), class = "rlang_error",
               regexp = "numeric")
  expect_error(mae(c(TRUE, FALSE), 1:2), class = "rlang_error")
})

# ---- fit_ols() / predict.mm_ols(): exact round trip -----------------------------

test_that("fit_ols() recovers the exact intercept and slope on a perfectly linear relationship", {
  x <- 1:10
  y <- 3 + 2 * x
  m <- fit_ols(x, y)
  expect_s3_class(m, "mm_ols")
  expect_equal(m$intercept, 3, tolerance = 1e-10)
  expect_equal(m$slope, 2, tolerance = 1e-10)
  expect_identical(m$n, 10L)
})

test_that("predict.mm_ols() reproduces y exactly on the fitted data and extrapolates linearly", {
  x <- 1:10
  y <- 3 + 2 * x
  m <- fit_ols(x, y)
  expect_equal(predict(m, x), y, tolerance = 1e-10)
  # Extrapolation follows the same line.
  expect_equal(predict(m, c(-5, 0, 100)), 3 + 2 * c(-5, 0, 100), tolerance = 1e-10)
})

test_that("fit_ols() with a negative slope also round-trips exactly", {
  x <- seq(-5, 5, by = 0.5)
  y <- 10 - 3 * x
  m <- fit_ols(x, y)
  expect_equal(m$intercept, 10, tolerance = 1e-8)
  expect_equal(m$slope, -3, tolerance = 1e-8)
})

test_that("fit_ols() degrades gracefully when x has no variance", {
  # denom = n*sxx - sx^2 == 0 when x is constant: slope collapses to 0 and
  # intercept falls back to the mean of y, rather than dividing by zero.
  x <- rep(5, 10)
  y <- 1:10
  m <- fit_ols(x, y)
  expect_equal(m$slope, 0)
  expect_equal(m$intercept, mean(y), tolerance = 1e-10)
  expect_false(anyNA(predict(m, x)))
})

test_that("fit_ols() drops non-finite pairs before fitting", {
  x <- c(1, 2, 3, NA, Inf)
  y <- c(2, 4, 6, 10, 10)
  m <- fit_ols(x, y)
  expect_identical(m$n, 3L)
  expect_equal(m$intercept, 0, tolerance = 1e-8)
  expect_equal(m$slope, 2, tolerance = 1e-8)
})

test_that("fit_ols() handles a single observation without erroring", {
  m <- fit_ols(5, 7)
  expect_identical(m$n, 1L)
  expect_equal(m$intercept, 7)
  expect_equal(m$slope, 0)
})

test_that("fit_ols() requires x and y of the same length", {
  expect_error(fit_ols(1:5, 1:4), class = "rlang_error", regexp = "same length")
})

test_that("fit_ols() feeding tune_carryover()'s default composes correctly with rmse()", {
  spend <- adstock_geometric(c(100, 50, 0, 0, 200, 100, 0, 50), decay = 0.5)
  set.seed(99)
  kpi <- 10 + 0.4 * spend + rnorm(8, sd = 0.01)
  m <- fit_ols(spend, kpi)
  err <- rmse(kpi, predict(m, spend))
  expect_lt(err, 0.05)  # near-perfect fit up to the small injected noise
})
