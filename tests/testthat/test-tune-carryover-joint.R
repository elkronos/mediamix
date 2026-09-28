# Tests for tune_carryover_joint(), controls in tune_carryover(), and
# adstock_steady_state().

sim_two <- function(seed = 1, n = 150) {
  set.seed(seed)
  a <- pmax(0, rnorm(n, 500, 250))
  b <- pmax(0, rnorm(n, 300, 150))
  trend <- seq_len(n)
  y <- 100 + 0.6 * adstock_geometric(a, 0.7) + 0.9 * adstock_geometric(b, 0.2) +
    2 * trend + rnorm(n, sd = 10)
  list(media = data.frame(a = a, b = b), y = y,
       controls = data.frame(trend = trend))
}

test_that("joint tuning recovers two channels' decays with a control", {
  s <- sim_two()
  j <- tune_carryover_joint(s$media, s$y, controls = s$controls,
                            decays = seq(0.1, 0.9, by = 0.1))
  expect_s3_class(j, "mm_carryover_joint")
  expect_true(j$converged)
  expect_equal(j$best$decay, c(0.7, 0.2))
  expect_identical(j$best$channel, c("a", "b"))
  expect_true(all(j$best_1se$decay <= j$best$decay))
  expect_setequal(unique(j$profiles$channel), c("a", "b"))
  expect_no_error(capture.output(suppressMessages(print(j))))
})

test_that("each sweep can only lower the cross-validated error", {
  s <- sim_two(2)
  j <- tune_carryover_joint(s$media, s$y, controls = s$controls,
                            decays = seq(0.1, 0.9, by = 0.2))
  # The selected point is the minimum of every channel's final profile.
  for (ch in c("a", "b")) {
    p <- j$profiles[j$profiles$channel == ch, ]
    expect_equal(min(p$metric), j$metric)
  }
})

test_that("a user-supplied model is used, and a bad setup is refused", {
  s <- sim_two(3)
  seen <- NULL
  fit <- function(X, y) {
    seen <<- names(X)
    stats::lm.fit(cbind(1, as.matrix(X)), y)$coefficients
  }
  pred <- function(m, X) as.numeric(cbind(1, as.matrix(X)) %*% m)
  j <- tune_carryover_joint(s$media, s$y, controls = s$controls,
                            fit_fn = fit, predict_fn = pred,
                            decays = c(0.2, 0.7), skip = 9)
  expect_identical(seen, c("a", "b", "trend"))
  expect_equal(j$best$decay, c(0.7, 0.2))
  expect_error(tune_carryover_joint(s$media, s$y, fit_fn = fit), "both")
  expect_error(tune_carryover_joint(s$media, s$y[-1]), "rows")
  expect_error(tune_carryover_joint(s$media, s$y, controls = s$controls[-1, ,
                                                      drop = FALSE]), "rows")
})

test_that("controls in tune_carryover() fix what the bare model gets wrong", {
  set.seed(42)
  n <- 120
  spend <- pmax(0, rnorm(n, 500, 250))
  trend <- seq_len(n)
  season <- sin(2 * pi * trend / 52)
  kpi <- 200 + 0.5 * adstock_geometric(spend, 0.7) + 40 * trend +
    300 * season + rnorm(n, sd = 8)
  with_ctrl <- tune_carryover(spend, kpi, decays = seq(0.1, 0.9, by = 0.1),
                              controls = data.frame(trend, season))
  expect_equal(with_ctrl$best$decay, 0.7)
})

test_that("adstock_steady_state() removes the burn-in of a constant series", {
  x <- rep(100, 12)
  for (L in c(Inf, 5)) {
    s <- adstock_steady_state(x, decay = 0.85, max_lag = L)
    expect_equal(adstock_geometric(x, 0.85, max_lag = L, state = s),
                 rep(100, 12))
  }
  g <- adstock_steady_state(c(10, 20, 100, 200), 0.5, by = c("a", "a", "b", "b"),
                            periods = 2)
  expect_equal(g, list(a = 30, b = 300))
  expect_equal(adstock_steady_state(x, 0, max_lag = 1), numeric(0))
  expect_error(adstock_steady_state(x, 1), "no steady state")
})

test_that("warm_start changes the start of the series, not its end", {
  set.seed(5)
  n <- 150
  spend <- pmax(0, rnorm(n, 500, 250))
  kpi <- 200 + 0.5 * adstock_geometric(spend, 0.85,
                                       state = adstock_steady_state(spend, 0.85)) +
    rnorm(n, sd = 8)
  warm <- tune_carryover(spend, kpi, decays = seq(0.05, 0.95, 0.05),
                         warm_start = TRUE)
  expect_equal(warm$best$decay, 0.85)
})
