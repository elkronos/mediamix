# Tests for tune_carryover_joint(), controls in tune_carryover(), and
# adstock_steady_state().

sim_two <- function(seed = 1, n = 150, noise = 10) {
  set.seed(seed)
  a <- pmax(0, rnorm(n, 500, 250))
  b <- pmax(0, rnorm(n, 300, 150))
  trend <- seq_len(n)
  y <- 100 + 0.6 * adstock_geometric(a, 0.7) + 0.9 * adstock_geometric(b, 0.2) +
    2 * trend + rnorm(n, sd = noise)
  list(media = data.frame(a = a, b = b), y = y,
       controls = data.frame(trend = trend))
}

# Pooled RMSE of rolling-origin forecasts from lm() on the set of decays given,
# computed without the package's own resampling code.
joint_rmse_by_hand <- function(s, decays) {
  X <- cbind(as.data.frame(Map(function(x, d) adstock_geometric(x, d),
                               s$media, decays)), s$controls)
  n <- length(s$y)
  e <- vapply(floor(n / 2):(n - 1), function(k) {
    f <- stats::lm(yy ~ ., data = cbind(yy = s$y[1:k], X[1:k, , drop = FALSE]))
    s$y[k + 1] - stats::predict(f, X[k + 1, , drop = FALSE])
  }, numeric(1))
  sqrt(mean(e^2))
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
  # Channel b lands on the lowest decay offered, which warns.
  j <- suppressWarnings(tune_carryover_joint(s$media, s$y,
                                             controls = s$controls,
                                             decays = seq(0.1, 0.9, by = 0.2)))
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

# ---- the one-standard-error set ------------------------------------------------

test_that("the one-SE set is scored as a whole", {
  s <- sim_two(4, noise = 80)
  j <- tune_carryover_joint(s$media, s$y, controls = s$controls,
                            decays = seq(0.1, 0.9, by = 0.1))
  expect_false(identical(j$best_1se$decay, j$best$decay))
  expect_true(all(j$best_1se$decay <= j$best$decay))
  expect_equal(j$metric, joint_rmse_by_hand(s, j$best$decay))
  expect_equal(j$best_1se_metric, joint_rmse_by_hand(s, j$best_1se$decay))
  table <- capture.output(msgs <- capture.output(print(j), type = "message"))
  expect_match(paste(msgs, collapse = " "), "One-SE set")
})

test_that("metric holds the value and metric_name the name in both tuners", {
  s <- sim_two()
  j <- tune_carryover_joint(s$media, s$y, controls = s$controls,
                            decays = c(0.2, 0.7), skip = 9)
  one <- tune_carryover(s$media$a, s$y, decays = c(0.2, 0.7), skip = 9)
  expect_type(j$metric, "double")
  expect_type(one$metric, "double")
  expect_identical(j$metric_name, one$metric_name)
})

# ---- grid edges and validation --------------------------------------------------

test_that("the joint tuner warns when a channel sits on the edge of the grid", {
  s <- sim_two()
  w <- NULL
  j <- withCallingHandlers(
    tune_carryover_joint(s$media, s$y, controls = s$controls,
                         decays = c(0.3, 0.4, 0.5)),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(w, 1L)
  expect_match(w, "a: the selected decay \\(0.5\\) is the largest value")
  expect_match(w, "b: the selected decay \\(0.3\\) is the smallest value")
  expect_no_warning(
    tune_carryover_joint(s$media, s$y, controls = s$controls,
                         decays = seq(0.1, 0.9, by = 0.1))
  )
})

test_that("the joint tuner refuses media and starts it cannot use", {
  s <- sim_two()
  m <- as.matrix(s$media)
  colnames(m) <- NULL
  expect_error(tune_carryover_joint(m, s$y), class = "rlang_error",
               regexp = "column names")
  expect_error(tune_carryover_joint(cbind(s$media, flat = 3), s$y),
               class = "rlang_error", regexp = "constant")
  bad <- s$media
  bad$a[3] <- Inf
  expect_error(tune_carryover_joint(bad, s$y), class = "rlang_error",
               regexp = "infinite")
  expect_error(tune_carryover_joint(s$media, s$y, start = c(a = NA)),
               class = "rlang_error", regexp = "start")
  expect_error(tune_carryover_joint(s$media, s$y, start = c(z = 0.5)),
               class = "rlang_error", regexp = "not in")
  expect_error(tune_carryover_joint(s$media, s$y,
                                    controls = data.frame(a = rnorm(150))),
               class = "rlang_error", regexp = "media column")
})

test_that("a missing control stops the joint tuner instead of shifting rows", {
  s <- sim_two()
  ctrl <- s$controls
  ctrl$trend[1:75] <- NA
  expect_error(tune_carryover_joint(s$media, s$y, controls = ctrl),
               class = "rlang_error", regexp = "trend")
})

test_that("a fit that fails for some grid points is compared on common splits", {
  s <- sim_two()
  # Fails on long windows whenever channel b is at decay 0.5, which is not
  # where b ends up, so b's final profile is scored on the short windows only.
  fit <- function(X, y) {
    if (nrow(X) > 120 && abs(X$b[1] - 0.5 * s$media$b[1]) < 1e-9) {
      stop("too many rows")
    }
    stats::lm.fit(cbind(1, as.matrix(X)), y)$coefficients
  }
  pred <- function(m, X) as.numeric(cbind(1, as.matrix(X)) %*% m)
  expect_warning(
    j <- tune_carryover_joint(s$media, s$y, controls = s$controls,
                              fit_fn = fit, predict_fn = pred,
                              decays = c(0.1, 0.2, 0.5, 0.7, 0.8)),
    "too many rows"
  )
  # Training windows of 75 to 120 rows survive: 46 of the 75 splits.
  expect_equal(j$n_splits, 75)
  expect_equal(j$n_ok, 46)
  expect_equal(j$best$decay, c(0.7, 0.2))
})

# ---- speed --------------------------------------------------------------------------

test_that("cores spreads the joint search without changing it", {
  skip_on_os("windows")
  skip_on_cran()
  skip_if_not(isTRUE(parallel::detectCores(logical = FALSE) >= 2))
  s <- sim_two(3)
  a <- tune_carryover_joint(s$media, s$y, controls = s$controls,
                            decays = seq(0.1, 0.9, by = 0.2), skip = 4)
  b <- tune_carryover_joint(s$media, s$y, controls = s$controls,
                            decays = seq(0.1, 0.9, by = 0.2), skip = 4,
                            cores = 2)
  expect_identical(b$best, a$best)
  expect_identical(b$best_1se, a$best_1se)
  expect_equal(b$profiles, a$profiles)
})

# ---- converting the result ---------------------------------------------------------

test_that("the joint result converts to a data frame with the selections marked", {
  s <- sim_two(4, noise = 80)
  j <- tune_carryover_joint(s$media, s$y, controls = s$controls,
                            decays = seq(0.1, 0.9, by = 0.1))
  df <- as.data.frame(j)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), nrow(j$profiles))
  expect_equal(df$decay[df$best], j$best$decay)
  expect_equal(df$channel[df$best], j$best$channel)
  expect_equal(df$decay[df$best_1se], j$best_1se$decay)
  skip_if_not_installed("generics")
  expect_identical(generics::tidy(j), df)
})

# ---- panel data -------------------------------------------------------------------

test_that("the joint tuner adstocks each series on its own", {
  set.seed(7)
  n <- 104
  d <- do.call(rbind, lapply(c("east", "west"), function(g) {
    a <- pmax(0, rnorm(n, 500, 250))
    b <- pmax(0, rnorm(n, 300, 150))
    data.frame(geo = g, week = seq_len(n), a = a, b = b,
               y = 100 + 0.6 * adstock_geometric(a, 0.7) +
                 0.9 * adstock_geometric(b, 0.2) + rnorm(n, sd = 10))
  }))
  run <- function(d) {
    tune_carryover_joint(d[c("a", "b")], d$y, by = d$geo, index = d$week,
                         controls = data.frame(geo = d$geo),
                         decays = seq(0.1, 0.9, by = 0.1))
  }
  j <- run(d)
  expect_equal(j$best$decay, c(0.7, 0.2))
  expect_equal(j$n_series, 2)
  set.seed(1)
  shuffled <- run(d[sample(nrow(d)), ])
  expect_equal(shuffled$profiles, j$profiles)
})

# ---- saved predictions -----------------------------------------------------------

test_that("save_pred keeps the forecasts of best and of the one-SE set", {
  s <- sim_two(4, noise = 80)
  decays <- seq(0.1, 0.9, by = 0.1)
  plain <- tune_carryover_joint(s$media, s$y, controls = s$controls,
                                decays = decays)
  j <- tune_carryover_joint(s$media, s$y, controls = s$controls,
                            decays = decays, save_pred = TRUE)
  expect_null(plain$predictions)
  expect_identical(unclass(j)[names(plain)], unclass(plain))

  p <- j$predictions
  expect_identical(names(p), c("selection", "split", "row", "max_lag_a",
                               "decay_a", "max_lag_b", "decay_b", "observed",
                               "predicted"))
  expect_identical(unique(p$selection), c("best", "best_1se"))
  expect_equal(nrow(p), 2 * j$n_splits)
  best <- p[p$selection == "best", ]
  one_se <- p[p$selection == "best_1se", ]
  expect_equal(unique(best$decay_a), j$best$decay[1])
  expect_equal(unique(best$decay_b), j$best$decay[2])
  expect_equal(unique(one_se$decay_a), j$best_1se$decay[1])
  expect_equal(unique(one_se$decay_b), j$best_1se$decay[2])
  expect_identical(best$row, best$split + 75L)
  expect_equal(best$observed, s$y[best$row])
  expect_equal(rmse(best$observed, best$predicted), j$metric)
  expect_equal(rmse(one_se$observed, one_se$predicted), j$best_1se_metric)

  # One forecast checked by hand: the best set, last split.
  X <- cbind(as.data.frame(Map(function(x, d) adstock_geometric(x, d),
                               s$media, j$best$decay)), s$controls)
  fit <- stats::lm(yy ~ ., data = cbind(yy = s$y[1:149], X[1:149, ]))
  expect_equal(best$predicted[best$split == 75L],
               unname(stats::predict(fit, X[150, , drop = FALSE])))

  table <- capture.output(msgs <- capture.output(print(j), type = "message"))
  expect_match(paste(msgs, collapse = " "), "150 out-of-sample predictions kept")
  expect_identical(as.data.frame(j), as.data.frame(plain))
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  drawn <- plot(j)
  grDevices::dev.off()
  unlink(f)
  expect_identical(drawn, j)
  skip_if_not_installed("generics")
  expect_identical(generics::tidy(j), generics::tidy(plain))
})

test_that("when the one-SE set is best, its rows repeat those of best", {
  s <- sim_two(3)
  j <- tune_carryover_joint(s$media, s$y, controls = s$controls,
                            decays = c(0.2, 0.7), skip = 9, save_pred = TRUE)
  expect_identical(j$best_1se, j$best)
  p <- j$predictions
  best <- p[p$selection == "best", setdiff(names(p), "selection")]
  one_se <- p[p$selection == "best_1se", setdiff(names(p), "selection")]
  expect_equal(one_se, best, ignore_attr = TRUE)
  expect_equal(rmse(best$observed, best$predicted), j$metric)
})

test_that("joint panel predictions carry index and group", {
  set.seed(7)
  n <- 104
  d <- do.call(rbind, lapply(c("east", "west"), function(g) {
    a <- pmax(0, rnorm(n, 500, 250))
    b <- pmax(0, rnorm(n, 300, 150))
    data.frame(geo = g, week = seq_len(n), a = a, b = b,
               y = 100 + 0.6 * adstock_geometric(a, 0.7) +
                 0.9 * adstock_geometric(b, 0.2) + rnorm(n, sd = 10))
  }))
  d <- d[sample(nrow(d)), ]
  j <- tune_carryover_joint(d[c("a", "b")], d$y, by = d$geo, index = d$week,
                            controls = data.frame(geo = d$geo),
                            decays = seq(0.1, 0.9, by = 0.1),
                            save_pred = TRUE)
  p <- j$predictions
  expect_identical(names(p)[1:5], c("selection", "split", "row", "index",
                                    "group"))
  expect_identical(p$index, d$week[p$row])
  expect_identical(p$group, d$geo[p$row])
  expect_equal(p$observed, d$y[p$row])
})

test_that("the joint save_pred is validated and keeps channel names as given", {
  s <- sim_two()
  expect_error(tune_carryover_joint(s$media, s$y, decays = 0.5,
                                    save_pred = "yes"),
               class = "rlang_error", regexp = "save_pred")
  spaced <- stats::setNames(s$media, c("TV Spend", "search"))
  j <- tune_carryover_joint(spaced, s$y, controls = s$controls,
                            decays = c(0.2, 0.7), skip = 9, save_pred = TRUE)
  expect_true(all(c("decay_TV Spend", "max_lag_TV Spend") %in%
                    names(j$predictions)))
})

test_that("cores = 2 keeps the same joint predictions", {
  skip_on_os("windows")
  skip_on_cran()
  skip_if_not(isTRUE(parallel::detectCores(logical = FALSE) >= 2))
  s <- sim_two(3)
  a <- tune_carryover_joint(s$media, s$y, controls = s$controls,
                            decays = seq(0.1, 0.9, by = 0.2), skip = 4,
                            save_pred = TRUE)
  b <- tune_carryover_joint(s$media, s$y, controls = s$controls,
                            decays = seq(0.1, 0.9, by = 0.2), skip = 4,
                            save_pred = TRUE, cores = 2)
  expect_identical(b$predictions, a$predictions)
})
