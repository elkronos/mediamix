# Regressions for the defects found in the second audit. Each block names the
# behaviour that was wrong, not the fix, so the test still means something if
# the implementation changes.

# ---- Module A ---------------------------------------------------------------

test_that("the fast OLS path is indistinguishable from the general path", {
  x <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120)
  y <- c(NA, NA, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
  fast <- tune_carryover(x, y, max_lags = Inf, decays = 0.2, initial = 2L)
  general <- tune_carryover(x, y, fit_fn = function(u, v) fit_ols(u, v),
                            max_lags = Inf, decays = 0.2, initial = 2L)
  expect_equal(fast$best$metric, general$best$metric)

  set.seed(11)
  z <- pmax(0, rnorm(80, 500, 200))
  k <- 100 + 0.4 * adstock_geometric(z, 0.5) + rnorm(80, sd = 5)
  g <- seq(0.1, 0.9, by = 0.1)
  a <- suppressWarnings(tune_carryover(z, k, max_lags = Inf, decays = g))
  b <- suppressWarnings(tune_carryover(z, k, fit_fn = function(u, v) fit_ols(u, v),
                                       max_lags = Inf, decays = g))
  expect_equal(a$results$metric, b$results$metric)
  expect_equal(a$best$decay, b$best$decay)
})

test_that("a series too short for the scheme errors informatively", {
  expect_error(tune_carryover(rnorm(12), rnorm(12), max_lags = Inf,
                              decays = 0.5, assess = 7L), "too short")
  expect_error(tune_carryover(rnorm(12), rnorm(12), max_lags = Inf,
                              decays = 0.5, initial = 20L), "less than")
})

test_that("bounded saturation curves stay bounded at extreme inputs", {
  expect_equal(saturate_hill(c(1e150, 1e200), half_max = 100, shape = 2),
               c(1, 1))
  expect_true(all(is.finite(saturate_hill(c(10, 100, 1000), 100, shape = 400))))
  expect_equal(saturate_hill(Inf, half_max = 100), 1)
  expect_equal(saturate_michaelis_menten(Inf, vmax = 1, km = 100), 1)
  expect_equal(saturate_exponential(Inf, rate = 0.01), 1)
  # The identity that ties the two parameterisations together must survive.
  x <- c(0, 1, 50, 300, 1e6)
  expect_equal(saturate_hill(x, 100, shape = 1),
               saturate_michaelis_menten(x, vmax = 1, km = 100))
  expect_equal(saturate_hill(100, half_max = 100), 0.5)
})

test_that("the OLS singularity guard is scale invariant", {
  x <- c(1, 2, 3, 4, 5) * 1e-5
  expect_equal(fit_ols(x, 3 + 2 * x)$slope, 2, tolerance = 1e-6)
  x2 <- c(1, 2, 3, 4, 5) * 1e5
  expect_equal(fit_ols(x2, 3 + 2 * x2)$slope, 2, tolerance = 1e-6)
  # A genuinely constant regressor still yields no slope.
  expect_equal(fit_ols(rep(3, 5), c(1, 2, 3, 4, 5))$slope, 0)
})

test_that("errors name the function the user called, not its caller", {
  called <- function(expr) {
    e <- tryCatch(expr, error = function(e) e)
    if (is.null(conditionCall(e))) return(NA_character_)
    deparse(conditionCall(e))[1L]
  }
  # Previously this named `f()` -- the caller of adstock_geometric -- because
  # the internal validator reached one frame too far up.
  f <- function() adstock_geometric(c(1, NA, 3), decay = 0.5)
  expect_match(called(f()), "adstock_geometric")

  # And these named package internals rather than the exported function.
  expect_match(called(tune_carryover(rnorm(20), rnorm(20), max_lags = Inf,
                                     decays = 0.5, assess = 2.5)),
               "tune_carryover")
  expect_match(called(adstock_geometric(1:4, 0.5, by = c("a", "a", "b", "b"),
                                        state = list(a = c(1, 2), b = c(1, 2)))),
               "adstock_geometric")
  # A negative value reaching saturation through media_transform used to dump
  # the entire body of `saturate` into the error.
  msg <- tryCatch(
    media_transform(c(1, -2), adstock = list(decay = 0.5),
                    saturation = list(half_max = 1)),
    error = function(e) paste(deparse(conditionCall(e)), collapse = " ")
  )
  expect_lt(nchar(msg), 200L)
})

test_that("normalise is validated wherever it is accepted", {
  expect_error(adstock_geometric(c(1, 2, 3), 0.5, normalise = "yes"), "normalise")
  expect_error(adstock_geometric(c(1, 2, 3), 0.5, normalise = 1), "normalise")
  expect_error(adstock_geometric(c(1, 2, 3), 0.5, normalise = NA), "normalise")
  expect_error(tune_carryover(rnorm(20), rnorm(20), max_lags = Inf,
                              decays = 0.5, normalise = "yes"), "normalise")
})

test_that("counts too large to represent error rather than becoming NA", {
  expect_error(adstock_geometric(c(1, 2, 3), 0.5, max_lag = 1e10), "too large")
  expect_error(effective_window(0.999999999, 0.9), "too long")
  # The ordinary cases are unaffected.
  expect_equal(effective_window(decay_from_half_life(3), 0.9), 10L)
  expect_length(adstock_geometric(1:5, 0.5, max_lag = 3), 5L)
})

test_that("a Weibull kernel that vanishes says which way to move the scale", {
  expect_error(adstock_weights_weibull(8, shape = 2, scale = 1e-8, type = "pdf"),
               "Increase")
})

test_that("the metric label is usable for anonymous functions", {
  set.seed(3)
  x <- pmax(0, rnorm(30, 500, 150))
  y <- 100 + 0.3 * adstock_geometric(x, 0.5) + rnorm(30)
  anon <- suppressWarnings(
    tune_carryover(x, y, max_lags = Inf, decays = c(0.3, 0.5),
                   metric_fn = function(a, p) sqrt(mean((a - p)^2))))
  expect_equal(anon$metric, "<anonymous>")
  named <- suppressWarnings(
    tune_carryover(x, y, max_lags = Inf, decays = 0.5, metric_fn = mae))
  expect_equal(named$metric, "mae")
})

test_that("media_transform validates by and no longer saturates by default", {
  expect_error(
    media_transform(c(1, 2, 3), adstock = list(kernel = "none"),
                    saturation = list(half_max = 1), by = c("a", "b")),
    "same length"
  )
  # Saturation is off unless asked for.
  expect_equal(media_transform(c(0, 500, 800), adstock = list(decay = 0.5)),
               adstock_geometric(c(0, 500, 800), 0.5))
  # A curve that needs a parameter says which one.
  expect_error(
    media_transform(c(1, 2), adstock = list(decay = 0.5),
                    saturation = list(type = "hill")),
    "half_max"
  )
})

test_that("a NULL entry in a per-group state is rejected", {
  expect_error(
    adstock_geometric(1:4, 0.5, by = c("a", "a", "b", "b"),
                      state = list(a = NULL, b = 10)),
    "NULL"
  )
})

# ---- Module B ---------------------------------------------------------------

test_that("credit_custom declines a journey rather than inventing an answer", {
  d <- data.frame(c = "a", ch = c("x", "y"),
                  t = as.POSIXct("2024-01-01", tz = "UTC") + c(0, 86400),
                  v = c(0, 1))
  p <- build_paths(d, id = "c", channel = "ch", timestamp = "t",
                   conversion = "v")
  zero <- suppressMessages(credit_custom(p, function(r, n, x) c(0, 0)))
  expect_equal(sum(zero$credit), 0)
  expect_error(credit_custom(p, function(r, n, x) c(Inf, 1)), "finite")
  expect_error(credit_custom(p, function(r, n, x) c(-1, 1)), "non-negative")
})

test_that("credit_time_decay does not degenerate when the kernel underflows", {
  d <- data.frame(customer_id = "a", channel = c("tv", "search", "email"),
                  timestamp = as.POSIXct("2024-01-01", tz = "UTC") +
                    c(0, 1, 3) * 86400,
                  conversion = c(0, 0, 1))
  p <- build_paths(d, id = "customer_id", channel = "channel",
                   timestamp = "timestamp", conversion = "conversion",
                   units = "secs")
  cr <- credit_time_decay(p)$credit
  expect_equal(sum(cr), 1)
  expect_false(isTRUE(all.equal(cr, rep(1 / 3, 3))))
  expect_equal(which.max(cr), 3L)
  # decay = 0 puts everything on the conversion touch.
  expect_equal(credit_time_decay(p, decay = 0)$credit, c(0, 0, 1))
})

test_that("a total wipeout still reports what went in", {
  d <- data.frame(customer_id = c("a", "b"), channel = c("direct", "direct"),
                  timestamp = as.POSIXct("2024-01-01", tz = "UTC") + c(0, 1),
                  conversion = c(1, 1))
  p <- suppressWarnings(build_paths(d, id = "customer_id", channel = "channel",
                                    timestamp = "timestamp",
                                    conversion = "conversion", direct = "drop"))
  s <- path_summary(p)
  expect_equal(s$events_in, 2L)
  expect_equal(s$conversions_observed, 2L)
  expect_equal(s$conversions_unattributable, 2L)
})

test_that("attribute() and attribution_spread() survive an empty journey table", {
  p <- build_paths(data.frame(customer_id = character(0),
                              channel = character(0),
                              timestamp = as.POSIXct(character(0))),
                   id = "customer_id", channel = "channel",
                   timestamp = "timestamp")
  expect_s3_class(attribute(p), "data.frame")
  expect_equal(nrow(attribute(p)), 0L)
  expect_equal(nrow(attribution_spread(attribute(p))), 0L)
})

test_that("difftime durations are validated like numeric ones", {
  d <- data.frame(customer_id = "a", channel = c("A", "B", "C", "D"),
                  timestamp = as.POSIXct("2024-01-01", tz = "UTC") +
                    c(0, 10, 20, 30) * 86400,
                  conversion = c(0, 0, 0, 1))
  B <- function(...) build_paths(d, id = "customer_id", channel = "channel",
                                 timestamp = "timestamp",
                                 conversion = "conversion", ...)
  expect_error(B(split_on = "gap", gap = as.difftime(NA_real_, units = "days")))
  expect_error(B(split_on = "gap", gap = as.difftime(c(5, 15), units = "days")))
  expect_error(B(lookback = as.difftime(-15, units = "days")))
  expect_no_error(B(lookback = as.difftime(15, units = "days")))

  dn <- data.frame(customer_id = "a", channel = c("A", "B", "C"),
                   timestamp = c(0, 10, 20), conversion = c(0, 0, 1))
  expect_error(
    build_paths(dn, id = "customer_id", channel = "channel",
                timestamp = "timestamp", conversion = "conversion",
                lookback = as.difftime(15, units = "days")),
    "difftime"
  )
})

test_that("a factor value column is read as revenue, not as level codes", {
  df <- data.frame(customer_id = "a", channel = c("A", "B"),
                   timestamp = as.POSIXct("2024-01-01", tz = "UTC") +
                     c(0, 86400),
                   conversion = c(0, 1), value = factor(c(NA, "250")))
  p <- build_paths(df, id = "customer_id", channel = "channel",
                   timestamp = "timestamp", conversion = "conversion",
                   value = "value")
  expect_equal(unique(p$conversion_value), 250)
})

test_that("dropping direct traffic does not move the lookback anchor", {
  d <- data.frame(customer_id = "a", channel = c("tv", "direct"),
                  timestamp = as.POSIXct("2024-01-01", tz = "UTC") +
                    c(0, 100) * 86400,
                  conversion = c(0, 0))
  B <- function(dir) build_paths(d, id = "customer_id", channel = "channel",
                                 timestamp = "timestamp",
                                 conversion = "conversion", lookback = 30,
                                 direct = dir)$channel
  expect_equal(B("keep"), "direct")
  # tv is 100 days before the last touch either way, so it stays excluded.
  expect_length(suppressWarnings(B("drop")), 0L)
})

test_that("path_summary reports conversion events distinctly from journeys", {
  d <- data.frame(customer_id = "a", channel = c("A", "B", "C", "D"),
                  timestamp = as.POSIXct("2024-01-01", tz = "UTC") +
                    c(0, 1, 2, 3) * 86400,
                  conversion = c(0, 1, 0, 1))
  p <- build_paths(d, id = "customer_id", channel = "channel",
                   timestamp = "timestamp", conversion = "conversion",
                   split_on = "none")
  expect_equal(path_summary(p)$conversion_events, 2L)
  expect_equal(path_summary(p)$conversions_observed, 1L)
})

test_that("an unused gap argument warns", {
  d <- data.frame(customer_id = "a", channel = c("A", "B"),
                  timestamp = as.POSIXct("2024-01-01", tz = "UTC") + c(0, 86400),
                  conversion = c(0, 1))
  expect_warning(
    build_paths(d, id = "customer_id", channel = "channel",
                timestamp = "timestamp", conversion = "conversion",
                split_on = "conversion", gap = 14),
    "gap"
  )
})

test_that("attribution_spread handles a blank channel label", {
  d <- data.frame(customer_id = c("a", "a", "b", "b"),
                  channel = c("", "tv", "tv", ""),
                  timestamp = as.POSIXct("2024-01-01", tz = "UTC") +
                    c(0, 1, 0, 1) * 86400,
                  conversion = c(0, 1, 0, 1))
  p <- build_paths(d, id = "customer_id", channel = "channel",
                   timestamp = "timestamp", conversion = "conversion",
                   direct = "label", direct_labels = "direct")
  sp <- attribution_spread(attribute(p, rules = c("first", "last")))
  expect_true(all(!is.na(sp$first_last_ratio)))
})

# ---- Modules C, D, E --------------------------------------------------------

test_that("contributions sum exactly to the model's fitted values", {
  data(mm_weekly, package = "mediamix")
  north <- mm_weekly[mm_weekly$geo == "north", ]
  ch <- c("tv", "video", "search", "social", "display")
  truth <- attr(mm_weekly, "truth")
  tr <- as.data.frame(Map(function(x, d, h, sh) {
    media_transform(x, adstock = list(decay = d),
                    saturation = list(half_max = h, shape = sh))
  }, north[ch], truth$decay[ch], truth$half_max[ch], truth$shape[ch]))
  md <- tr
  md$week <- seq_len(nrow(md))
  md$price <- north$price
  fit <- stats::lm(north$revenue ~ ., data = md)

  co <- contributions(tr, fit, index = north$date)
  totals <- as.numeric(tapply(co$contribution, co$period, sum))
  expect_equal(totals, unname(stats::fitted(fit)))
  # The baseline absorbs the controls, so it is not a constant.
  expect_gt(length(unique(round(co$contribution[co$channel == "(baseline)"], 6))), 1L)
})

test_that("an aliased coefficient is refused rather than propagated as NA", {
  set.seed(4)
  d <- data.frame(tv = rnorm(30))
  d$dup <- d$tv
  d$y <- rnorm(30)
  fit <- stats::lm(y ~ tv + dup, d)
  expect_error(contributions(d[, c("tv", "dup")], fit), "identified")
})

test_that("mroi is correct at zero spend", {
  expect_equal(mroi(0, coefficient = 1, type = "michaelis_menten",
                    vmax = 1, km = 100), 0.01, tolerance = 1e-5)
  expect_equal(mroi(0, coefficient = 1, half_max = 100, shape = 1),
               0.01, tolerance = 1e-5)
})

test_that("marginal ROI is above average below an S-curve inflection", {
  # The documented claim: concave curves have marginal below average
  # everywhere, S-curves have it above below the inflection.
  concave <- response_curve(seq(0, 100000, by = 20000), 5200,
                            half_max = 45000, shape = 1)
  expect_true(all(diff(concave$marginal) < 0))
  s_curve <- response_curve(seq(0, 100000, by = 20000), 5200,
                            half_max = 45000, shape = 1.6)
  expect_gt(s_curve$spend[which.max(s_curve$marginal)], 0)
})

test_that("spend_for needs a range for unbounded curves and works in reverse", {
  expect_error(spend_for(200000, 5200, type = "power", exponent = 0.5),
               "max_spend")
  expect_equal(spend_for(200000, 5200, type = "power", exponent = 0.5,
                         max_spend = 1e6), (200000 / 5200)^2, tolerance = 1e-4)
  expect_equal(spend_for(0.9, 1, type = "exponential", rate = 1e-4),
               -log(0.1) / 1e-4, tolerance = 1e-4)
  # A decreasing response still has a solvable target.
  expect_equal(spend_for(-2000, -5200, half_max = 45000, shape = 1.6),
               spend_for(2000, 5200, half_max = 45000, shape = 1.6))
  expect_warning(spend_for(9e9, 5200, type = "power", exponent = 0.5,
                           max_spend = 1e3), "unbounded")
})

test_that("diagnose_media groups when told to", {
  set.seed(5)
  g1 <- data.frame(geo = "A", tv = rnorm(100, 100, 10),
                   search = rnorm(100, 100, 10))
  g2 <- data.frame(geo = "B", tv = rnorm(100, 1000, 10),
                   search = rnorm(100, 1000, 10))
  d <- rbind(g1, g2)
  # Pooling two scale-separated series manufactures collinearity that is in
  # neither of them.
  expect_true(all(diagnose_media(d, media = c("tv", "search"),
                                 by = "geo")$collinearity$vif < 2))
  expect_true(any(diagnose_media(d, media = c("tv", "search"))$collinearity$vif > 5))
})

test_that("diagnose_media picks a real partner when a correlation is NA", {
  set.seed(6)
  d <- data.frame(a = runif(60), b = runif(60), c = rep(1, 60))
  cc <- diagnose_media(d, media = c("a", "b", "c"))$collinearity
  expect_equal(cc$most_correlated_with[cc$channel == "a"], "b")
})

test_that("one dead column does not produce an all-clear report", {
  data(mm_weekly, package = "mediamix")
  ch <- c("tv", "video", "search", "social", "display")
  d <- mm_weekly
  d$dead <- NA_real_
  z <- diagnose_media(d, media = c(ch, "dead"), by = "geo")
  expect_gte(sum(!is.na(z$collinearity$vif)), 5L)
  expect_true(any(grepl("No usable values", z$flags)))
  expect_true(is.na(z$collinearity$identified[z$collinearity$channel == "dead"]))
})

test_that("step_adstock enforces its preconditions at bake time too", {
  skip_if_not_installed("recipes")
  data(mm_weekly, package = "mediamix")
  tr <- mm_weekly[mm_weekly$date < as.Date("2025-07-01"), ]
  te <- mm_weekly[mm_weekly$date >= as.Date("2025-07-01"), ]
  rec <- recipes::recipe(revenue ~ ., data = tr) |>
    step_adstock(tv, index = "date", by = "geo", decay = 0.5) |>
    recipes::prep()

  north <- te[te$geo == "north", ]
  expect_error(recipes::bake(rec, new_data = rbind(north[1:3, ], north[1:3, ])),
               "repeated")
  expect_warning(recipes::bake(rec, new_data = north[-(3:8), ]),
                 "unevenly spaced")
})

test_that("all_predictors() selects media without tripping the type check", {
  skip_if_not_installed("recipes")
  data(mm_weekly, package = "mediamix")
  tr <- mm_weekly[mm_weekly$date < as.Date("2025-07-01"), ]
  expect_no_error(
    recipes::recipe(revenue ~ ., data = tr) |>
      step_adstock(recipes::all_predictors(), index = "date", by = "geo") |>
      recipes::prep()
  )
})

test_that("grouping keys cannot collide", {
  skip_if_not_installed("recipes")
  d <- data.frame(
    date = c(seq(as.Date("2024-01-01"), by = 7, length.out = 4),
             seq(as.Date("2024-02-05"), by = 7, length.out = 4)),
    a = c(rep("x\ry", 4), rep("x", 4)),
    b = c(rep("z", 4), rep("y\rz", 4)),
    tv = c(100, 0, 0, 0, 100, 0, 0, 0), y = rnorm(8)
  )
  rec <- recipes::recipe(y ~ ., d) |>
    step_adstock(tv, index = "date", by = c("a", "b"), decay = 0.5) |>
    recipes::prep()
  expect_length(rec$steps[[1]]$states, 2L)
  out <- recipes::bake(rec, new_data = NULL)
  expect_equal(out$tv[1:4], out$tv[5:8])
})

test_that("step_saturation validates and tunes only what its curve accepts", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("dials")
  data(mm_weekly, package = "mediamix")
  rec <- recipes::recipe(revenue ~ ., data = mm_weekly)
  expect_error(step_saturation(rec, tv, type = "power", shape = 1.5), "shape")
  expect_error(step_saturation(rec, tv, half_max = 5), "half_max")
  expect_error(step_saturation(rec, tv, half_max = 0), "half_max")

  pw <- step_saturation(rec, tv, type = "power", shape = 0.6)
  tn <- generics::tunable(pw$steps[[1]])
  expect_equal(tn$name, "shape")
  expect_equal(tn$call_info[[1]]$range, c(0.1, 1))
  expect_equal(nrow(generics::tunable(step_saturation(rec, tv,
                                                      half_max = 0.4)$steps[[1]])), 2L)
})

test_that("step_saturation warns rather than silently clamping negatives", {
  skip_if_not_installed("recipes")
  data(mm_weekly, package = "mediamix")
  expect_warning(
    recipes::recipe(revenue ~ ., data = mm_weekly) |>
      recipes::step_normalize(tv) |>
      step_adstock(tv, index = "date", by = "geo", decay = 0.7) |>
      step_saturation(tv, half_max = 0.4) |>
      recipes::prep() |>
      recipes::bake(new_data = NULL),
    "clamped to zero"
  )
})

test_that("tidy() reports legible group labels and honest state", {
  skip_if_not_installed("recipes")
  data(mm_weekly, package = "mediamix")
  tr <- mm_weekly[mm_weekly$date < as.Date("2025-07-01"), ]
  tr$size <- ifelse(tr$geo == "north", "big", "small")
  rec <- recipes::recipe(revenue ~ ., data = tr) |>
    step_adstock(tv, index = "date", by = c("geo", "size"), decay = 0.7) |>
    recipes::prep()
  td <- generics::tidy(rec, number = 1)
  expect_false(any(grepl("[\001-\037]", td$group)))
  expect_true(all(grepl(" / ", td$group)))
  expect_true(all(td$state_length == 1L))

  fir <- recipes::recipe(revenue ~ ., data = tr) |>
    step_adstock(tv, index = "date", by = "geo", decay = 0.7, max_lag = 4) |>
    recipes::prep()
  expect_true(all(generics::tidy(fir, number = 1)$state_length == 3L))
})

test_that("the tuning recipe from vignette('tidymodels') preps cleanly", {
  # The vignette's joint-tuning chunk only runs where the full tidymodels stack
  # is installed, so this checks the recipe it builds -- with tune() replaced
  # by constants -- prepares and yields only numeric predictors. A leftover
  # character or factor column would reach glmnet's matrix interface and abort
  # mid-resample.
  skip_if_not_installed("recipes")
  data(mm_weekly, package = "mediamix")
  north <- mm_weekly[mm_weekly$geo == "north", ]
  north <- north[order(north$date), ]
  channels <- c("tv", "video", "search", "social", "display")

  rec <- recipes::recipe(revenue ~ ., data = north) |>
    recipes::update_role(date, new_role = "index") |>
    recipes::step_rm(geo) |>
    step_adstock(tv, video, search, social, display, index = "date",
                 decay = 0.6) |>
    step_saturation(tv, video, search, social, display, half_max = 0.4,
                    shape = 1.2) |>
    recipes::step_normalize(recipes::all_numeric_predictors())

  prepped <- recipes::prep(rec)
  expect_s3_class(prepped, "recipe")

  info <- summary(prepped)
  predictors <- info$variable[info$role == "predictor"]
  juiced <- recipes::juice(prepped)
  expect_false(any(vapply(juiced[predictors], is.factor, logical(1))))
  expect_false(any(vapply(juiced[predictors], is.character, logical(1))))
  expect_true(all(vapply(juiced[predictors], is.numeric, logical(1))))
  expect_equal(nrow(juiced), nrow(north))
})
