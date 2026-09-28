# Regression tests for defects found in the adversarial review.

# ---- journey subsets are valid journey tables --------------------------------

test_that("a row subset that removes part of a journey re-derives ranks", {
  ev <- data.frame(
    id = "a", ch = c("display", "social", "email", "search"),
    ts = as.POSIXct("2026-01-01", tz = "UTC") + (0:3) * 86400,
    conv = c(0, 0, 0, 1)
  )
  p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                   conversion = "conv", collapse_repeats = FALSE)
  sub <- p[p$channel != "display", ]
  expect_s3_class(sub, "mm_paths")
  expect_identical(sub$touch_rank, 1:3)
  expect_identical(unique(sub$touch_n), 3L)
  expect_equal(credit_first(sub)$credit, c(1, 0, 0))
  expect_equal(sum(credit_position(sub)$credit), 1)
})

test_that("every rule credits one conversion per journey after a channel filter", {
  data(mm_events, envir = environment())
  paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                       timestamp = "timestamp", conversion = "conversion")
  sub <- paths[paths$channel != "display", ]
  n_conv <- length(unique(sub$path_id[sub$converted]))
  res <- attribute(sub, rules = c("linear", "first", "last", "position",
                                  "time_decay"))
  totals <- tapply(res$conversions, res$rule, sum)
  expect_equal(as.numeric(totals), rep(n_conv, 5))
})

test_that("whole-journey subsets keep their original ranks", {
  data(mm_events, envir = environment())
  paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                       timestamp = "timestamp", conversion = "conversion")
  conv <- paths[paths$converted, ]
  keep <- paths$converted
  expect_identical(conv$touch_rank, paths$touch_rank[keep])
  expect_identical(conv$touch_n, paths$touch_n[keep])
})

# ---- multiple conversions per journey are reported ---------------------------

test_that("build_paths() reports journeys holding several conversions", {
  ev <- data.frame(
    id = "a", ch = c("display", "search", "email", "search"),
    ts = as.POSIXct("2026-01-01", tz = "UTC") + (0:3) * 86400,
    conv = c(0, 1, 0, 1)
  )
  expect_message(
    build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                conversion = "conv", split_on = "none"),
    "more than one conversion"
  )
  expect_no_message(
    build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                conversion = "conv", split_on = "conversion")
  )
})

# ---- tune_carryover(): the metric means what it is called --------------------

test_that("pooled aggregation computes the metric over all forecasts", {
  set.seed(42)
  n <- 120
  sp <- pmax(0, rnorm(n, 500, 250))
  kpi <- 200 + 0.5 * adstock_geometric(sp, decay = 0.7) + rnorm(n, sd = 8)
  z <- adstock_geometric(sp, 0.7)
  e <- vapply(60:119, function(k) {
    m <- fit_ols(z[1:k], kpi[1:k])
    kpi[k + 1] - predict(m, z[k + 1])
  }, numeric(1))

  pooled <- tune_carryover(sp, kpi, decays = 0.7)
  mean_agg <- tune_carryover(sp, kpi, decays = 0.7, aggregate = "mean")
  expect_equal(pooled$best$metric, sqrt(mean(e^2)))
  # With one-period assessment sets, the mean of per-split RMSEs is an MAE.
  expect_equal(mean_agg$best$metric, mean(abs(e)))
  expect_equal(pooled$best$std_err, stats::sd(abs(e)) / sqrt(length(e)))
})

test_that("the one-standard-error rule never picks more carryover than best", {
  set.seed(9)
  n <- 104
  sp <- pmax(0, rnorm(n, 500, 250))
  kpi <- 200 + 0.5 * adstock_geometric(sp, decay = 0.5) + rnorm(n, sd = 40)
  tuned <- suppressWarnings(tune_carryover(sp, kpi,
                                           decays = seq(0.05, 0.95, 0.05)))
  expect_lte(tuned$best_1se$decay, tuned$best$decay)
  expect_s3_class(tuned$best_1se, "data.frame")
  expect_no_error(suppressMessages(capture.output(print(tuned))))
})

# ---- marginal_roi(): counterfactual marginal return --------------------------

test_that("a linear, normalised transform has marginal = average = coefficient in the long run", {
  set.seed(1)
  x <- pmax(0, rnorm(60, 100, 30))
  r <- marginal_roi(x, coefficient = 2, adstock = list(decay = 0.6),
                    extend = effective_window(0.6, 0.999999))
  expect_equal(r$mroi, 2, tolerance = 1e-5)
  expect_equal(r$roi, 2, tolerance = 1e-5)
})

test_that("without extension, late carryover is lost and matches contributions()", {
  x <- c(100, 0, 0, 0)
  r <- marginal_roi(x, coefficient = 1, adstock = list(decay = 0.5))
  expect_equal(r$contribution, sum(adstock_geometric(x, 0.5)))
  expect_equal(r$roi, sum(adstock_geometric(x, 0.5)) / 100)
})

test_that("marginal return counts carryover, not just the first kernel weight", {
  set.seed(3)
  x <- pmax(0, rnorm(200, 1000, 300))
  r <- marginal_roi(x, coefficient = 1000, adstock = list(decay = 0.85),
                    saturation = list(half_max = 1000))
  z <- adstock_geometric(x, 0.85)
  first_weight_only <- mroi(mean(z), 1000, half_max = 1000) * (1 - 0.85)
  expect_gt(r$mroi, 4 * first_weight_only)
  # Concave curve: the next unit returns less than the average unit
  expect_lt(r$mroi, r$roi)
})

test_that("rows restricts the lift to a window and by pads each group", {
  x <- c(10, 10, 10, 10, 20, 20, 20, 20)
  g <- rep(c("a", "b"), each = 4)
  full <- marginal_roi(x, 1, adstock = list(decay = 0.5), by = g)
  a_only <- marginal_roi(x, 1, adstock = list(decay = 0.5), by = g,
                         rows = g == "a")
  expect_equal(full$mroi, a_only$mroi)          # linear: same slope
  ext <- marginal_roi(x, 1, adstock = list(decay = 0.5), by = g, extend = 50)
  expect_equal(ext$roi, 1, tolerance = 1e-9)
  expect_error(marginal_roi(c(0, 0), 1), "no spend")
  expect_error(marginal_roi(x, 1, rows = 20), "index periods")
})

# ---- markov_removal() ---------------------------------------------------------

test_that("markov_removal() matches a hand-computed absorbing chain", {
  ev <- data.frame(id = c("j1", "j1", "j2", "j3"), ch = c("a", "b", "a", "b"),
                   ts = c(1, 2, 1, 1), conv = c(0, 1, 0, 1))
  p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                   conversion = "conv")
  mk <- markov_removal(p)
  expect_equal(attr(mk, "p_conversion"), 2 / 3)
  expect_equal(mk$removal_effect[mk$channel == "a"], 0.5)
  expect_equal(mk$removal_effect[mk$channel == "b"], 1)
  expect_equal(mk$conversions[mk$channel == "a"], 2 / 3)
  expect_equal(sum(mk$conversions), 2)
})

test_that("markov_removal() handles loops and warns without null paths", {
  ev <- data.frame(id = c("j1", "j1", "j1", "j2", "j2"),
                   ch = c("a", "b", "a", "b", "b"),
                   ts = c(1, 2, 3, 1, 2), conv = c(0, 0, 1, 0, 0))
  p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                   conversion = "conv", collapse_repeats = FALSE)
  mk <- markov_removal(p)
  expect_true(all(is.finite(mk$removal_effect)))
  expect_equal(sum(mk$conversions), 1)

  only_conv <- build_paths(ev[ev$id == "j1", ], id = "id", channel = "ch",
                           timestamp = "ts", conversion = "conv")
  expect_warning(markov_removal(only_conv), "no non-converting")
})

test_that("the markov rule sits alongside the heuristics in attribute()", {
  data(mm_events, envir = environment())
  paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                       timestamp = "timestamp", conversion = "conversion",
                       value = "value")
  res <- attribute(paths)
  expect_true("markov" %in% res$rule)
  mk <- res[res$rule == "markov", ]
  expect_equal(sum(mk$conversions), path_summary(paths)$converting_journeys)
  expect_equal(sum(mk$share), 1)
  expect_equal(sum(mk$value),
               sum(res$value[res$rule == "linear"]), tolerance = 1e-8)
})

# ---- delayed adstock (Jin et al. 2017) ---------------------------------------

test_that("the delayed kernel peaks at `peak` and matches its formula", {
  w <- adstock_weights_delayed(10, decay = 0.6, peak = 3, normalise = FALSE)
  expect_equal(w, 0.6^((0:9 - 3)^2))
  expect_identical(which.max(w), 4L)
  expect_equal(sum(adstock_weights_delayed(10, 0.6, 3)), 1)
  expect_error(adstock_weights_delayed(5, 0.6, peak = 5), "peak")
})

test_that("adstock_delayed() is a causal filter that chains across chunks", {
  x <- c(100, 0, 0, 20, 50, 0, 0, 0)
  w <- adstock_weights_delayed(4, decay = 0.5, peak = 1)
  expect_equal(adstock_delayed(x, 0.5, peak = 1, max_lag = 4),
               adstock_filter(x, w))
  whole <- adstock_delayed(x, 0.5, peak = 1, max_lag = 4)
  s <- adstock_state(x[1:5], decay = 0.5, max_lag = 4)
  expect_equal(c(whole[1:5],
                 adstock_delayed(x[6:8], 0.5, 1, max_lag = 4, state = s)),
               whole)
  expect_equal(
    media_transform(x, adstock = list(kernel = "delayed", decay = 0.5,
                                      peak = 1, max_lag = 4)),
    whole)
  expect_error(adstock_delayed(x, 0.5), "required")
})

test_that("the paired one-SE rule stays close to a clearly identified truth", {
  for (truth in c(0.3, 0.5, 0.7)) {
    set.seed(2)
    n <- 150
    spend <- pmax(0, rnorm(n, 500, 250))
    kpi <- 200 + 0.5 * adstock_geometric(spend, decay = truth) + rnorm(n, sd = 8)
    tuned <- tune_carryover(spend, kpi, decays = seq(0.02, 0.98, by = 0.02))
    expect_lte(truth - tuned$best_1se$decay, 0.1)
    expect_lte(tuned$best_1se$decay, tuned$best$decay)
  }
})

# ---- diagnose_media(): CPM outliers are judged within each series -----------

test_that("a geography that simply pays more is not flagged as join errors", {
  set.seed(4)
  d <- data.frame(geo = rep(c("cheap", "dear"), each = 30),
                  imp = runif(60, 900, 1100))
  d$spend <- d$imp / 1000 * ifelse(d$geo == "cheap", 5, 20) *
    runif(60, 0.95, 1.05)
  d$spend[7] <- d$spend[7] * 10          # one genuine join error
  pooled <- diagnose_media(d, media = "spend", spend = "spend",
                           impressions = "imp")
  grouped <- diagnose_media(d, media = "spend", spend = "spend",
                            impressions = "imp", by = "geo")
  expect_identical(grouped$cpm$row[grouped$cpm$outlier], 7L)
  expect_true("group" %in% names(grouped$cpm))
  expect_identical(nrow(grouped$cpm), 60L)
  expect_identical(pooled$cpm$row, 1:60)
})
