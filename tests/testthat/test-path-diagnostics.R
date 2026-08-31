# Tests for R/path-diagnostics.R: path_summary(), path_lengths(),
# channel_positions(), assisted_conversions(), top_paths(), conversion_lag(),
# path_diagnostics().
#
# NOTE: assisted_conversions() (and therefore path_diagnostics(), which calls
# it) errors on a journey table whose channel column contains a literal ""
# value -- see the bug reported in the test-suite summary. mm_events, built
# with the default direct = "keep", contains such a channel. Scale tests below
# that touch assisted_conversions()/path_diagnostics() therefore build paths
# with direct = "label" to sidestep that bug; this is orthogonal to what is
# being tested here.

# ---- shapes and no-error-on-zero-rows, across every diagnostic -------------

test_that("every path diagnostic returns its documented shape and does not error on zero-row mm_paths", {
  empty <- build_paths(data.frame(cust = character(0), ch = character(0),
                                   ts = as.POSIXct(character(0)),
                                   conv = integer(0)),
                        id = "cust", channel = "ch", timestamp = "ts",
                        conversion = "conv")
  expect_identical(nrow(empty), 0L)

  smry <- expect_no_error(path_summary(empty))
  expect_identical(nrow(smry), 1L)
  expect_true(all(c("events_in", "events_out", "journeys", "converting_journeys",
                     "mean_length", "median_length", "max_length",
                     "direct_events", "conversions_observed",
                     "conversions_unattributable") %in% names(smry)))

  lens <- expect_no_error(path_lengths(empty))
  expect_identical(names(lens), c("length", "journeys", "converting",
                                   "conversion_rate"))
  expect_identical(nrow(lens), 0L)

  pos <- expect_no_error(channel_positions(empty))
  expect_identical(names(pos), c("channel", "only", "first", "middle", "last",
                                  "touches", "first_last_ratio"))
  expect_identical(nrow(pos), 0L)

  ac <- expect_no_error(assisted_conversions(empty))
  expect_true(is.matrix(ac))
  expect_identical(dim(ac), c(0L, 0L))

  tp <- expect_no_error(top_paths(empty))
  expect_identical(names(tp), c("path", "journeys", "conversions",
                                 "conversion_rate"))
  expect_identical(nrow(tp), 0L)

  cl <- expect_no_error(conversion_lag(empty))
  expect_identical(names(cl), c("quantile", "lag"))
  expect_true(all(is.na(cl$lag)))

  diag <- expect_no_error(path_diagnostics(empty))
  expect_identical(names(diag), c("summary", "lengths", "positions", "assists",
                                   "top_paths", "conversion_lag"))
})

# ---- path_summary() accounting ---------------------------------------------

test_that("path_summary() reports construction counts consistent with the journey table", {
  data(mm_events)
  paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                        timestamp = "timestamp", conversion = "conversion",
                        value = "value")
  smry <- path_summary(paths)

  expect_identical(smry$journeys, length(unique(paths$path_id)))
  expect_identical(smry$converting_journeys,
                    length(unique(paths$path_id[paths$converted])))
  expect_identical(smry$events_out, nrow(paths))
  expect_equal(smry$mean_length,
               mean(vapply(split(paths$touch_n, paths$path_id),
                           function(z) z[1L], numeric(1))))
})

# ---- path_lengths() ----------------------------------------------------------

test_that("path_lengths() bins journeys by length correctly", {
  ev <- data.frame(
    cust = c("a", "b", "c"),
    ch = c("X", "Y", "Z"),
    ts = as.POSIXct("2024-01-01", tz = "UTC"),
    conv = c(1, 0, 1)
  )
  # two 1-touch journeys (a converts, b doesn't) plus a 2-touch journey (c, d)
  ev2 <- data.frame(
    cust = c("a", "b", "d", "d"),
    ch = c("X", "Y", "P", "Q"),
    ts = c(rep(as.POSIXct("2024-01-01", tz = "UTC"), 2),
           as.POSIXct("2024-01-01", tz = "UTC"),
           as.POSIXct("2024-01-02", tz = "UTC")),
    conv = c(1, 0, 0, 1)
  )
  p <- build_paths(ev2, id = "cust", channel = "ch", timestamp = "ts",
                    conversion = "conv")
  lens <- path_lengths(p)
  expect_identical(lens$length, c(1L, 2L))
  expect_identical(lens$journeys, c(2L, 1L))
  expect_identical(lens$converting, c(1L, 1L))
  expect_equal(lens$conversion_rate, c(0.5, 1))
})

# ---- channel_positions() -----------------------------------------------------

test_that("channel_positions() classifies only/first/middle/last correctly", {
  # customer "a" is a single-touch (only) journey; "b" is a 3-touch journey
  # exercising first/middle/last
  ev3 <- data.frame(
    cust = c("a", "b", "b", "b"),
    ch = c("solo", "start", "mid", "end"),
    ts = c(as.POSIXct("2024-01-01", tz = "UTC"),
           as.POSIXct("2024-01-01", tz = "UTC") + 0:2 * 86400),
    conv = c(1, 0, 0, 1)
  )
  p <- build_paths(ev3, id = "cust", channel = "ch", timestamp = "ts",
                    conversion = "conv")
  pos <- channel_positions(p, converting_only = TRUE)
  rownames(pos) <- pos$channel

  expect_identical(pos["solo", "only"], 1L)
  expect_identical(pos["start", "first"], 1L)
  expect_identical(pos["mid", "middle"], 1L)
  expect_identical(pos["end", "last"], 1L)
  expect_identical(pos["start", "touches"], 1L)

  # first_last_ratio: NA when there is no "last" appearance
  expect_true(is.na(pos["mid", "first_last_ratio"]))
})

# ---- assisted_conversions() --------------------------------------------------

test_that("assisted_conversions() counts ordered channel pairs within converting journeys", {
  ev <- data.frame(
    cust = "a",
    ch = c("A", "B", "C"),
    ts = as.POSIXct("2024-01-01", tz = "UTC") + 0:2 * 86400,
    conv = c(0, 0, 1)
  )
  p <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                    conversion = "conv")
  m <- assisted_conversions(p)
  expect_identical(m["A", "B"], 1)
  expect_identical(m["A", "C"], 1)
  expect_identical(m["B", "C"], 1)
  expect_identical(m["C", "A"], 0)   # order matters: C never precedes A

  m_norm <- assisted_conversions(p, normalise = TRUE)
  expect_equal(m_norm["A", "B"], 1)  # A appears in 1 journey, assisted B once

  # At scale (avoiding the "" channel that build_paths(..., direct = "keep")
  # would otherwise carry through from mm_events)
  data(mm_events)
  paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                        timestamp = "timestamp", conversion = "conversion",
                        direct = "label")
  ac <- expect_no_error(assisted_conversions(paths))
  chans <- sort(unique(paths$channel))
  expect_identical(dim(ac), c(length(chans), length(chans)))
  expect_identical(rownames(ac), chans)
})

# ---- top_paths() --------------------------------------------------------------

test_that("top_paths() orders by descending frequency and reports conversion rate", {
  ev <- data.frame(
    cust = c("a", "b", "c"),
    ch = c("X", "X", "Y"),
    ts = as.POSIXct("2024-01-01", tz = "UTC"),
    conv = c(1, 0, 1)
  )
  p <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                    conversion = "conv")
  tp <- top_paths(p, n = 5)
  expect_identical(tp$path[1], "X")
  expect_identical(tp$journeys[tp$path == "X"], 2L)
  expect_identical(tp$conversions[tp$path == "X"], 1L)
  expect_equal(tp$conversion_rate[tp$path == "X"], 0.5)

  tp_conv_only <- top_paths(p, converting_only = TRUE)
  expect_true(all(tp_conv_only$conversion_rate == 1))
})

# ---- conversion_lag() ---------------------------------------------------------

test_that("conversion_lag() reports quantiles of time-to-conversion for converting journeys only", {
  ev <- data.frame(
    cust = c("a", "a", "b"),
    ch = c("X", "Y", "Z"),
    ts = c(as.POSIXct("2024-01-01", tz = "UTC"),
           as.POSIXct("2024-01-05", tz = "UTC"),
           as.POSIXct("2024-01-01", tz = "UTC")),
    conv = c(0, 1, 0)
  )
  p <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                    conversion = "conv")
  cl <- conversion_lag(p, probs = c(0.5, 1))
  expect_identical(cl$quantile, c(0.5, 1))
  # customer a's journey took 4 days start-to-conversion; b never converts
  expect_equal(cl$lag, c(4, 4))
  expect_identical(attr(cl, "units"), "days")
})

# ---- path_diagnostics() convenience wrapper --------------------------------

test_that("path_diagnostics() bundles every diagnostic and matches calling them directly", {
  data(mm_events)
  paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                        timestamp = "timestamp", conversion = "conversion",
                        direct = "label")
  diag <- path_diagnostics(paths, n = 5)

  expect_equal(diag$summary, path_summary(paths))
  expect_equal(diag$lengths, path_lengths(paths))
  expect_equal(diag$positions, channel_positions(paths))
  expect_equal(diag$assists, assisted_conversions(paths, normalise = TRUE))
  expect_equal(diag$top_paths, top_paths(paths, n = 5))
  expect_equal(diag$conversion_lag, conversion_lag(paths))
})
