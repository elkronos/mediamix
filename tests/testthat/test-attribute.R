# Tests for attribute() and attribution_spread() (R/attribute.R).

test_that("attribute() returns one row per rule x channel and share sums to 1 within each rule", {
  data(mm_events)
  paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                        timestamp = "timestamp", conversion = "conversion",
                        value = "value")
  res <- attribute(paths)

  expect_s3_class(res, "data.frame")
  expect_identical(names(res)[1:4], c("rule", "channel", "conversions", "share"))

  rules <- c("linear", "first", "last", "position", "time_decay")
  n_channels <- length(unique(paths$channel))
  expect_identical(nrow(res), length(rules) * n_channels)

  shares <- tapply(res$share, res$rule, sum)
  expect_equal(as.numeric(shares), rep(1, length(shares)), tolerance = 1e-8)

  # conversions sum to converting_journeys under every rule (mirrors the
  # credit_*() invariant, one level up)
  conv_sums <- tapply(res$conversions, res$rule, sum)
  expect_equal(as.numeric(conv_sums),
               rep(path_summary(paths)$converting_journeys, length(conv_sums)),
               tolerance = 1e-6)

  # value column present because mm_events carries conversion values
  expect_true("value" %in% names(res))
  expect_true("touches" %in% names(res))
})

test_that("attribute() respects the rules argument and validates it", {
  ev <- data.frame(
    cust = c("a", "a", "b"),
    ch = c("X", "Y", "Z"),
    ts = as.POSIXct("2024-01-01", tz = "UTC") + c(0, 1, 0) * 86400,
    conv = c(0, 1, 1)
  )
  p <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                    conversion = "conv")

  res <- attribute(p, rules = c("first", "last"))
  expect_setequal(unique(res$rule), c("first", "last"))

  expect_error(attribute(p, rules = "not_a_rule"), "should be one of")
})

test_that("attribute() works on a hand-made journey table with a known answer", {
  ev <- data.frame(
    cust = c("a", "b"),
    ch = c("X", "Y"),
    ts = as.POSIXct("2024-01-01", tz = "UTC"),
    conv = c(1, 1)
  )
  p <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                    conversion = "conv")
  res <- attribute(p, rules = "linear")
  expect_setequal(res$channel, c("X", "Y"))
  expect_equal(res$conversions, c(1, 1))
  expect_equal(res$share, c(0.5, 0.5))
})

test_that("attribution_spread() summarises attribute() output correctly", {
  data(mm_events)
  paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                        timestamp = "timestamp", conversion = "conversion")
  res <- attribute(paths)
  sp <- attribution_spread(res)

  expect_s3_class(sp, "data.frame")
  expect_identical(sort(names(sp)),
                    sort(c("channel", "min_share", "max_share", "mean_share",
                           "spread", "first_last_ratio")))
  expect_equal(sp$spread, sp$max_share - sp$min_share, tolerance = 1e-10)
  expect_true(all(sp$min_share <= sp$mean_share & sp$mean_share <= sp$max_share))
  # ordered by descending spread
  expect_true(all(diff(sp$spread) <= 0))

  expect_error(attribution_spread(data.frame(x = 1)), "must be the data frame")
})
