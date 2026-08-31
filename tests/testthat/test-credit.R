# Tests for the credit_*() family (R/credit.R).

.mm_test_paths <- function() {
  data(mm_events, envir = environment())
  build_paths(mm_events, id = "customer_id", channel = "channel",
              timestamp = "timestamp", conversion = "conversion",
              value = "value")
}

.mm_all_rules <- function(paths) {
  list(
    linear = credit_linear(paths),
    first = credit_first(paths),
    last = credit_last(paths),
    position = credit_position(paths),
    time_decay = credit_time_decay(paths),
    custom = credit_custom(paths, function(rank, n, recency) rep(1, length(rank)))
  )
}

# ---- invariant 1 ------------------------------------------------------------

test_that("credit sums to 1 per converting journey and is exactly 0 for non-converters, for every rule", {
  paths <- .mm_test_paths()
  rules <- .mm_all_rules(paths)

  for (nm in names(rules)) {
    scored <- rules[[nm]]
    conv <- scored[scored$converted, ]
    per_journey <- as.numeric(tapply(conv$credit, conv$path_id, sum))
    expect_equal(per_journey, rep(1, length(per_journey)), tolerance = 1e-8,
                 info = nm)

    non_conv <- scored[!scored$converted, ]
    expect_true(all(non_conv$credit == 0), info = nm)
  }
})

# ---- invariant 2 ------------------------------------------------------------

test_that("total attributed conversions equals attributable conversions, for every rule", {
  paths <- .mm_test_paths()
  expected <- path_summary(paths)$converting_journeys
  rules <- .mm_all_rules(paths)

  for (nm in names(rules)) {
    expect_equal(sum(rules[[nm]]$credit), expected, tolerance = 1e-6, info = nm)
  }
})

# ---- invariant 3 (credit_* half) --------------------------------------------

test_that("row count and order are preserved by every credit_* function", {
  paths <- .mm_test_paths()
  rules <- .mm_all_rules(paths)

  for (nm in names(rules)) {
    scored <- rules[[nm]]
    expect_identical(nrow(scored), nrow(paths), info = nm)
    expect_identical(scored$path_id, paths$path_id, info = nm)
    expect_identical(scored$touch_rank, paths$touch_rank, info = nm)
  }
})

# ---- rule-specific exact behaviour, hand-made ------------------------------

test_that("credit_first, credit_last, credit_linear and credit_position assign weights as documented", {
  ev <- data.frame(
    cust = "a",
    ch = c("A", "B", "C"),
    ts = as.POSIXct("2024-01-01", tz = "UTC") + (0:2) * 86400,
    conv = c(0, 0, 1)
  )
  p <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                    conversion = "conv")

  lin <- credit_linear(p)
  expect_equal(lin$credit, rep(1 / 3, 3))

  first <- credit_first(p)
  expect_equal(first$credit, c(1, 0, 0))

  last <- credit_last(p)
  expect_equal(last$credit, c(0, 0, 1))

  pos <- credit_position(p, first_weight = 0.4, last_weight = 0.4)
  expect_equal(pos$credit, c(0.4, 0.2, 0.4))

  # two-touch journey: the "middle" share is split between the two touches
  ev2 <- data.frame(cust = "b", ch = c("X", "Y"),
                     ts = as.POSIXct("2024-01-01", tz = "UTC") + c(0, 1),
                     conv = c(0, 1))
  p2 <- build_paths(ev2, id = "cust", channel = "ch", timestamp = "ts",
                     conversion = "conv")
  pos2 <- credit_position(p2, first_weight = 0.4, last_weight = 0.4)
  expect_equal(pos2$credit, c(0.5, 0.5))

  # single-touch journey gets all the credit
  ev3 <- data.frame(cust = "c", ch = "Z",
                     ts = as.POSIXct("2024-01-01", tz = "UTC"), conv = 1)
  p3 <- build_paths(ev3, id = "cust", channel = "ch", timestamp = "ts",
                     conversion = "conv")
  expect_equal(credit_position(p3)$credit, 1)

  expect_error(credit_position(p, first_weight = 0.7, last_weight = 0.6),
               "sum to at most 1")
})

# ---- credit_time_decay: recency and the adstock mirror ----------------------

test_that("credit_time_decay gives strictly more credit to more recent touches", {
  ev <- data.frame(
    cust = "a",
    ch = c("A", "B", "C", "D"),
    ts = as.POSIXct("2024-01-01", tz = "UTC") + (0:3) * 86400,
    conv = c(0, 0, 0, 1)
  )
  p <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                    conversion = "conv")
  td <- credit_time_decay(p, decay = 0.6, period = 1)
  ord <- order(p$touch_rank)
  # touch_rank increases -> recency decreases -> credit strictly increases
  expect_true(all(diff(td$credit[ord]) > 0))
})

test_that("credit_time_decay is adstock_weights() run backwards, for evenly spaced touches", {
  ev <- data.frame(
    cust = "a",
    ch = c("A", "B", "C", "D"),
    ts = as.POSIXct("2024-01-01", tz = "UTC") + (0:3) * 86400,
    conv = c(0, 0, 0, 1)
  )
  p <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                    conversion = "conv")
  td <- credit_time_decay(p, decay = 0.6, period = 1)
  w <- adstock_weights(max_lag = 4, decay = 0.6)

  # touch_rank 1 (oldest) mirrors the most distant lag; touch_rank n (most
  # recent, at conversion) mirrors lag 0. Reversing the credit vector (by
  # touch_rank) lines it up with the adstock weight vector (by lag).
  ord <- order(p$touch_rank)
  reversed <- rev(td$credit[ord])
  expect_equal(reversed, w, tolerance = 1e-8)
  # they are not merely proportional here (both already sum to 1): equal
  expect_equal(sum(w), 1, tolerance = 1e-8)
})

# ---- credit_custom validation -----------------------------------------------

test_that("credit_custom errors when fn returns the wrong length, or negative/NA weights", {
  ev <- data.frame(
    cust = "a",
    ch = c("A", "B", "C"),
    ts = as.POSIXct("2024-01-01", tz = "UTC") + (0:2) * 86400,
    conv = c(0, 0, 1)
  )
  p <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                    conversion = "conv")

  expect_error(
    credit_custom(p, function(rank, n, recency) rep(1, length(rank) - 1)),
    "as long as the journey"
  )
  expect_error(
    credit_custom(p, function(rank, n, recency) c(-1, 1, 1)),
    "non-negative"
  )
  expect_error(
    credit_custom(p, function(rank, n, recency) c(NA_real_, 1, 1)),
    "non-negative"
  )
  expect_error(
    credit_custom(p, "not a function"),
    "must be a function"
  )

  # a valid custom rule works and still sums to 1
  ok <- credit_custom(p, function(rank, n, recency) as.numeric(recency <= 1))
  expect_equal(sum(ok$credit), 1)
})
