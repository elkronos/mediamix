# Tests for build_paths() and the mm_paths class (R/paths.R).
#
# Seven documented prep concerns, plus subsetting, plus the two accounting
# invariants that make path_summary() trustworthy.

# ---- 1. journey splitting -----------------------------------------------

test_that("split_on = 'conversion' produces more journeys than 'none', and a repeat converter yields two journeys", {
  data(mm_events)
  p_conv <- build_paths(mm_events, id = "customer_id", channel = "channel",
                         timestamp = "timestamp", conversion = "conversion",
                         split_on = "conversion")
  p_none <- build_paths(mm_events, id = "customer_id", channel = "channel",
                         timestamp = "timestamp", conversion = "conversion",
                         split_on = "none")
  expect_gt(length(unique(p_conv$path_id)), length(unique(p_none$path_id)))
  # one journey per customer under "none"
  expect_identical(length(unique(p_none$path_id)),
                    length(unique(mm_events$customer_id)))

  # Hand-made: a customer who converts twice is two journeys, not one
  ev <- data.frame(
    cust = c("a", "a", "a", "a"),
    ch = c("X", "Y", "Z", "W"),
    ts = as.POSIXct("2024-01-01", tz = "UTC") + (0:3) * 86400,
    conv = c(0, 1, 0, 1)
  )
  p <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                    conversion = "conv", split_on = "conversion")
  expect_identical(length(unique(p$path_id)), 2L)
  expect_true(all(p$converted))
})

# ---- 2. gap splitting -----------------------------------------------------

test_that("split_on = 'gap' splits on inactivity, and omitting gap errors", {
  ev <- data.frame(
    cust = c("a", "a", "a", "a"),
    ch = c("X", "Y", "Z", "W"),
    ts = as.POSIXct("2024-01-01", tz = "UTC") + c(0, 1, 20, 21) * 86400,
    conv = c(0, 0, 0, 1)
  )
  p_gap <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                        conversion = "conv", split_on = c("conversion", "gap"),
                        gap = 14)
  expect_identical(length(unique(p_gap$path_id)), 2L)

  p_nogap <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                          conversion = "conv", split_on = "conversion")
  expect_identical(length(unique(p_nogap$path_id)), 1L)

  expect_error(
    build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                conversion = "conv", split_on = c("conversion", "gap")),
    "gap.*required"
  )
})

# ---- 3. lookback -----------------------------------------------------------

test_that("lookback drops old touches and a small lookback shrinks mean journey length", {
  ev <- data.frame(
    cust = c("a", "a", "a"),
    ch = c("X", "Y", "Z"),
    ts = as.POSIXct("2024-01-01", tz = "UTC") + c(0, 10, 20) * 86400,
    conv = c(0, 0, 1)
  )
  p_full <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                         conversion = "conv")
  expect_identical(unique(p_full$touch_n), 3L)

  p_lb <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                       conversion = "conv", lookback = 5)
  expect_identical(unique(p_lb$touch_n), 1L)
  expect_identical(p_lb$channel, "Z")

  # At scale: a very small lookback shrinks the mean journey length
  data(mm_events)
  full <- build_paths(mm_events, id = "customer_id", channel = "channel",
                       timestamp = "timestamp", conversion = "conversion")
  short <- build_paths(mm_events, id = "customer_id", channel = "channel",
                        timestamp = "timestamp", conversion = "conversion",
                        lookback = 1)
  expect_lt(path_summary(short)$mean_length, path_summary(full)$mean_length)
})

# ---- 4. keep_null_paths -----------------------------------------------------

test_that("keep_null_paths = FALSE leaves only converting journeys; TRUE (default) retains non-converters", {
  ev <- data.frame(
    cust = c("a", "b"),
    ch = c("X", "Y"),
    ts = as.POSIXct("2024-01-01", tz = "UTC"),
    conv = c(1, 0)
  )
  p_keep <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                         conversion = "conv", keep_null_paths = TRUE)
  expect_identical(length(unique(p_keep$path_id)), 2L)
  expect_true(any(!p_keep$converted))

  p_drop <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                         conversion = "conv", keep_null_paths = FALSE)
  expect_identical(length(unique(p_drop$path_id)), 1L)
  expect_true(all(p_drop$converted))
})

# ---- 5. direct traffic handling --------------------------------------------

test_that("direct = keep/drop/label behave as documented", {
  ev <- data.frame(
    cust = c("a", "a"),
    ch = c("email", NA_character_),
    ts = as.POSIXct("2024-01-01", tz = "UTC") + c(0, 86400),
    conv = c(0, 1)
  )
  p_keep <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                         conversion = "conv", direct = "keep")
  expect_identical(p_keep$channel, c("email", "(missing)"))

  p_label <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                          conversion = "conv", direct = "label")
  expect_identical(p_label$channel, c("email", "(direct)"))

  p_drop <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                         conversion = "conv", direct = "drop")
  expect_identical(p_drop$channel, "email")
  expect_true(p_drop$converted[1])
})

test_that("direct = 'drop' does not erase the conversion it recorded", {
  # The conversion event itself is on a direct channel. Its own journey has no
  # other touch, so dropping direct traffic empties that journey completely --
  # but a second, unrelated journey survives, so build_paths does not hit the
  # fully-degenerate early return and the loss is genuinely accounted for.
  ev <- data.frame(
    cust = c("a", "b"),
    ch = c("direct", "email"),
    ts = as.POSIXct("2024-01-01", tz = "UTC"),
    conv = c(1, 0),
    val = c(42, NA)
  )
  p <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                    conversion = "conv", value = "val", direct = "drop")

  # customer a's touch is gone entirely
  expect_false("a#1" %in% p$path_id)
  expect_identical(nrow(p), 1L)

  smry <- path_summary(p)
  expect_identical(smry$conversions_observed, 1L)
  expect_identical(smry$conversions_unattributable, 1L)
  expect_identical(smry$converting_journeys, 0L)
})

# ---- 6. collapse_repeats -----------------------------------------------------

test_that("collapse_repeats collapses A,A,A to a single A, keeping the first of the run", {
  ev <- data.frame(
    cust = "a",
    ch = c("A", "A", "A", "B"),
    ts = as.POSIXct("2024-01-01", tz = "UTC") + (0:3) * 86400,
    conv = c(0, 0, 0, 1)
  )
  p_collapse <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                             conversion = "conv", collapse_repeats = TRUE)
  expect_identical(p_collapse$channel, c("A", "B"))
  # keeps the FIRST of the run, i.e. the earliest timestamp
  expect_identical(p_collapse$timestamp[1], as.POSIXct("2024-01-01", tz = "UTC"))

  p_nocollapse <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                               conversion = "conv", collapse_repeats = FALSE)
  expect_identical(p_nocollapse$channel, c("A", "A", "A", "B"))
})

# ---- 7. deterministic tie-breaking -----------------------------------------

test_that("shuffling the input row order leaves the result unchanged after sorting", {
  # Customer "a" has three tied touches; other rows exist purely so a shuffle
  # of the whole table is meaningful. The tie-break is on original row order
  # *within a customer*, so a shuffle that moves other customers' rows around
  # a's own rows (without changing a's rows' relative order) must reproduce
  # exactly the same journeys.
  ev <- data.frame(
    cust = c("a", "a", "a", "b", "b", "c"),
    ch   = c("X", "Y", "Z", "P", "Q", "R"),
    ts = c(rep(as.POSIXct("2024-01-01", tz = "UTC"), 3),
           as.POSIXct("2024-01-02", tz = "UTC"),
           as.POSIXct("2024-01-03", tz = "UTC"),
           as.POSIXct("2024-01-01", tz = "UTC")),
    conv = c(0, 0, 1, 0, 1, 0)
  )
  perm <- c(6, 1, 4, 2, 5, 3)
  shuffled <- ev[perm, ]
  rownames(shuffled) <- NULL

  p1 <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                     conversion = "conv")
  p2 <- build_paths(shuffled, id = "cust", channel = "ch", timestamp = "ts",
                     conversion = "conv")

  d1 <- as.data.frame(p1)
  d1 <- d1[order(d1$path_id, d1$touch_rank), ]
  rownames(d1) <- NULL
  d2 <- as.data.frame(p2)
  d2 <- d2[order(d2$path_id, d2$touch_rank), ]
  rownames(d2) <- NULL

  expect_identical(d1, d2)
  # and the tie for customer "a" really was broken by original row order
  a_rows <- d1[d1$id == "a", ]
  expect_identical(a_rows$channel, c("X", "Y", "Z"))
})

# ---- 8. empty input ---------------------------------------------------------

test_that("an empty input data frame returns a zero-row mm_paths without error", {
  ev_empty <- data.frame(cust = character(0), ch = character(0),
                          ts = as.POSIXct(character(0)), conv = integer(0))
  p <- expect_no_error(
    build_paths(ev_empty, id = "cust", channel = "ch", timestamp = "ts",
                conversion = "conv")
  )
  expect_s3_class(p, "mm_paths")
  expect_identical(nrow(p), 0L)
})

# ---- 9. missing / misnamed column -------------------------------------------

test_that("a missing or misnamed column errors with a message naming the column", {
  ev <- data.frame(cust = "a", ch = "X",
                    ts = as.POSIXct("2024-01-01", tz = "UTC"), conv = 1)
  expect_error(
    build_paths(ev, id = "cust", channel = "ch", timestamp = "not_a_column",
                conversion = "conv"),
    "not_a_column"
  )
  expect_error(
    build_paths(ev, id = "cust", channel = "also_missing", timestamp = "ts",
                conversion = "conv"),
    "also_missing"
  )
})

# ---- [.mm_paths --------------------------------------------------------------

test_that("[.mm_paths keeps the class on a row subset and drops it when structural columns go", {
  data(mm_events)
  paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                        timestamp = "timestamp", conversion = "conversion")

  row_sub <- paths[paths$converted, ]
  expect_s3_class(row_sub, "mm_paths")
  expect_true(all(row_sub$converted))

  col_sub <- paths[, c("channel", "timestamp")]
  expect_false(inherits(col_sub, "mm_paths"))
  expect_identical(class(col_sub), "data.frame")

  # A subset keeping every structural column stays mm_paths.
  col_keep <- paths[, mediamix:::.mm_path_cols()]
  expect_s3_class(col_keep, "mm_paths")
  expect_no_error(credit_time_decay(col_keep))

  # Dropping a column the credit rules read must drop the class, not leave an
  # object that answers credit_time_decay() with a silently linear split.
  col_short <- paths[, c("path_id", "channel", "touch_rank", "touch_n",
                         "converted")]
  expect_false(inherits(col_short, "mm_paths"))
})

test_that("row subsets drop the construction counts they no longer describe", {
  data(mm_events)
  paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                        timestamp = "timestamp", conversion = "conversion")
  expect_false(is.na(path_summary(paths)$events_in))

  sub <- paths[paths$converted, ]
  s <- path_summary(sub)
  expect_true(is.na(s$events_in))
  expect_equal(s$journeys, length(unique(sub$path_id)))
  expect_equal(s$events_out, nrow(sub))
})

# ---- print.mm_paths ---------------------------------------------------------

test_that("print.mm_paths runs without error on populated and empty paths", {
  data(mm_events)
  paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                        timestamp = "timestamp", conversion = "conversion")
  expect_no_error(capture.output(print(paths)))

  empty <- build_paths(mm_events[0, ], id = "customer_id", channel = "channel",
                        timestamp = "timestamp", conversion = "conversion")
  expect_no_error(capture.output(print(empty)))
})
