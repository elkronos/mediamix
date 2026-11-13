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

# ---- character timestamps ------------------------------------------------------

test_that("ISO 8601 date-times with T, offsets and fractions parse completely", {
  ev <- data.frame(id = "a", ch = c("display", "search", "email"),
                   ts = c("2024-05-01T18:00:00Z", "2024-05-01T09:00:00Z",
                          "2024-05-01T12:00:00Z"),
                   conv = c(0, 0, 1))
  p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                   conversion = "conv")
  # display came after the sale, so it starts a second journey; first-touch
  # credit for the sale goes to search, not to display.
  expect_identical(p$channel[p$path_id == "a#1"], c("search", "email"))
  expect_identical(p$channel[p$path_id == "a#2"], "display")
  first <- credit_first(p)
  expect_identical(first$channel[first$credit > 0], "search")

  posix <- ev
  posix$ts <- as.POSIXct(c("2024-05-01 18:00:00", "2024-05-01 09:00:00",
                           "2024-05-01 12:00:00"), tz = "UTC")
  p_posix <- build_paths(posix, id = "id", channel = "ch", timestamp = "ts",
                         conversion = "conv")
  expect_identical(as.data.frame(p), as.data.frame(p_posix))
})

test_that("UTC offsets, fractional seconds and other ISO forms give the right instant", {
  # Seconds since 1970, so that values read in different zones compare.
  read <- function(x, tz = "UTC") {
    as.numeric(mediamix:::.mm_parse_time(x, "ts", tz = tz))
  }
  at9 <- as.numeric(as.POSIXct("2024-05-01 09:00:00", tz = "UTC"))
  expect_equal(read("2024-05-01T11:00:00+02:00"), at9)
  expect_equal(read("2024-05-01T11:00:00+0200"), at9)
  expect_equal(read("2024-05-01T11:00+02"), at9)
  expect_equal(read("2024-05-01T03:30:00-05:30"), at9)
  expect_equal(read("2024-05-01 09:00:00 UTC"), at9)
  expect_equal(read("2024-05-01 09:00"), at9)
  expect_equal(read("2024/05/01 09:00:00"), at9)
  expect_equal(read("2024-05-01T09:00:00.250Z") - at9, 0.25, tolerance = 1e-6)
  # Without an offset the value is read in `tz`; with one, `tz` is ignored.
  expect_equal(read("2024-05-01 11:00:00", tz = "Europe/Paris"), at9)
  expect_equal(read("2024-05-01T09:00:00Z", tz = "Europe/Paris"), at9)
  # A plain date is midnight in `tz`.
  expect_equal(read("2024-05-01", tz = "Europe/Paris"), at9 - 11 * 3600)
  # Missing and blank values come back as NA for build_paths() to report.
  expect_true(is.na(read(c("2024-05-01", NA, "  "))[2L]))
  expect_true(is.na(read(c("2024-05-01", NA, "  "))[3L]))
})

test_that("unparseable or mixed character timestamps abort instead of truncating", {
  B <- function(ts, ...) {
    ev <- data.frame(id = "a", ch = c("x", "y"), ts = ts, conv = c(0, 1))
    build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                conversion = "conv", ...)
  }
  expect_error(B(c("05/01/2024", "05/02/2024")), "could not be parsed")
  expect_error(B(c("05/01/2024", "05/02/2024")), "timestamp_format")
  expect_error(B(c("2024-05-01", "2024-05-01T09:00:00")), "mixes dates")
  expect_error(B(c("2024-02-30 09:00", "2024-03-01 09:00")), "not a valid")
  expect_error(B(c("2024-05-01 09:00", "yesterday")), "yesterday")
  expect_error(B(c("2024-05-01 09:00", "2024-05-01 10:00 EST")),
               "could not be parsed")
  expect_error(B(c("2024-05-01", NA)), "missing values")
})

test_that("timestamp_format reads other layouts and must match the whole value", {
  ev <- data.frame(id = "a", ch = c("x", "y"),
                   ts = c("05/01/2024 09:00", "05/02/2024 10:30"),
                   conv = c(0, 1))
  p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                   conversion = "conv", timestamp_format = "%m/%d/%Y %H:%M")
  expect_equal(p$timestamp,
               as.POSIXct(c("2024-05-01 09:00", "2024-05-02 10:30"),
                          tz = "UTC"))
  expect_equal(p$time_to_conversion, c(1 + 1.5 / 24, 0))

  # Trailing text that strptime() would skip is a mismatch.
  ev$ts[2L] <- "05/02/2024 10:30 extra"
  expect_error(
    build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                conversion = "conv", timestamp_format = "%m/%d/%Y %H:%M"),
    "did not parse completely"
  )
  expect_error(
    build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                conversion = "conv", timestamp_format = 1),
    "single string"
  )
  ok <- data.frame(id = "a", ch = "x",
                   ts = as.POSIXct("2024-05-01", tz = "UTC"), conv = 1)
  expect_warning(
    build_paths(ok, id = "id", channel = "ch", timestamp = "ts",
                conversion = "conv", timestamp_format = "%Y"),
    "ignored"
  )
})

# ---- other input handling ------------------------------------------------------

test_that("an exact duplicate conversion row is one conversion, not two journeys", {
  ev <- data.frame(id = "a", ch = c("search", "direct", "direct"),
                   ts = c(1, 5, 5), conv = c(0, 1, 1), val = c(NA, 50, 50))
  expect_message(
    p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                     conversion = "conv", value = "val"),
    "duplicate conversion"
  )
  expect_identical(unique(p$path_id), "a#1")
  expect_identical(unique(p$conversion_value), 50)
  s <- path_summary(p)
  expect_identical(s$duplicate_events, 1L)
  expect_identical(s$conversion_events, 1L)
  expect_identical(s$events_in, 3L)

  kept <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                      conversion = "conv", value = "val", dedupe = FALSE)
  expect_identical(length(unique(kept$path_id[kept$converted])), 2L)
  expect_identical(path_summary(kept)$duplicate_events, 0L)

  # Rows differing in value are two events and both stay.
  ev2 <- ev
  ev2$val[3L] <- 70
  p2 <- build_paths(ev2, id = "id", channel = "ch", timestamp = "ts",
                    conversion = "conv", value = "val")
  expect_identical(path_summary(p2)$duplicate_events, 0L)
  expect_error(build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                           conversion = "conv", dedupe = NA), "dedupe")
})

test_that("a touch tied with its conversion stays in the journey the conversion closes", {
  ev <- data.frame(id = "a", ch = c("direct", "search", "email"),
                   ts = c(5, 5, 1), conv = c(1, 0, 0))
  p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                   conversion = "conv")
  expect_identical(unique(p$path_id), "a#1")
  expect_identical(p$channel, c("email", "search", "direct"))
  expect_true(all(p$converted))
  expect_equal(credit_last(p)$credit, c(0, 0, 1))
})

test_that("numeric timestamps are an index: units are 'index' and misuse warns", {
  ev <- data.frame(id = "a", ch = c("tv", "search"), ts = c(0, 2), conv = c(0, 1))
  p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                   conversion = "conv")
  expect_identical(attr(p, "mm_units"), "index")
  expect_identical(attr(conversion_lag(p), "units"), "index")
  expect_warning(
    build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                conversion = "conv", units = "hours"),
    "no effect"
  )
  epoch <- ev
  epoch$ts <- 1714550400 + c(0, 2 * 86400)
  expect_warning(
    build_paths(epoch, id = "id", channel = "ch", timestamp = "ts",
                conversion = "conv", lookback = 30),
    "epoch"
  )
  # Calendar timestamps keep their units.
  cal <- ev
  cal$ts <- as.POSIXct("2024-05-01", tz = "UTC") + ev$ts * 3600
  expect_identical(
    attr(build_paths(cal, id = "id", channel = "ch", timestamp = "ts",
                     conversion = "conv", units = "hours"), "mm_units"),
    "hours"
  )
})

test_that("character conversion flags are read as numbers or words, or abort", {
  B <- function(flag) {
    ev <- data.frame(id = "a", ch = c("tv", "search"), ts = c(1, 2),
                     conv = c("0", flag))
    any(build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                    conversion = "conv")$converted)
  }
  expect_true(B("1.0"))
  expect_true(B("2"))
  expect_true(B(" Yes "))
  expect_false(B("no"))
  expect_false(B(""))
  expect_error(B("purchase"), "purchase")
  expect_error(B("purchase"), "conversion flag")
})

test_that("numeric ids keep their digits and value text off conversions is not read", {
  ev <- data.frame(id = c(100000, 100000, 2.5e6), ch = c("tv", "search", "tv"),
                   ts = c(1, 2, 1), conv = c(0, 1, 0),
                   val = c("n/a", "40", "-"))
  p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                   conversion = "conv", value = "val")
  expect_setequal(unique(p$path_id), c("100000#1", "2500000#1"))
  expect_identical(unique(p$id), c("100000", "2500000"))
  expect_identical(unique(p$conversion_value[p$converted]), 40)

  ev$val[2L] <- "forty"
  expect_error(build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                           conversion = "conv", value = "val"),
               "forty")
})

test_that("lookback and gap accept Inf, and path_summary reports the pre-filter count", {
  ev <- data.frame(id = "a", ch = c("x", "y", "z"), ts = c(0, 500, 1000),
                   conv = c(0, 0, 1))
  p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                   conversion = "conv", lookback = Inf,
                   split_on = c("conversion", "gap"), gap = Inf)
  expect_identical(p$channel, c("x", "y", "z"))
  expect_error(build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                           conversion = "conv", lookback = -Inf), "lookback")

  d <- data.frame(id = c("a", "b"), ch = c("direct", "tv"), ts = c(1, 1),
                  conv = c(1, 0))
  dropped <- build_paths(d, id = "id", channel = "ch", timestamp = "ts",
                         conversion = "conv", direct = "drop")
  s <- path_summary(dropped)
  expect_identical(s$journeys_before_filtering, 2L)
  expect_identical(s$journeys, 1L)
})

test_that("journey facts computed on runs match a per-journey calculation", {
  set.seed(5)
  n <- 3000
  ev <- data.frame(id = sample(sprintf("c%02d", 1:60), n, replace = TRUE),
                   ts = round(runif(n, 0, 300)),
                   conv = as.integer(runif(n) < 0.15))
  ev <- ev[order(ev$id, ev$ts, ev$conv), ]
  new_id <- c(TRUE, ev$id[-1L] != ev$id[-n])
  for (split_on in list("conversion", "none", "gap", c("conversion", "gap"))) {
    starts <- mediamix:::.mm_journey_starts(new_id, ev$conv, ev$ts, split_on,
                                            gap_num = 20)
    # Reference: walk each customer's events on their own.
    ref <- unlist(lapply(split(seq_len(n), ev$id), function(rows) {
      s <- rep(FALSE, length(rows))
      s[1L] <- TRUE
      for (k in seq_along(rows)[-1L]) {
        prev <- rows[k - 1L]
        s[k] <- ("conversion" %in% split_on && ev$conv[prev] == 1L) ||
          ("gap" %in% split_on && ev$ts[rows[k]] - ev$ts[prev] > 20)
      }
      s
    }), use.names = FALSE)
    expect_identical(starts, ref, info = paste(split_on, collapse = "+"))
  }

  run <- cumsum(starts)
  val <- ifelse(ev$conv == 1L, round(runif(n, 1, 50), 2), NA)
  val[sample(which(ev$conv == 1L), 10)] <- NA
  got <- mediamix:::.mm_run_sums(val, run, max(run), ev$conv == 1L)
  ref <- vapply(split(seq_len(n), factor(run, levels = seq_len(max(run)))),
                function(rows) {
                  v <- val[rows][ev$conv[rows] == 1L]
                  v <- v[!is.na(v)]
                  if (length(v) == 0L) NA_real_ else sum(v)
                }, numeric(1))
  expect_identical(got, unname(ref))
})

# ---- keep ----------------------------------------------------------------------

# Data columns only, so that attributes such as the settings do not count.
cols_of <- function(p, nms = names(p)) as.list(as.data.frame(p))[nms]

test_that("keep carries columns per touch, in their own type, and changes nothing else", {
  data(mm_events)
  ev <- mm_events
  ev$device <- factor(ifelse(seq_len(nrow(ev)) %% 3 == 0, "mobile", "desktop"))
  ev$cost <- seq_len(nrow(ev)) / 100
  ev$day <- as.Date("2025-01-01") + seq_len(nrow(ev)) %% 30
  B <- function(...) {
    build_paths(ev, id = "customer_id", channel = "channel",
                timestamp = "timestamp", conversion = "conversion",
                value = "value", ...)
  }
  plain <- B()
  kept <- B(keep = c("device", "cost", "day", "customer_id"))
  expect_identical(names(kept), c(names(plain), "device", "cost", "day",
                                  "customer_id"))
  expect_identical(cols_of(kept, names(plain)), cols_of(plain))
  expect_identical(attr(kept, "mm_counts"), attr(plain, "mm_counts"))
  expect_identical(attr(kept, "mm_settings")$keep,
                   c("device", "cost", "day", "customer_id"))

  # cost is unique per event, so it identifies the row each touch came from.
  src <- match(kept$cost, ev$cost)
  expect_identical(kept$id, ev$customer_id[src])
  expect_identical(kept$timestamp, ev$timestamp[src])
  expect_identical(kept$device, ev$device[src])
  expect_identical(kept$day, ev$day[src])
  expect_identical(levels(kept$device), c("desktop", "mobile"))
})

test_that("kept values come from the first event of a collapsed run and survive dedupe", {
  ev <- data.frame(id = "a", ch = c("tv", "tv", "tv", "search"),
                   ts = c(1, 2, 3, 4), conv = c(0, 0, 0, 1),
                   cost = c(5, 6, 7, 8))
  p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                   conversion = "conv", keep = "cost")
  expect_identical(p$cost, c(5, 8))
  every <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                       conversion = "conv", keep = "cost",
                       collapse_repeats = FALSE)
  expect_identical(every$cost, c(5, 6, 7, 8))

  # A pixel firing twice is still one event when a kept column differs.
  dup <- data.frame(id = "a", ch = c("tv", "search", "search"),
                    ts = c(1, 2, 2), conv = c(0, 1, 1),
                    tag = c("x", "first", "second"))
  expect_message(
    pd <- build_paths(dup, id = "id", channel = "ch", timestamp = "ts",
                      conversion = "conv", keep = "tag"),
    "duplicate conversion"
  )
  expect_identical(pd$tag, c("x", "first"))
  expect_identical(unique(pd$path_id), "a#1")
})

test_that("keep handles list columns, POSIXlt and a single journey", {
  ev <- data.frame(id = "a", ch = c("tv", "search"), ts = c(1, 2),
                   conv = c(0, 1))
  ev$items <- list(1:2, "basket")
  ev$seen <- as.POSIXlt(as.POSIXct(c("2024-01-01 10:00", "2024-01-02 11:00"),
                                   tz = "UTC"))
  p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                   conversion = "conv", keep = c("items", "seen"))
  expect_identical(p$items, list(1:2, "basket"))
  expect_s3_class(p$seen, "POSIXct")
  expect_equal(credit_last(p)$credit, c(0, 1))
})

test_that("kept columns are typed in an empty result", {
  ev <- data.frame(id = c("a", "b"), ch = "direct", ts = c(1, 2),
                   conv = c(1, 0), device = factor(c("x", "y")))
  none <- build_paths(ev[0, ], id = "id", channel = "ch", timestamp = "ts",
                      conversion = "conv", keep = "device")
  expect_identical(nrow(none), 0L)
  expect_s3_class(none$device, "factor")
  expect_warning(
    gone <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                        conversion = "conv", keep = "device",
                        direct = "drop"),
    "direct traffic"
  )
  expect_identical(levels(gone$device), c("x", "y"))
  expect_identical(nrow(attribute(gone)), 0L)
})

test_that("keep validates its column names", {
  ev <- data.frame(id = "a", ch = c("tv", "search"), ts = c(1, 2),
                   conv = c(0, 1), touch_n = 1, credit = 2)
  B <- function(keep) {
    build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                conversion = "conv", keep = keep)
  }
  expect_error(B(1), "character vector")
  expect_error(B(NA_character_), "character vector")
  expect_error(B(""), "character vector")
  expect_error(B("device"), "not in the data")
  expect_error(B("touch_n"), "journey table uses")
  expect_error(B("credit"), "credit")
  ev$m <- matrix(1:4, 2)
  expect_error(B("m"), "vector columns")
  # Repeated names are kept once; an empty vector keeps nothing.
  expect_identical(names(B(c("ch", "ch")))[10], "ch")
  expect_identical(ncol(B(character(0))), 9L)
})

test_that("kept columns travel through binding, subsetting and every consumer", {
  ev <- data.frame(
    id = c("a", "a", "a", "b", "b", "c"),
    ch = c("tv", "search", "email", "tv", "email", "search"),
    ts = c(1, 2, 3, 1, 2, 1), conv = c(0, 0, 1, 0, 0, 1),
    val = c(NA, NA, 40, NA, NA, 25),
    device = c("pc", "pc", "phone", "pc", "pc", "phone")
  )
  B <- function(...) {
    build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                conversion = "conv", value = "val", ...)
  }
  plain <- B()
  kept <- B(keep = "device")

  # Every result is the same as without the extra column.
  expect_identical(attribute(kept), attribute(plain))
  expect_identical(markov_removal(kept), markov_removal(plain))
  expect_identical(as_channel_paths(kept), as_channel_paths(plain))
  expect_identical(path_diagnostics(kept), path_diagnostics(plain))
  expect_identical(credit_linear(kept)$credit, credit_linear(plain)$credit)

  # Subsetting keeps the column and the class.
  sub <- kept[kept$device == "pc", ]
  expect_s3_class(sub, "mm_paths")
  expect_true(all(sub$device == "pc"))
  expect_s3_class(kept[, c(mediamix:::.mm_path_cols(), "device")], "mm_paths")

  # Binding keeps it; tables with different columns do not bind.
  both <- bind_paths(one = kept, two = kept)
  expect_identical(both$device, rep(kept$device, 2))
  expect_identical(attr(both, "mm_settings")$keep, "device")
  expect_error(bind_paths(kept, plain), "different columns")
  other <- build_paths(transform(ev, id = paste0(id, "2")), id = "id",
                       channel = "ch", timestamp = "ts", conversion = "conv",
                       value = "val", keep = "device")
  expect_identical(rbind(kept, other)$device, c(kept$device, other$device))

  pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(plot(attribute(kept)))
})

test_that("dplyr verbs keep the kept columns", {
  skip_if_not_installed("dplyr")
  ev <- data.frame(id = c("a", "a", "b"), ch = c("tv", "search", "email"),
                   ts = c(1, 2, 1), conv = c(0, 1, 1),
                   device = c("pc", "phone", "phone"))
  p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                   conversion = "conv", keep = "device")
  f <- dplyr::filter(p, device == "phone")
  expect_s3_class(f, "mm_paths")
  expect_identical(f$touch_rank, c(1L, 1L))
  expect_identical(f$touch_n, c(1L, 1L))
  m <- dplyr::mutate(p, device = toupper(device))
  expect_identical(m$device, c("PC", "PHONE", "PHONE"))
  expect_s3_class(m, "mm_paths")
})

# ---- conversion_touch ------------------------------------------------------------

test_that("conversion_touch = 'drop' removes the conversion event but not the conversion", {
  ev <- data.frame(id = "a", ch = c("display", "search", "checkout"),
                   ts = c(1, 2, 4), conv = c(0, 0, 1), val = c(NA, NA, 80))
  B <- function(...) {
    build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                conversion = "conv", value = "val", ...)
  }
  kept <- B()
  dropped <- B(conversion_touch = "drop")
  expect_identical(kept$channel, c("display", "search", "checkout"))
  expect_identical(dropped$channel, c("display", "search"))
  expect_true(all(dropped$converted))
  expect_identical(unique(dropped$conversion_value), 80)
  expect_identical(dropped$time_to_conversion, c(3, 2))
  expect_identical(dropped$touch_n, c(2L, 2L))
  expect_equal(credit_last(dropped)$credit, c(0, 1))
  expect_identical(attr(dropped, "mm_settings")$conversion_touch, "drop")
  expect_identical(attr(kept, "mm_settings")$conversion_touch, "keep")
  expect_identical(path_summary(dropped)$converting_journeys, 1L)
  expect_identical(path_summary(dropped)$events_out, 2L)
  expect_error(B(conversion_touch = "omit"), "should be one of")
})

test_that("a touch tied with the conversion stays when the conversion row goes", {
  ev <- data.frame(id = "a", ch = c("checkout", "email", "tv"),
                   ts = c(5, 5, 1), conv = c(1, 0, 0))
  p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                   conversion = "conv", conversion_touch = "drop")
  expect_identical(p$channel, c("tv", "email"))
  expect_identical(p$time_to_conversion, c(4, 0))
})

test_that("a journey whose only event was its conversion becomes unattributable", {
  ev <- data.frame(id = c("a", "b", "b"), ch = c("direct", "tv", "direct"),
                   ts = c(1, 1, 2), conv = c(1, 0, 1), val = c(30, NA, 50))
  p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                   conversion = "conv", value = "val",
                   conversion_touch = "drop")
  expect_identical(unique(p$path_id), "b#1")
  s <- path_summary(p)
  expect_identical(s$conversions_observed, 2L)
  expect_identical(s$converting_journeys, 1L)
  expect_identical(s$conversions_unattributable, 1L)
  expect_equal(sum(credit_linear(p)$credit), 1)
  expect_match(paste(capture.output(print(p)), collapse = "\n"),
               "cannot be attributed")

  # With a lookback, a converting journey can now lose every touch.
  expect_warning(
    lb <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                      conversion = "conv", conversion_touch = "drop",
                      lookback = 0.5),
    "lookback window removed"
  )
  expect_identical(nrow(lb), 0L)
  expect_identical(path_summary(lb)$conversions_unattributable, 2L)
})

test_that("dropping every conversion event warns and returns an empty table", {
  ev <- data.frame(id = c("a", "b"), ch = c("tv", "email"), ts = c(1, 1),
                   conv = c(1, 1), device = c("pc", "phone"))
  expect_warning(
    p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                     conversion = "conv", conversion_touch = "drop",
                     keep = "device"),
    "conversion event"
  )
  expect_identical(nrow(p), 0L)
  expect_identical(names(p)[10], "device")
  expect_identical(path_summary(p)$conversions_unattributable, 2L)
})

test_that("conversion_touch = 'drop' removes every conversion row in a journey", {
  ev <- data.frame(id = "a", ch = c("tv", "shop", "tv", "shop", "email"),
                   ts = 1:5, conv = c(0, 1, 0, 1, 0))
  expect_message(
    p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                     conversion = "conv", split_on = "none",
                     conversion_touch = "drop", collapse_repeats = FALSE),
    "more than one conversion"
  )
  expect_identical(p$channel, c("tv", "tv"))
  collapsed <- suppressMessages(
    build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                conversion = "conv", split_on = "none",
                conversion_touch = "drop")
  )
  expect_identical(collapsed$channel, "tv")
})

test_that("conversion_touch = 'drop' without a conversion column warns", {
  ev <- data.frame(id = "a", ch = c("tv", "search"), ts = c(1, 2))
  expect_warning(
    p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                     split_on = "none", conversion_touch = "drop"),
    "no effect"
  )
  expect_identical(p$channel, c("tv", "search"))
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

test_that("an NA in a logical row index drops those rows instead of keeping NA rows", {
  data(mm_events)
  paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                       timestamp = "timestamp", conversion = "conversion")
  expect_message(sub <- paths[paths$time_to_conversion <= 3, ], "Dropped")
  expect_false(anyNA(sub$path_id))
  expect_true(all(sub$converted))
  expect_no_error(suppressWarnings(markov_removal(sub)))
  expect_equal(sum(credit_linear(sub)$credit),
               length(unique(sub$path_id)))
})

test_that("a row selected twice is re-ranked even when the row count is unchanged", {
  ev <- data.frame(id = "a", ch = c("x", "y", "z"), ts = 1:3, conv = c(0, 0, 1))
  p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                   conversion = "conv")
  twice <- p[c(1, 1, 3), ]
  expect_identical(twice$touch_rank, 1:3)
  expect_identical(twice$touch_n, rep(3L, 3))
  expect_null(attr(twice, "mm_counts"))
  expect_equal(credit_last(twice)$credit, c(0, 0, 1))
})

# ---- journey invariants ------------------------------------------------------

test_that("consumers recompute stale ranks with a message and refuse mixed journeys", {
  ev <- data.frame(id = c("a", "a", "a", "b"), ch = c("x", "y", "z", "x"),
                   ts = c(1, 2, 3, 1), conv = c(0, 0, 1, 0))
  p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                   conversion = "conv")
  stale <- p
  stale$touch_rank <- 1L
  expect_message(last <- credit_last(stale), "Recomputed")
  expect_equal(last$credit, c(0, 0, 1, 0))
  expect_identical(last$touch_rank, c(1L, 2L, 3L, 1L))
  expect_true(is.na(suppressMessages(path_summary(stale))$events_in))

  mixed <- p
  mixed$converted[1L] <- FALSE
  expect_error(credit_linear(mixed), "disagree")
  expect_error(credit_linear(mixed), "bind_paths")

  holes <- p
  holes$path_id[2L] <- NA
  expect_error(attribute(holes), "missing values")
})

test_that("dplyr verbs keep ranks and counts in step with the rows", {
  skip_if_not_installed("dplyr")
  data(mm_events)
  paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                       timestamp = "timestamp", conversion = "conversion")
  f <- dplyr::filter(paths, channel != "display")
  b <- paths[paths$channel != "display", ]
  rownames(b) <- NULL
  expect_identical(f, b)
  n_conv <- length(unique(f$path_id[f$converted]))
  expect_equal(sum(credit_first(f)$credit), n_conv)
  expect_equal(sum(credit_last(f)$credit), n_conv)
  s <- path_summary(f)
  expect_true(is.na(s$events_in))
  expect_identical(s$journeys, length(unique(f$path_id)))

  # Verbs that keep every row keep the construction counts.
  expect_false(is.null(attr(dplyr::arrange(paths, channel), "mm_counts")))
  expect_false(is.null(attr(dplyr::mutate(paths, k = 1), "mm_counts")))
  expect_s3_class(dplyr::mutate(paths, k = 1), "mm_paths")
  expect_identical(class(dplyr::select(paths, channel, touch_rank)),
                   "data.frame")

  sl <- dplyr::slice(paths, c(1, 1, 3:nrow(paths)))
  expect_false(any(mediamix:::.mm_rank_state(sl)$stale))
})

test_that("bind_rows() of tables whose journey ids collide aborts", {
  skip_if_not_installed("dplyr")
  jan <- data.frame(id = "a", ch = c("tv", "search"), ts = c(1, 2),
                    conv = c(0, 1))
  feb <- data.frame(id = "a", ch = c("email", "display"), ts = c(40, 41),
                    conv = c(0, 1))
  pj <- build_paths(jan, id = "id", channel = "ch", timestamp = "ts",
                    conversion = "conv")
  pf <- build_paths(feb, id = "id", channel = "ch", timestamp = "ts",
                    conversion = "conv")
  expect_error(dplyr::bind_rows(pj, pf), "bind_paths")
})

test_that("bind_paths() keeps journeys from separate batches apart", {
  jan <- data.frame(id = "a", ch = c("tv", "search"), ts = c(1, 2),
                    conv = c(0, 0))
  feb <- data.frame(id = "a", ch = c("email", "display"), ts = c(40, 41),
                    conv = c(0, 1), val = c(NA, 30))
  pj <- build_paths(jan, id = "id", channel = "ch", timestamp = "ts",
                    conversion = "conv")
  pf <- build_paths(feb, id = "id", channel = "ch", timestamp = "ts",
                    conversion = "conv")
  both <- bind_paths(jan = pj, feb = pf)
  expect_s3_class(both, "mm_paths")
  expect_identical(unique(both$path_id), c("jan:a#1", "feb:a#1"))
  expect_equal(sum(credit_linear(both)$credit), 1)
  expect_equal(credit_linear(both)$credit, c(0, 0, 0.5, 0.5))
  s <- path_summary(both)
  expect_true(is.na(s$events_in))
  expect_identical(s$journeys, 2L)
  expect_identical(attr(both, "mm_units"), "index")

  # A list works too, with numbered labels when the tables are unnamed.
  expect_identical(unique(bind_paths(list(pj, pf))$path_id),
                   c("1:a#1", "2:a#1"))
  expect_error(bind_paths(pj, pf, rekey = FALSE), "more than one table")
  expect_error(bind_paths(pj, as.data.frame(pf)), "mm_paths")
  expect_error(bind_paths(a = pj, a = pf), "unique")
  expect_error(bind_paths(`x:y` = pj, z = pf), "cannot contain")

  cal <- build_paths(data.frame(id = "b", ch = "tv", conv = 1,
                                ts = as.POSIXct("2024-01-01", tz = "UTC")),
                     id = "id", channel = "ch", timestamp = "ts",
                     conversion = "conv")
  expect_error(bind_paths(pj, cal), "different units")
})

test_that("rbind() refuses journey ids found in more than one table", {
  jan <- data.frame(id = "a", ch = c("tv", "search"), ts = c(1, 2),
                    conv = c(0, 0))
  feb <- data.frame(id = "a", ch = c("email", "display"), ts = c(40, 41),
                    conv = c(0, 1))
  pj <- build_paths(jan, id = "id", channel = "ch", timestamp = "ts",
                    conversion = "conv")
  pf <- build_paths(feb, id = "id", channel = "ch", timestamp = "ts",
                    conversion = "conv")
  expect_error(rbind(pj, pf), "bind_paths")
  expect_error(rbind(pj, pj), "more than one table")

  other <- build_paths(transform(feb, id = "b"), id = "id", channel = "ch",
                       timestamp = "ts", conversion = "conv")
  ok <- rbind(pj, other)
  expect_s3_class(ok, "mm_paths")
  expect_null(attr(ok, "mm_counts"))
  expect_identical(unique(ok$path_id), c("a#1", "b#1"))
  expect_equal(sum(credit_linear(ok)$credit), 1)
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

test_that("print.mm_paths writes to standard output and passes ... on", {
  ev <- data.frame(id = "a", ch = c("x", "y"), ts = c(1, 2), conv = c(0, 1),
                   val = c(NA, 12.3456))
  p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                   conversion = "conv", value = "val")
  expect_no_message(out <- capture.output(print(p)))
  expect_identical(out[1L], "<mm_paths>")
  expect_match(out[2L], "1 journey over 2 touchpoints \\(1 converting\\)")
  expect_true(any(grepl("12.3456", out, fixed = TRUE)))
  short <- capture.output(print(p, digits = 3))
  expect_false(any(grepl("12.3456", short, fixed = TRUE)))
  expect_true(any(grepl("12.3", short, fixed = TRUE)))
  expect_identical(capture.output(print(p[0, ]))[2L], "No journeys.")
})

test_that("direct = \"label\" names a blank channel as keep and drop do", {
  ev <- data.frame(id = "a", ch = c("", "tv", "direct"), ts = 1:3,
                   conv = c(0, 0, 1))
  labelled <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                          conversion = "conv", direct = "label",
                          direct_labels = "direct")
  expect_identical(labelled$channel, c("(blank)", "tv", "(direct)"))
  kept <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                      conversion = "conv", direct = "keep",
                      direct_labels = "direct")
  expect_identical(kept$channel[1], "(blank)")
})
