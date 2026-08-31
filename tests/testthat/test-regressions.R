# Regressions for bugs found during development. Each was reachable from the
# package's own documented examples.

test_that("diagnose_media() handles exactly two media columns (dimname drop)", {
  # cors[i, -i] collapses to an unnamed scalar when there are two channels,
  # which used to break the vapply building `most_correlated_with`.
  d <- data.frame(a = c(1, 2, 3, 4, 5, 6, 7, 8),
                  b = c(2, 1, 4, 3, 6, 5, 8, 7))
  res <- diagnose_media(d, media = c("a", "b"))
  expect_s3_class(res, "mm_diagnosis")
  expect_equal(nrow(res$collinearity), 2L)
  expect_setequal(res$collinearity$most_correlated_with, c("a", "b"))
  expect_true(all(is.finite(res$collinearity$correlation)))

  # One and three columns must keep working too.
  expect_equal(nrow(diagnose_media(d, media = "a")$collinearity), 1L)
  d$c <- c(5, 5, 6, 6, 7, 7, 8, 8)
  expect_equal(nrow(diagnose_media(d, media = c("a", "b", "c"))$collinearity), 3L)
})

test_that("blank channel labels are named, and survive the assist matrix", {
  # A channel literally named "" cannot be indexed by name in a matrix or a
  # table, so build_paths() names it "(blank)" rather than leaving it nameless.
  ev <- data.frame(
    cust = c("a", "a", "a", "b", "b"),
    ch = c("", "social", "organic_search", "", "social"),
    ts = as.POSIXct("2024-01-01", tz = "UTC") + c(0, 86400, 172800, 0, 86400),
    conv = c(0, 0, 1, 0, 1),
    stringsAsFactors = FALSE
  )
  paths <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                       conversion = "conv")
  expect_false("" %in% paths$channel)
  expect_true("(blank)" %in% paths$channel)

  m <- assisted_conversions(paths)
  expect_true(is.matrix(m))
  # The blank channel precedes social in both journeys.
  expect_equal(unname(m["(blank)", "social"]), 2)
  expect_equal(unname(m["social", "(blank)"]), 0)

  expect_no_error(assisted_conversions(paths, normalise = TRUE))
  expect_no_error(path_diagnostics(paths))
})

test_that("the documented assisted_conversions() example runs on mm_events", {
  # mm_events deliberately contains blank channel labels, so the default
  # pipeline must not error.
  data(mm_events, package = "mediamix")
  paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                       timestamp = "timestamp", conversion = "conversion")
  expect_no_error(assisted_conversions(paths, normalise = TRUE))
  expect_length(path_diagnostics(paths), 6L)
})

test_that("dropping direct traffic cannot erase a conversion", {
  # The conversion event itself sits on a direct channel. Dropping direct
  # touches must still count the conversion, and must report it as
  # unattributable rather than losing it silently.
  ev <- data.frame(
    cust = c("a", "a", "b"),
    ch = c("social", "direct", "direct"),
    ts = as.POSIXct("2024-01-01", tz = "UTC") + c(0, 86400, 0),
    conv = c(0, 1, 1),
    stringsAsFactors = FALSE
  )
  kept <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                      conversion = "conv", direct = "keep")
  expect_equal(path_summary(kept)$conversions_observed, 2L)
  expect_equal(path_summary(kept)$conversions_unattributable, 0L)

  dropped <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                         conversion = "conv", direct = "drop")
  s <- path_summary(dropped)
  expect_equal(s$conversions_observed, 2L)
  # Customer "a" keeps its social touch and stays attributable; customer "b"
  # had only a direct touch and is now unattributable.
  expect_equal(s$converting_journeys, 1L)
  expect_equal(s$conversions_unattributable, 1L)
  # Credit still sums to what is actually attributable, not to what was observed.
  expect_equal(sum(credit_linear(dropped)$credit), 1)
})
