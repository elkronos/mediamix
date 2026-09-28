# Tests for as_channel_paths() (R/interop.R).

test_that("as_channel_paths(): sum(total_conversions) equals converting journeys and sum(total_null) equals non-converting ones", {
  data(mm_events)
  paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                        timestamp = "timestamp", conversion = "conversion",
                        value = "value")
  ca <- as_channel_paths(paths)

  smry <- path_summary(paths)
  expect_equal(sum(ca$total_conversions), smry$converting_journeys)
  expect_equal(sum(ca$total_null), smry$journeys - smry$converting_journeys)
})

test_that("as_channel_paths() aggregates by channel sequence and carries conversion value", {
  ev <- data.frame(
    cust = c("a", "b", "c"),
    ch = c("X", "X", "Y"),
    ts = as.POSIXct("2024-01-01", tz = "UTC"),
    conv = c(1, 0, 1),
    val = c(10, NA, 20)
  )
  p <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                    conversion = "conv", value = "val")
  ca <- as_channel_paths(p)

  expect_identical(names(ca), c("path", "total_conversions", "total_null",
                                 "total_conversion_value"))
  x_row <- ca[ca$path == "X", ]
  expect_identical(x_row$total_conversions, 1)
  expect_identical(x_row$total_null, 1)
  expect_identical(x_row$total_conversion_value, 10)

  y_row <- ca[ca$path == "Y", ]
  expect_identical(y_row$total_conversions, 1)
  expect_identical(y_row$total_null, 0)
  expect_identical(y_row$total_conversion_value, 20)

  # no value column at all -> no total_conversion_value column
  p_noval <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                          conversion = "conv")
  ca_noval <- as_channel_paths(p_noval)
  expect_false("total_conversion_value" %in% names(ca_noval))
})

test_that("as_channel_paths() uses the sep argument and validates it", {
  ev <- data.frame(
    cust = "a", ch = c("X", "Y"),
    ts = as.POSIXct("2024-01-01", tz = "UTC") + c(0, 1) * 86400,
    conv = c(0, 1)
  )
  p <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                    conversion = "conv")
  ca <- as_channel_paths(p, sep = " -> ")
  expect_identical(ca$path, "X -> Y")

  expect_error(as_channel_paths(p, sep = 1), "single string")
})

test_that("as_channel_paths() returns a zero-row table without error on empty paths", {
  empty <- build_paths(data.frame(cust = character(0), ch = character(0),
                                   ts = as.POSIXct(character(0)),
                                   conv = integer(0)),
                        id = "cust", channel = "ch", timestamp = "ts",
                        conversion = "conv")
  ca <- expect_no_error(as_channel_paths(empty))
  expect_identical(names(ca), c("path", "total_conversions", "total_null"))
  expect_identical(nrow(ca), 0L)
})

test_that("as_channel_paths() errors on a non-mm_paths input", {
  expect_error(as_channel_paths(data.frame(x = 1)), "mm_paths")
})

test_that("markov_removal() agrees with ChannelAttribution::markov_model() at order 1", {
  skip_if_not_installed("ChannelAttribution")
  skip_on_cran()
  data(mm_events, envir = environment())
  paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                       timestamp = "timestamp", conversion = "conversion",
                       direct = "label")
  ours <- markov_removal(paths)
  ca_in <- as_channel_paths(paths, sep = ">")
  theirs <- suppressWarnings(suppressMessages(utils::capture.output(
    res <- ChannelAttribution::markov_model(
      ca_in, var_path = "path", var_conv = "total_conversions",
      var_null = "total_null", order = 1, sep = ">", verbose = 0)
  )))
  if (is.list(res) && !is.data.frame(res)) res <- res[[1L]]
  ca_share <- res$total_conversions / sum(res$total_conversions)
  names(ca_share) <- trimws(res$channel_name)
  common <- intersect(ours$channel, names(ca_share))
  expect_setequal(common, ours$channel)
  # ChannelAttribution estimates by simulation; allow for its error.
  expect_equal(unname(ours$share[match(common, ours$channel)]),
               unname(ca_share[common]), tolerance = 0.05)
})
