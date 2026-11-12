# Tests for R/media-transform.R: media_transform() and its order argument.

test_that("length(out) == length(in), including with by= grouping", {
  spend <- c(0, 500, 800, 300, 0, 0, 1200, 400)
  out <- media_transform(spend, adstock = list(decay = 0.5),
                         saturation = list(half_max = 300))
  expect_length(out, length(spend))

  g <- rep(c("a", "b"), 4)
  out_g <- media_transform(spend, adstock = list(decay = 0.5),
                           saturation = list(half_max = 300), by = g)
  expect_length(out_g, length(spend))
})

test_that("adstock-then-saturate differs from saturate-then-adstock", {
  spend <- c(0, 500, 800, 300, 0, 0, 1200, 400)
  forward <- media_transform(spend, adstock = list(decay = 0.5),
                             saturation = list(half_max = 300))
  reverse <- suppressWarnings(media_transform(
    spend, adstock = list(decay = 0.5), saturation = list(half_max = 300),
    order = "saturate_then_adstock"
  ))
  expect_false(isTRUE(all.equal(forward, reverse)))
  expect_length(reverse, length(spend))
})

test_that("adstock-then-saturate matches composing the two steps by hand", {
  spend <- c(0, 500, 800, 300, 0, 0, 1200, 400)
  forward <- media_transform(spend, adstock = list(decay = 0.5),
                             saturation = list(half_max = 300))
  hand <- saturate_hill(adstock_geometric(spend, decay = 0.5), half_max = 300)
  expect_equal(forward, hand, tolerance = 1e-12)
})

test_that("saturate-then-adstock (reversed order) matches composing by hand", {
  spend <- c(0, 500, 800, 300, 0, 0, 1200, 400)
  reverse <- suppressWarnings(media_transform(
    spend, adstock = list(decay = 0.5), saturation = list(half_max = 300),
    order = "saturate_then_adstock"
  ))
  hand <- adstock_geometric(saturate_hill(spend, half_max = 300), decay = 0.5)
  expect_equal(reverse, hand, tolerance = 1e-12)
})

test_that("the reverse order emits a warning, and the forward order does not", {
  spend <- c(0, 500, 800, 300, 0, 0, 1200, 400)
  expect_warning(
    media_transform(spend, order = "saturate_then_adstock"),
    regexp = "adstock first|before adstock"
  )
  expect_silent(media_transform(spend, order = "adstock_then_saturate"))
  expect_silent(media_transform(spend))  # default order
})

test_that("adstock kernel = 'none' short-circuits: no carryover is applied", {
  spend <- c(0, 500, 800, 300, 0, 0, 1200, 400)
  out <- media_transform(spend, adstock = list(kernel = "none"),
                         saturation = list(half_max = 300))
  expect_equal(out, saturate_hill(spend, half_max = 300), tolerance = 1e-12)
})

test_that("saturation type = 'none' short-circuits: output is adstock only", {
  spend <- c(0, 500, 800, 300, 0, 0, 1200, 400)
  out <- media_transform(spend, adstock = list(decay = 0.6),
                         saturation = list(type = "none"))
  expect_equal(out, adstock_geometric(spend, decay = 0.6), tolerance = 1e-12)
})

test_that("kernel = 'none' and type = 'none' together return x unchanged", {
  spend <- c(0, 500, 800, 300, 0, 0, 1200, 400)
  out <- media_transform(spend, adstock = list(kernel = "none"),
                         saturation = list(type = "none"))
  expect_equal(out, spend, tolerance = 1e-12)
})

test_that("a weibull kernel composes correctly through media_transform", {
  spend <- c(0, 500, 800, 300, 0, 0, 1200, 400)
  out <- media_transform(
    spend,
    adstock = list(kernel = "weibull", shape = 2, scale = 2, max_lag = 6,
                   type = "pdf"),
    saturation = list(type = "hill", half_max = 300, shape = 2)
  )
  hand <- saturate_hill(
    adstock_weibull(spend, shape = 2, scale = 2, max_lag = 6, type = "pdf"),
    half_max = 300, shape = 2
  )
  expect_equal(out, hand, tolerance = 1e-12)
})

test_that("media_transform() with by= matches per-group adstock and shared saturation", {
  spend <- c(100, 50, 25, 200, 100, 50)
  geo <- c("north", "north", "north", "south", "south", "south")
  out <- media_transform(spend, adstock = list(decay = 0.5),
                         saturation = list(half_max = 80), by = geo)
  hand <- saturate_hill(adstock_geometric(spend, decay = 0.5, by = geo),
                        half_max = 80)
  expect_equal(out, hand, tolerance = 1e-12)
})

test_that("media_transform() validates its list arguments and kernel/type values", {
  spend <- c(1, 2, 3)
  expect_error(media_transform(spend, adstock = 5), class = "rlang_error")
  expect_error(media_transform(spend, saturation = "hill"), class = "rlang_error")
  expect_error(media_transform(spend, adstock = list(kernel = "bogus")),
               class = "rlang_error")
  expect_error(media_transform(spend, saturation = list(type = "bogus")),
               class = "rlang_error")
})

test_that("by inside the adstock list is honoured, and by given twice is an error", {
  spend <- c(100, 50, 25, 200, 100, 50)
  geo <- rep(c("north", "south"), each = 3)
  per_group <- adstock_geometric(spend, decay = 0.5, by = geo)
  # This used to pool the two geographies: 50 50 37.5 118.75 109.375 79.69.
  expect_equal(media_transform(spend, adstock = list(decay = 0.5, by = geo)),
               per_group)
  expect_equal(media_transform(spend, adstock = list(decay = 0.5), by = geo),
               per_group)
  expect_error(media_transform(spend, adstock = list(decay = 0.5, by = geo),
                               by = geo),
               class = "rlang_error", regexp = "twice")
  expect_error(media_transform(spend, adstock = list(decay = 0.5, by = geo[1:2])),
               class = "rlang_error", regexp = "same length")
})

test_that("an x inside either list is an error rather than silently replaced", {
  spend <- c(100, 50, 25)
  expect_error(media_transform(spend, adstock = list(decay = 0.5, x = 1:3)),
               class = "rlang_error", regexp = "must not contain")
  expect_error(media_transform(spend, saturation = list(half_max = 10, x = 1:3)),
               class = "rlang_error", regexp = "must not contain")
})

test_that("the reverse-order warning gives the reason that applies", {
  spend <- c(0, 500, 800, 300, 0, 0, 1200, 400)
  warning_text <- function(...) {
    tryCatch(media_transform(spend, ..., saturation = list(half_max = 300),
                             order = "saturate_then_adstock"),
             warning = conditionMessage)
  }
  normalised <- warning_text(adstock = list(decay = 0.9))
  expect_match(normalised, "accumulated media pressure")
  expect_no_match(normalised, "past the ceiling")
  raw <- warning_text(adstock = list(decay = 0.9, normalise = FALSE))
  expect_match(raw, "past the ceiling")

  # The claims behind the two messages: a normalised kernel keeps the
  # saturate-first result under the ceiling, an unnormalised one does not.
  set.seed(7)
  x <- stats::rexp(200, 1 / 1000)
  first <- function(normalise) {
    suppressWarnings(media_transform(
      x, adstock = list(decay = 0.9, normalise = normalise),
      saturation = list(half_max = 500), order = "saturate_then_adstock"))
  }
  expect_lt(max(first(TRUE)), 1)
  expect_gt(max(first(FALSE)), 1)
})

test_that("errors from either step name the transform and keep the call short", {
  x <- c(1, NA, rep(3, 20000))
  err <- tryCatch(media_transform(x, adstock = list(decay = 0.5)),
                  error = identity)
  expect_identical(deparse(conditionCall(err)[[1L]]), "adstock_geometric")
  expect_lt(nchar(paste(deparse(conditionCall(err)), collapse = " ")), 200L)

  err <- tryCatch(media_transform(c(1, -2), adstock = NULL,
                                  saturation = list(half_max = 1)),
                  error = identity)
  expect_identical(deparse(conditionCall(err)[[1L]]), "saturate_hill")
})

test_that("adstock = NULL means no carryover, and the default still adstocks", {
  spend <- c(0, 500, 800, 300, 0, 0, 1200, 400)
  expect_equal(media_transform(spend, adstock = NULL,
                               saturation = list(half_max = 300)),
               saturate_hill(spend, half_max = 300))
  # The documented default: saturation alone still gets decay 0.5 carryover.
  expect_equal(media_transform(spend, saturation = list(half_max = 300)),
               saturate_hill(adstock_geometric(spend, decay = 0.5), half_max = 300))
  expect_equal(media_transform(spend, adstock = list(decay = 0.5),
                               saturation = NULL),
               adstock_geometric(spend, decay = 0.5))
})

test_that("media_transform() keeps names and time-series attributes", {
  x <- c(w1 = 100, w2 = 0, w3 = 50)
  out <- media_transform(x, adstock = list(decay = 0.5),
                         saturation = list(half_max = 100))
  expect_named(out, names(x))
  tx <- stats::ts(c(100, 0, 50, 0), frequency = 12, start = c(2024, 6))
  out_ts <- media_transform(tx, adstock = list(decay = 0.5))
  expect_s3_class(out_ts, "ts")
  expect_equal(stats::tsp(out_ts), stats::tsp(tx))
})
