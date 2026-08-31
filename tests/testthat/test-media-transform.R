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
