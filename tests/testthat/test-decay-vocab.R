# Tests for decay_from_half_life(), half_life() and effective_window()
# (R/decay-vocab.R). This is the shared vocabulary translation layer, so the
# main things worth nailing down are the round trip, the documented worked
# example, and the boundary validation.

test_that("decay_from_half_life() matches the closed-form formula", {
  # theta = 0.5^(period / half_life)
  expect_equal(decay_from_half_life(3), 0.5^(1 / 3), tolerance = 1e-12)
  expect_equal(decay_from_half_life(1), 0.5, tolerance = 1e-12)
  expect_equal(decay_from_half_life(10, period = 2), 0.5^(2 / 10),
               tolerance = 1e-12)
})

test_that("decay_from_half_life(3) is approximately 0.7937", {
  expect_equal(decay_from_half_life(3), 0.7937005, tolerance = 1e-6)
})

test_that("decay_from_half_life() / half_life() round-trip", {
  for (h in c(0.5, 1, 2, 3, 5.5, 10, 26)) {
    theta <- decay_from_half_life(h)
    expect_equal(half_life(theta), h, tolerance = 1e-8)
  }
  # And the other direction.
  for (theta in c(0.1, 0.3, 0.5, 0.7, 0.9, 0.99)) {
    h <- half_life(theta)
    expect_equal(decay_from_half_life(h), theta, tolerance = 1e-8)
  }
})

test_that("period rescaling gives the same decay for an equivalent half-life", {
  # A 21-day half-life observed weekly (period = 7) is the same carryover as
  # a 3-period half-life stated directly in periods.
  expect_equal(decay_from_half_life(21, period = 7), decay_from_half_life(3),
               tolerance = 1e-12)
})

test_that("effective_window() matches the documented worked example", {
  # effective_window(decay_from_half_life(3), 0.9) == 10, by hand:
  # theta = 0.5^(1/3); smallest n with 1 - theta^n >= 0.9
  # n = ceiling(log(0.1) / log(theta)) = ceiling(9.977..) = 10
  theta <- decay_from_half_life(3)
  expect_equal(effective_window(theta, 0.9), 10L)
  expect_type(effective_window(theta, 0.9), "integer")
})

test_that("effective_window() is the smallest n satisfying the coverage bound", {
  theta <- 0.6
  coverage <- 0.8
  n <- effective_window(theta, coverage)
  expect_gte(1 - theta^n, coverage)
  expect_lt(1 - theta^(n - 1L), coverage)
})

test_that("effective_window(decay = 0, ...) is the degenerate 1-period window", {
  expect_identical(effective_window(0, 0.9), 1L)
  expect_identical(effective_window(0, 0.5), 1L)
})

test_that("decay_from_half_life() validates half_life and period", {
  expect_error(decay_from_half_life(0), class = "rlang_error")
  expect_error(decay_from_half_life(-1), class = "rlang_error")
  expect_error(decay_from_half_life(Inf), class = "rlang_error")
  expect_error(decay_from_half_life(3, period = 0), class = "rlang_error")
  expect_error(decay_from_half_life(3, period = -2), class = "rlang_error")
  expect_error(decay_from_half_life(c(1, 2)), class = "rlang_error")
  expect_error(decay_from_half_life(NA_real_), class = "rlang_error")
})

test_that("half_life() requires decay strictly inside (0, 1)", {
  expect_error(half_life(0), class = "rlang_error", regexp = "\\(0, 1\\)")
  expect_error(half_life(1), class = "rlang_error", regexp = "\\(0, 1\\)")
  expect_error(half_life(-0.1), class = "rlang_error")
  expect_error(half_life(1.1), class = "rlang_error")
})

test_that("effective_window() requires decay in [0, 1) and coverage in (0, 1)", {
  expect_error(effective_window(1), class = "rlang_error", regexp = "\\[0, 1\\)")
  expect_error(effective_window(-0.1), class = "rlang_error")
  expect_error(effective_window(0.5, coverage = 0), class = "rlang_error")
  expect_error(effective_window(0.5, coverage = 1), class = "rlang_error")
  expect_error(effective_window(0.5, coverage = -0.1), class = "rlang_error")
})

test_that("the decay vocabulary is shared with adstock_geometric()", {
  theta <- decay_from_half_life(2)
  spend <- c(100, 0, 0, 0, 0)
  out <- adstock_geometric(spend, decay = theta)
  expect_length(out, length(spend))
  expect_true(is.numeric(out))
})
