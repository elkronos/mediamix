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

test_that("effective_window() is exact at whole half-life boundaries", {
  # With coverage 0.5 the window is the half-life itself. Rounding in the logs
  # used to push 2, 3 and 7 one period too far.
  for (h in 1:60) {
    expect_identical(effective_window(decay_from_half_life(h), 0.5),
                     as.integer(h), info = paste("half-life", h))
  }
  expect_identical(effective_window(0.5, 0.75), 2L)
  expect_identical(effective_window(0.5, 0.7499), 2L)
  expect_identical(effective_window(0.5, 0.7501), 3L)
  # A coverage too small for 1 - coverage to differ from 1 still needs one period.
  expect_identical(effective_window(0.5, 1e-20), 1L)
})

test_that("half_life() reports the half-life in the unit of period", {
  expect_equal(half_life(decay_from_half_life(21, period = 7), period = 7), 21)
  expect_equal(half_life(decay_from_half_life(3)), 3)
})

test_that("the decay vocabulary is shared with adstock_geometric()", {
  theta <- decay_from_half_life(2)
  spend <- c(100, 0, 0, 0, 0)
  out <- adstock_geometric(spend, decay = theta)
  expect_length(out, length(spend))
  expect_true(is.numeric(out))
})

# ---- vectorised over the first argument --------------------------------------

test_that("each function is vectorised over its first argument", {
  h <- c(0.5, 1, 2, 3, 26)
  expect_identical(decay_from_half_life(h),
                   vapply(h, decay_from_half_life, numeric(1)))
  expect_identical(decay_from_half_life(h, period = 7),
                   vapply(h, decay_from_half_life, numeric(1), period = 7))
  d <- c(0.1, 0.5, 0.85, 0.99)
  expect_identical(half_life(d), vapply(d, half_life, numeric(1)))
  expect_identical(half_life(d, period = 7),
                   vapply(d, half_life, numeric(1), period = 7))
  d0 <- c(0, d)
  expect_identical(effective_window(d0, 0.9),
                   vapply(d0, effective_window, integer(1), coverage = 0.9))
})

test_that("decay_from_half_life() builds a decay grid from half-lives", {
  grid <- decay_from_half_life(c(1, 2, 4))
  expect_equal(grid, 0.5^(1 / c(1, 2, 4)), tolerance = 1e-12)
  expect_equal(half_life(grid), c(1, 2, 4), tolerance = 1e-12)
  expect_identical(effective_window(grid, 0.5), c(1L, 2L, 4L))
})

test_that("a single value gives the same unnamed result", {
  expect_identical(decay_from_half_life(3), 0.5^(1 / 3))
  expect_identical(decay_from_half_life(21, period = 7), 0.5^(7 / 21))
  expect_identical(half_life(0.7, period = 7), 7 * log(0.5) / log(0.7))
  expect_identical(effective_window(0.5, 0.9), 4L)
  expect_null(names(decay_from_half_life(3)))
  expect_null(names(half_life(0.5)))
  expect_null(names(effective_window(0.5)))
})

test_that("names of the first argument are kept", {
  expect_named(decay_from_half_life(c(tv = 4, search = 0.5)),
               c("tv", "search"))
  d <- c(tv = 0.85, video = 0.7, search = 0.15)
  expect_named(half_life(d), names(d))
  expect_named(effective_window(d), names(d))
  expect_named(half_life(c(tv = 0.85)), "tv")
  expect_null(names(decay_from_half_life(c(1, 2))))
})

test_that("period and coverage must be single numbers", {
  expect_error(decay_from_half_life(c(1, 2), period = c(1, 7)), "`period`")
  expect_error(half_life(c(0.5, 0.6), period = c(1, 7)), "`period`")
  expect_error(effective_window(c(0.5, 0.6), coverage = c(0.5, 0.9)),
               "`coverage`")
})

test_that("an invalid element is reported by its position", {
  expect_error(decay_from_half_life(c(1, -2, 0)), "Element 2 is -2")
  expect_error(decay_from_half_life(c(1, -2, 0)), "2 elements are out of range")
  expect_error(half_life(c(0.5, 1)), "\\(0, 1\\)")
  expect_error(half_life(c(0.5, 1)), "Element 2 is 1")
  expect_error(effective_window(c(0.5, 1)), "\\[0, 1\\)")
  expect_error(decay_from_half_life(c(1, NA)), "missing")
  expect_error(half_life(c(0.5, Inf)), "finite")
  expect_error(decay_from_half_life(numeric(0)), "at least one element")
  expect_error(half_life(c("0.5", "0.6")), "numeric vector")
  expect_error(half_life(matrix(c(0.5, 0.6), 1)), "numeric vector")
})

test_that("effective_window() names the element whose window is too long", {
  expect_error(effective_window(c(0.5, 0.999999999), 0.9),
               "Element 2 of `decay`")
  expect_error(effective_window(c(0.5, 0.999999999), 0.9), "too long")
})
