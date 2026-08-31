# Tests for R/saturation.R: saturate_hill(), saturate_exponential(),
# saturate_michaelis_menten(), saturate_power() and the saturate() dispatcher.

# ---- length preservation -----------------------------------------------------

test_that("length(out) == length(in) for every saturation curve", {
  x <- c(0, 5, 25, 50, 100, 400)
  expect_length(saturate_hill(x, half_max = 50), length(x))
  expect_length(saturate_exponential(x, rate = 0.01), length(x))
  expect_length(saturate_michaelis_menten(x, vmax = 2, km = 50), length(x))
  expect_length(saturate_power(x, exponent = 0.5), length(x))
  expect_length(saturate(x, type = "hill", half_max = 50), length(x))

  expect_length(saturate_hill(7, half_max = 50), 1L)
})

# ---- hill / michaelis-menten identities --------------------------------------

test_that("saturate_hill() at x == half_max is exactly 0.5, for any shape", {
  for (shape in c(0.5, 1, 2, 5)) {
    expect_equal(saturate_hill(100, half_max = 100, shape = shape), 0.5,
                 tolerance = 1e-12)
  }
  # Vectorised too.
  x <- c(50, 50, 50)
  expect_equal(saturate_hill(x, half_max = 50, shape = 3), rep(0.5, 3),
               tolerance = 1e-12)
})

test_that("saturate_hill(shape = 1) is the same function as saturate_michaelis_menten(vmax = 1)", {
  x <- c(0, 5, 25, 50, 100, 400)
  half_max <- 40
  expect_equal(saturate_hill(x, half_max = half_max, shape = 1),
               saturate_michaelis_menten(x, vmax = 1, km = half_max),
               tolerance = 1e-12)
})

test_that("saturate_hill(shape = 1) is concave (diminishing returns everywhere)", {
  x <- seq(0, 200, by = 10)
  y <- saturate_hill(x, half_max = 50, shape = 1)
  d1 <- diff(y)
  expect_true(all(diff(d1) <= 1e-10))  # second difference non-positive: concave
})

# ---- boundedness --------------------------------------------------------------

test_that("saturate_exponential() and saturate_hill() are bounded below 1", {
  # Values chosen with enough headroom that the result is not so close to 1
  # that floating point rounds it up to exactly 1 (which would make the
  # invariant untestable rather than false).
  h <- saturate_hill(1e6, half_max = 50, shape = 2)
  expect_lt(h, 1)
  expect_gt(h, 0.999)

  e <- saturate_exponential(1000, rate = 0.01)
  expect_lt(e, 1)
  expect_gt(e, 0.99)

  # And bounded for a whole vector approaching the asymptote from below.
  # (x kept small enough relative to `rate`/`half_max` that 1 - exp(-y) does
  # not itself round to exactly 1 in double precision once y exceeds ~37;
  # that is a floating-point limit on 1 - exp(-y), not a package bug.)
  x <- c(0, 10, 100, 1000)
  expect_true(all(saturate_hill(x, half_max = 50, shape = 2) < 1))
  expect_true(all(saturate_exponential(x, rate = 0.01) < 1))
})

test_that("saturate_michaelis_menten() is bounded by vmax", {
  x <- c(0, 10, 100, 1e6)
  out <- saturate_michaelis_menten(x, vmax = 5, km = 50)
  expect_true(all(out < 5))
  expect_equal(saturate_michaelis_menten(1e9, vmax = 5, km = 50), 5,
               tolerance = 1e-4)
})

test_that("saturate_power() is unbounded but concave for exponent < 1", {
  out <- saturate_power(c(100, 1e4, 1e8), exponent = 0.5)
  expect_true(all(diff(out) > 0))          # still increasing
  expect_true(is.finite(out[3]))            # no artificial ceiling
})

# ---- power identity -------------------------------------------------------------

test_that("saturate_power(exponent = 1) is the identity", {
  x <- c(0, 1, 10, 250.5, 1000)
  expect_equal(saturate_power(x, exponent = 1), x, tolerance = 1e-12)
})

# ---- dispatcher agreement -------------------------------------------------------

test_that("saturate() dispatcher agrees with each named function", {
  x <- c(0, 10, 50, 100, 200)
  expect_equal(saturate(x, type = "hill", half_max = 50),
               saturate_hill(x, half_max = 50))
  expect_equal(saturate(x, type = "hill", half_max = 50, shape = 2),
               saturate_hill(x, half_max = 50, shape = 2))
  expect_equal(saturate(x, type = "exponential", rate = 0.02),
               saturate_exponential(x, rate = 0.02))
  expect_equal(saturate(x, type = "michaelis_menten", vmax = 2, km = 50),
               saturate_michaelis_menten(x, vmax = 2, km = 50))
  expect_equal(saturate(x, type = "power", exponent = 0.5),
               saturate_power(x, exponent = 0.5))
})

test_that("saturate() defaults to hill when type is unspecified", {
  x <- c(0, 10, 50, 100)
  expect_equal(saturate(x, half_max = 50), saturate_hill(x, half_max = 50))
})

# ---- golden values --------------------------------------------------------------

test_that("golden values: saturate_hill at known points", {
  # shape = 1: x / (x + half_max)
  expect_equal(saturate_hill(c(0, 50, 100, 300), half_max = 100, shape = 1),
               c(0, 50 / 150, 100 / 200, 300 / 400), tolerance = 1e-12)
  # shape = 2: x^2 / (x^2 + half_max^2)
  expect_equal(saturate_hill(c(0, 50, 100), half_max = 100, shape = 2),
               c(0, 2500 / (2500 + 10000), 10000 / (10000 + 10000)),
               tolerance = 1e-12)
})

test_that("golden values: saturate_exponential and saturate_power", {
  expect_equal(saturate_exponential(c(0, 100), rate = 0.01),
               c(0, 1 - exp(-1)), tolerance = 1e-12)
  expect_equal(saturate_power(c(0, 4, 100), exponent = 0.5), c(0, 2, 10),
               tolerance = 1e-12)
})

# ---- input validation -------------------------------------------------------------

test_that("negative x is rejected by every saturation curve", {
  expect_error(saturate_hill(c(-1, 2), half_max = 1), class = "rlang_error",
               regexp = "negative")
  expect_error(saturate_exponential(c(-1, 2), rate = 0.5), class = "rlang_error")
  expect_error(saturate_michaelis_menten(c(-1, 2), km = 1), class = "rlang_error")
  expect_error(saturate_power(c(-1, 2), exponent = 0.5), class = "rlang_error")
  expect_error(saturate(c(-1, 2), type = "hill", half_max = 1),
               class = "rlang_error")
})

test_that("saturation curve parameters are validated to their documented domains", {
  expect_error(saturate_hill(1:3, half_max = 0), class = "rlang_error")
  expect_error(saturate_hill(1:3, half_max = -1), class = "rlang_error")
  expect_error(saturate_hill(1:3, half_max = 5, shape = 0), class = "rlang_error")
  expect_error(saturate_hill(1:3, half_max = 5, shape = -1), class = "rlang_error")

  expect_error(saturate_exponential(1:3, rate = 0), class = "rlang_error")
  expect_error(saturate_exponential(1:3, rate = -0.1), class = "rlang_error")

  expect_error(saturate_michaelis_menten(1:3, vmax = -1, km = 1),
               class = "rlang_error")
  expect_error(saturate_michaelis_menten(1:3, km = 0), class = "rlang_error")

  # exponent in (0, 1]: 0 and > 1 are rejected, 1 itself is fine.
  expect_error(saturate_power(1:3, exponent = 0), class = "rlang_error")
  expect_error(saturate_power(1:3, exponent = 1.5), class = "rlang_error")
  expect_silent(saturate_power(1:3, exponent = 1))
})

test_that("saturate() rejects an unknown type", {
  expect_error(saturate(1:3, type = "bogus"), regexp = "should be one of")
})
