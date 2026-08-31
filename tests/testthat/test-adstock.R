# Tests for R/adstock.R: adstock_weights(), adstock_weights_weibull(),
# adstock_geometric(), adstock_weibull(), adstock_filter() and adstock_state().
#
# This file carries the invariants the package's design doc calls mandatory:
# weight normalisation, causality (no future leakage), length preservation,
# warm-start equivalence, zero-run passthrough, golden values and level
# preservation. Causality is tested most thoroughly because tune_carryover()
# depends on it to slice a once-adstocked series into CV folds safely.

# ---- 1. adstock_weights(): normalisation invariant --------------------------

test_that("adstock_weights(): normalised weights sum to 1", {
  for (decay in c(0, 0.1, 0.5, 0.7, 0.99, 1)) {
    for (max_lag in c(1L, 2L, 5L, 20L)) {
      w <- adstock_weights(max_lag, decay)
      expect_equal(sum(w), 1, tolerance = 1e-10,
                   info = sprintf("decay=%s max_lag=%s", decay, max_lag))
    }
  }
})

test_that("adstock_weights(): unnormalised weights start at 1", {
  for (decay in c(0, 0.25, 0.5, 0.9, 1)) {
    for (max_lag in c(1L, 3L, 10L)) {
      w <- adstock_weights(max_lag, decay, normalise = FALSE)
      expect_equal(w[1], 1, tolerance = 1e-12)
    }
  }
})

test_that("adstock_weights(): weights decay geometrically and have length max_lag", {
  w <- adstock_weights(5, 0.5, normalise = FALSE)
  expect_equal(w, 0.5^(0:4), tolerance = 1e-12)
  expect_length(w, 5)
})

# ---- 2. Causality / no future leakage ----------------------------------------
# adstock(x)[1:m] must equal adstock(x[1:m]) for every m: the filter can only
# look backward. This is what makes it safe to adstock a full series once and
# then slice it into CV folds (tune_carryover()'s central shortcut).

test_that("adstock_geometric() is causal: infinite kernel", {
  set.seed(1001)
  x <- round(pmax(0, rnorm(40, 100, 40)), 2)
  full <- adstock_geometric(x, decay = 0.6, max_lag = Inf)
  for (m in c(1L, 2L, 5L, 13L, 27L, 40L)) {
    prefix <- adstock_geometric(x[1:m], decay = 0.6, max_lag = Inf)
    expect_equal(full[1:m], prefix, tolerance = 1e-10, info = paste("m =", m))
  }
})

test_that("adstock_geometric() is causal: finite max_lag kernel", {
  set.seed(1002)
  x <- round(pmax(0, rnorm(40, 100, 40)), 2)
  full <- adstock_geometric(x, decay = 0.6, max_lag = 7)
  for (m in c(1L, 2L, 5L, 13L, 27L, 40L)) {
    prefix <- adstock_geometric(x[1:m], decay = 0.6, max_lag = 7)
    expect_equal(full[1:m], prefix, tolerance = 1e-10, info = paste("m =", m))
  }
})

test_that("adstock_weibull() is causal, for both cdf and pdf forms", {
  set.seed(1003)
  x <- round(pmax(0, rnorm(35, 50, 20)), 2)
  for (type in c("cdf", "pdf")) {
    full <- adstock_weibull(x, shape = 2, scale = 3, max_lag = 8, type = type)
    for (m in c(1L, 3L, 9L, 22L, 35L)) {
      prefix <- adstock_weibull(x[1:m], shape = 2, scale = 3, max_lag = 8,
                                type = type)
      expect_equal(full[1:m], prefix, tolerance = 1e-10,
                   info = paste("type =", type, "m =", m))
    }
  }
})

test_that("adstock_filter() is causal for an arbitrary kernel", {
  set.seed(1004)
  x <- round(pmax(0, rnorm(35, 50, 20)), 2)
  w <- c(0.4, 0.3, 0.2, 0.1)
  full <- adstock_filter(x, weights = w)
  for (m in c(1L, 2L, 4L, 10L, 35L)) {
    prefix <- adstock_filter(x[1:m], weights = w)
    expect_equal(full[1:m], prefix, tolerance = 1e-10, info = paste("m =", m))
  }
})

test_that("causality holds within groups when by= is supplied", {
  set.seed(1005)
  n <- 24
  x <- round(pmax(0, rnorm(n, 80, 30)), 2)
  g <- sample(c("north", "south"), n, replace = TRUE)
  full <- adstock_geometric(x, decay = 0.5, by = g)
  for (m in c(1L, 6L, 15L, 24L)) {
    prefix <- adstock_geometric(x[1:m], decay = 0.5, by = g[1:m])
    expect_equal(full[1:m], prefix, tolerance = 1e-10, info = paste("m =", m))
  }
})

# ---- 3. Length preservation ---------------------------------------------------

test_that("length(out) == length(in) for every adstock transform", {
  set.seed(2001)
  x <- pmax(0, rnorm(17, 50, 20))

  expect_length(adstock_geometric(x, decay = 0.5), length(x))
  expect_length(adstock_geometric(x, decay = 0.5, max_lag = 4), length(x))
  expect_length(adstock_weibull(x, shape = 2, scale = 3, max_lag = 5), length(x))
  expect_length(adstock_filter(x, weights = c(0.5, 0.3, 0.2)), length(x))
})

test_that("length is preserved at length-1 input", {
  expect_length(adstock_geometric(5, decay = 0.5), 1L)
  expect_length(adstock_geometric(5, decay = 0.5, max_lag = 3), 1L)
  expect_length(adstock_weibull(5, shape = 2, scale = 3, max_lag = 3), 1L)
  expect_length(adstock_filter(5, weights = c(0.5, 0.5)), 1L)
})

test_that("length is preserved at the degenerate max_lag = 1 (zero-length state)", {
  x <- c(10, 20, 30, 40)
  # max_lag = 1 means no carryover at all: state has length 0.
  out <- adstock_geometric(x, decay = 0.9, max_lag = 1)
  expect_length(out, length(x))
  expect_equal(out, x, tolerance = 1e-12)  # no carryover -> output == input
  expect_equal(adstock_state(x, decay = 0.9, max_lag = 1), numeric(0))

  out_w <- adstock_weibull(x, shape = 2, scale = 3, max_lag = 1)
  expect_length(out_w, length(x))

  out_f <- adstock_filter(x, weights = 1)
  expect_length(out_f, length(x))
  expect_equal(out_f, x, tolerance = 1e-12)
})

test_that("length is preserved with by= grouping, including a singleton group", {
  x <- c(10, 20, 30, 40, 50)
  g <- c("a", "a", "b", "a", "b")  # group "b" and "a" both present, mixed
  out <- adstock_geometric(x, decay = 0.5, by = g)
  expect_length(out, length(x))

  # A group that has exactly one observation.
  g2 <- c("a", "a", "a", "a", "singleton")
  out2 <- adstock_geometric(x, decay = 0.5, by = g2)
  expect_length(out2, length(x))
  # The singleton group's raw (unnormalised) accumulator at a cold start is
  # just x itself; the default normalise = TRUE then scales every period,
  # including the first, by (1 - decay).
  expect_equal(out2[5], x[5] * (1 - 0.5), tolerance = 1e-12)
  raw <- adstock_geometric(x, decay = 0.5, by = g2, normalise = FALSE)
  expect_equal(raw[5], x[5], tolerance = 1e-12)
})

test_that("zero-length x is rejected rather than silently returning zero-length output", {
  # .mm_check_numeric() requires at least one element; this is intentional
  # input validation, not a length-preservation violation.
  expect_error(adstock_geometric(numeric(0), decay = 0.5), class = "rlang_error")
})

# ---- 4. Warm-start equivalence -------------------------------------------------
# Chaining state across two chunks must reproduce whole-series filtering
# exactly, for both kernel forms and with by= groups.

test_that("warm-start equivalence holds for the infinite kernel", {
  first_half <- c(100, 80, 60, 40)
  second_half <- c(20, 10, 5, 0, 30)
  decay <- 0.5

  s <- adstock_state(first_half, decay = decay)
  chunked <- c(
    adstock_geometric(first_half, decay = decay),
    adstock_geometric(second_half, decay = decay, state = s)
  )
  whole <- adstock_geometric(c(first_half, second_half), decay = decay)
  expect_equal(chunked, whole, tolerance = 1e-10)
})

test_that("warm-start equivalence holds for a finite max_lag kernel", {
  first_half <- c(100, 80, 60, 40, 15)
  second_half <- c(20, 10, 5, 0, 30, 7)
  decay <- 0.6
  max_lag <- 4

  s <- adstock_state(first_half, decay = decay, max_lag = max_lag)
  expect_length(s, max_lag - 1L)
  chunked <- c(
    adstock_geometric(first_half, decay = decay, max_lag = max_lag),
    adstock_geometric(second_half, decay = decay, max_lag = max_lag, state = s)
  )
  whole <- adstock_geometric(c(first_half, second_half), decay = decay,
                             max_lag = max_lag)
  expect_equal(chunked, whole, tolerance = 1e-10)
})

test_that("warm-start equivalence holds with by= groups, infinite kernel", {
  gA <- c(100, 80, 60, 40, 20, 10)
  gB <- c(50, 40, 30, 20, 10, 5)
  # Interleave so both chunks contain both groups.
  x <- as.numeric(rbind(gA, gB))
  by <- rep(c("A", "B"), times = length(gA))
  m <- 8L

  chunk1 <- x[1:m]; by1 <- by[1:m]
  chunk2 <- x[(m + 1L):length(x)]; by2 <- by[(m + 1L):length(x)]
  decay <- 0.4

  c1 <- adstock_geometric(chunk1, decay = decay, by = by1)
  s <- adstock_state(chunk1, decay = decay, by = by1)
  expect_true(is.list(s))
  expect_setequal(names(s), c("A", "B"))
  c2 <- adstock_geometric(chunk2, decay = decay, by = by2, state = s)

  chunked <- c(c1, c2)
  whole <- adstock_geometric(x, decay = decay, by = by)
  expect_equal(chunked, whole, tolerance = 1e-10)
})

test_that("warm-start equivalence holds with by= groups, finite max_lag", {
  gA <- c(100, 80, 60, 40, 20, 10)
  gB <- c(50, 40, 30, 20, 10, 5)
  x <- as.numeric(rbind(gA, gB))
  by <- rep(c("A", "B"), times = length(gA))
  m <- 8L
  max_lag <- 3L

  chunk1 <- x[1:m]; by1 <- by[1:m]
  chunk2 <- x[(m + 1L):length(x)]; by2 <- by[(m + 1L):length(x)]
  decay <- 0.4

  c1 <- adstock_geometric(chunk1, decay = decay, max_lag = max_lag, by = by1)
  s <- adstock_state(chunk1, decay = decay, max_lag = max_lag, by = by1)
  c2 <- adstock_geometric(chunk2, decay = decay, max_lag = max_lag, by = by2,
                          state = s)

  chunked <- c(c1, c2)
  whole <- adstock_geometric(x, decay = decay, max_lag = max_lag, by = by)
  expect_equal(chunked, whole, tolerance = 1e-10)
})

test_that("warm-start state is stored in raw units, independent of normalise", {
  # The docs promise state is always raw/unnormalised, so a state captured
  # under one normalisation setting stays valid under the other.
  first_half <- c(100, 80, 60, 40)
  second_half <- c(20, 10, 5, 0)
  decay <- 0.5

  s <- adstock_state(first_half, decay = decay)  # not affected by normalise
  norm_second <- adstock_geometric(second_half, decay = decay, state = s,
                                   normalise = TRUE)
  unnorm_second <- adstock_geometric(second_half, decay = decay, state = s,
                                     normalise = FALSE)
  expect_equal(norm_second, unnorm_second * (1 - decay), tolerance = 1e-10)
})

# ---- 5. Zero-run passthrough (flighting) ---------------------------------------

test_that("carryover decays correctly across a run of zero spend (flighting)", {
  # By hand: x = (100, 0, 0, 0, 100), decay = 0.5.
  # raw[1] = 100
  # raw[2] = 0   + 0.5*100    = 50
  # raw[3] = 0   + 0.5*50     = 25
  # raw[4] = 0   + 0.5*25     = 12.5
  # raw[5] = 100 + 0.5*12.5   = 106.25
  x <- c(100, 0, 0, 0, 100)
  expected_raw <- c(100, 50, 25, 12.5, 106.25)

  out_unnorm <- adstock_geometric(x, decay = 0.5, normalise = FALSE)
  expect_equal(out_unnorm, expected_raw, tolerance = 1e-12)

  out_norm <- adstock_geometric(x, decay = 0.5, normalise = TRUE)
  expect_equal(out_norm, expected_raw * 0.5, tolerance = 1e-12)
})

test_that("a flight gap does not reset carryover to zero", {
  flighted <- c(100, 100, 0, 0, 0, 0, 100)
  out <- adstock_geometric(flighted, decay = 0.6, normalise = FALSE)
  # After two dark periods the carried value should still be strictly
  # positive (decaying, not reset), and strictly less than the peak.
  expect_gt(out[4], 0)
  expect_lt(out[4], out[2])
  # Hand check for the first four periods:
  # raw[1]=100; raw[2]=100+0.6*100=160; raw[3]=0.6*160=96; raw[4]=0.6*96=57.6
  expect_equal(out[1:4], c(100, 160, 96, 57.6), tolerance = 1e-10)
})

# ---- 6. Golden values -----------------------------------------------------------

test_that("golden values: geometric adstock on an isolated impulse", {
  # adstock_geometric(c(100,0,0), decay=0.5, normalise=FALSE) == c(100,50,25)
  expect_equal(adstock_geometric(c(100, 0, 0), decay = 0.5, normalise = FALSE),
               c(100, 50, 25), tolerance = 1e-12)
  # Normalised is that times (1 - decay).
  expect_equal(adstock_geometric(c(100, 0, 0), decay = 0.5, normalise = TRUE),
               c(100, 50, 25) * 0.5, tolerance = 1e-12)

  # A different decay, same shape: theta^i * 100.
  expect_equal(
    adstock_geometric(c(100, 0, 0, 0), decay = 0.25, normalise = FALSE),
    100 * 0.25^(0:3), tolerance = 1e-12
  )
  expect_equal(
    adstock_geometric(c(100, 0, 0, 0), decay = 0.25, normalise = TRUE),
    100 * 0.25^(0:3) * (1 - 0.25), tolerance = 1e-12
  )
})

test_that("golden values: finite max_lag truncates the kernel exactly", {
  # max_lag = 2 means out[t] = x[t] + 0.5 * x[t-1], nothing further back.
  x <- c(100, 0, 0, 0)
  expect_equal(adstock_geometric(x, decay = 0.5, max_lag = 2, normalise = FALSE),
               c(100, 50, 0, 0), tolerance = 1e-12)
  # Normalised: weights c(1, 0.5) sum to 1.5.
  expect_equal(adstock_geometric(x, decay = 0.5, max_lag = 2, normalise = TRUE),
               c(100, 50, 0, 0) / 1.5, tolerance = 1e-12)
})

test_that("golden values: adstock_filter applies the supplied kernel literally", {
  # out[t] = 0.2*x[t] + 0.5*x[t-1] + 0.3*x[t-2]
  out <- adstock_filter(c(100, 0, 0, 0, 0), weights = c(0.2, 0.5, 0.3))
  expect_equal(out, c(20, 50, 30, 0, 0), tolerance = 1e-12)
})

test_that("golden values: adstock_weights_weibull matches the hand-derived cdf kernel", {
  # Independent derivation from the Weibull cdf F(t) = 1 - exp(-(t/scale)^shape):
  # w_i = prod_{j<i} (1 - F(j)), normalised to sum to 1. Computed here from the
  # raw formula, not by calling the function under test.
  F <- function(t) 1 - exp(-(t / 3)^2)
  surv <- 1 - F(1:7)
  raw <- cumprod(c(1, surv))
  hand <- raw / sum(raw)

  w <- adstock_weights_weibull(8, shape = 2, scale = 3, type = "cdf")
  expect_equal(w, hand, tolerance = 1e-8)
  expect_equal(round(w, 4), c(0.3680, 0.3293, 0.2111, 0.0777, 0.0131, 0.0008,
                              0.0000, 0.0000), tolerance = 1e-4)
})

test_that("golden values: by= grouping is equivalent to running each group alone", {
  spend_panel <- c(100, 50, 25, 200, 100, 50)
  geo <- c("north", "north", "north", "south", "south", "south")
  grouped <- adstock_geometric(spend_panel, decay = 0.5, by = geo)
  expect_equal(grouped[1:3], adstock_geometric(spend_panel[1:3], decay = 0.5),
               tolerance = 1e-12)
  expect_equal(grouped[4:6], adstock_geometric(spend_panel[4:6], decay = 0.5),
               tolerance = 1e-12)
})

# ---- 7. Level preservation -------------------------------------------------------

test_that("normalised adstock of a constant series returns that constant after burn-in", {
  const <- rep(10, 60)
  out <- adstock_geometric(const, decay = 0.8, normalise = TRUE)
  expect_equal(tail(out, 1), 10, tolerance = 1e-4)
  expect_equal(tail(out, 5), rep(10, 5), tolerance = 1e-4)
})

test_that("unnormalised adstock of a constant series converges to c / (1 - decay)", {
  const <- rep(10, 60)
  out <- adstock_geometric(const, decay = 0.8, normalise = FALSE)
  expect_equal(tail(out, 1), 10 / (1 - 0.8), tolerance = 1e-3)
})

test_that("the steady state is an exact fixed point of the recursion", {
  # If raw carryover is already sitting at the steady state c/(1-decay), one
  # more period of constant spend c must reproduce that same steady state
  # exactly (no burn-in needed): this checks the recursion algebraically
  # rather than only approximately via convergence.
  c_val <- 10; decay <- 0.8
  steady <- c_val / (1 - decay)
  out_unnorm <- adstock_geometric(rep(c_val, 5), decay = decay, state = steady,
                                  normalise = FALSE)
  expect_equal(out_unnorm, rep(steady, 5), tolerance = 1e-10)

  out_norm <- adstock_geometric(rep(c_val, 5), decay = decay, state = steady,
                                normalise = TRUE)
  expect_equal(out_norm, rep(c_val, 5), tolerance = 1e-10)
})

# ---- 8. Input validation ----------------------------------------------------------

test_that("adstock_geometric() rejects decay outside [0, 1]", {
  expect_error(adstock_geometric(c(1, 2), decay = -0.1), class = "rlang_error")
  expect_error(adstock_geometric(c(1, 2), decay = 1.1), class = "rlang_error")
})

test_that("decay = 1 with max_lag = Inf and normalise = TRUE is a deliberate error", {
  expect_error(
    adstock_geometric(c(1, 2, 3), decay = 1, max_lag = Inf, normalise = TRUE),
    class = "rlang_error",
    regexp = "infinite mass"
  )
  # ...but is fine when normalise = FALSE (raw Koyck accumulation, decay = 1
  # is a plain running sum) or when max_lag is finite.
  expect_silent(adstock_geometric(c(1, 2, 3), decay = 1, max_lag = Inf,
                                  normalise = FALSE))
  expect_silent(adstock_geometric(c(1, 2, 3), decay = 1, max_lag = 2,
                                  normalise = TRUE))
})

test_that("NA in x errors by default and is handled by na_action = 'zero'", {
  x <- c(1, NA, 2)
  expect_error(adstock_geometric(x, decay = 0.5), class = "rlang_error",
               regexp = "missing value")
  out <- adstock_geometric(x, decay = 0.5, na_action = "zero")
  expect_equal(out, adstock_geometric(c(1, 0, 2), decay = 0.5), tolerance = 1e-12)
  expect_false(anyNA(out))
})

test_that("na_action = 'keep' lets NA propagate through the recursive filter", {
  out <- adstock_geometric(c(1, NA, 2), decay = 0.5, na_action = "keep")
  expect_equal(out[1], 0.5, tolerance = 1e-12)
  expect_true(is.na(out[2]))
  expect_true(is.na(out[3]))  # poisoned by the NA one step earlier
})

test_that("adstock_weibull() requires max_lag and has no infinite form", {
  expect_error(adstock_weibull(c(1, 2, 3), shape = 2, scale = 2),
               class = "rlang_error", regexp = "max_lag")
})

test_that("max_lag must be a positive whole number (or Inf where allowed)", {
  expect_error(adstock_geometric(c(1, 2), decay = 0.5, max_lag = 0),
               class = "rlang_error")
  expect_error(adstock_geometric(c(1, 2), decay = 0.5, max_lag = 2.5),
               class = "rlang_error")
  expect_error(adstock_weibull(c(1, 2), shape = 1, scale = 1, max_lag = Inf),
               class = "rlang_error")
})

test_that("adstock_weights() rejects a non-logical normalise argument", {
  expect_error(adstock_weights(4, 0.5, normalise = "yes"), class = "rlang_error")
})

test_that("state of the wrong length is rejected with a helpful message", {
  expect_error(
    adstock_filter(1:5, weights = c(0.5, 0.3), state = c(1, 2, 3)),
    class = "rlang_error",
    regexp = "wrong length"
  )
})
