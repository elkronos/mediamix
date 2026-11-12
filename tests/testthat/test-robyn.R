# Tests for from_robyn() (R/robyn.R).
#
# Robyn's transformations are re-created below from R/R/transformation.R in
# github.com/facebookexperimental/Robyn (MIT licence), main branch at commit
# 5915aa9, read on 12 November 2026. The file last changed in commit 33baf19
# (June 2025). The "range" inflexion point is the same file at release 3.11.1.
# Robyn's camelCase names are written in snake_case (scaleTrans is
# scale_trans, thetaVecCum is theta_vec_cum), and its dplyr::lag() is written
# out with base R.

robyn_geometric <- function(x, theta) {
  x_decayed <- c(x[1], rep(0, length(x) - 1))
  for (xi in 2:length(x_decayed)) {
    x_decayed[xi] <- x[xi] + theta * x_decayed[xi - 1]
  }
  x_decayed
}

robyn_weibull <- function(x, shape, scale, type) {
  windlen <- length(x)
  x_bin <- 1:windlen
  scale_trans <- round(quantile(1:windlen, scale), 0)
  if (shape == 0 || scale == 0) return(x)
  if (type == "pdf") {
    d <- dweibull(x_bin, shape = shape, scale = scale_trans)
    theta_vec_cum <- if (diff(range(d)) == 0) {
      c(1, rep(0, length(d) - 1))
    } else {
      (d - min(d)) / (max(d) - min(d))
    }
  } else {
    theta_vec <- c(1, 1 - pweibull(head(x_bin, -1), shape = shape,
                                   scale = scale_trans))
    theta_vec_cum <- cumprod(theta_vec)
  }
  x_decayed <- mapply(function(x_val, x_pos) {
    x_vec <- c(rep(0, x_pos - 1), rep(x_val, windlen - x_pos + 1))
    lagged <- c(rep(0, x_pos - 1), theta_vec_cum)[seq_len(windlen)]
    x_vec * lagged
  }, x_val = x, x_pos = seq_along(x))
  rowSums(x_decayed)[seq_along(x)]
}

robyn_hill <- function(x, alpha, gamma, inflexion = "max") {
  inflexion <- if (inflexion == "max") {
    max(x) * gamma
  } else {
    c(range(x) %*% c(1 - gamma, gamma))
  }
  x**alpha / (x**alpha + inflexion**alpha)
}

# Flighted weekly spend with dark weeks, the first of them at the start.
robyn_spend <- function(n = 60) {
  set.seed(42)
  on <- stats::rbinom(n, 1, 0.7)
  on[1] <- 0
  round(on * pmax(0, stats::rnorm(n, 300, 150)))
}

transform_with <- function(x, args) {
  media_transform(x, adstock = args$adstock, saturation = args$saturation)
}

# ---- geometric adstock -------------------------------------------------------

test_that("theta converts to an unnormalised geometric decay", {
  x <- robyn_spend()
  args <- from_robyn(x, theta = 0.45)
  expect_identical(args$adstock,
                   list(kernel = "geometric", decay = 0.45, normalise = FALSE))
  expect_identical(args$saturation, list(type = "none"))
  expect_equal(transform_with(x, args), robyn_geometric(x, 0.45),
               tolerance = 1e-12)
})

test_that("theta converts without the media series", {
  expect_identical(from_robyn(theta = 0.2)$adstock,
                   list(kernel = "geometric", decay = 0.2, normalise = FALSE))
  expect_identical(from_robyn(theta = 0)$adstock$decay, 0)
})

test_that("geometric adstock with Hill saturation reproduces Robyn's series", {
  x <- robyn_spend()
  window <- 9:60
  for (theta in c(0, 0.3, 0.8)) {
    args <- from_robyn(x, theta = theta, alpha = 2.3, gamma = 0.62,
                       window = window)
    adstocked <- robyn_geometric(x, theta)[window]
    expect_equal(transform_with(x, args)[window],
                 robyn_hill(adstocked, alpha = 2.3, gamma = 0.62),
                 tolerance = 1e-10, info = paste("theta", theta))
    expect_equal(args$saturation$half_max, max(adstocked) * 0.62,
                 tolerance = 1e-12)
    expect_identical(args$saturation$shape, 2.3)
    expect_identical(args$saturation$type, "hill")
  }
})

test_that("the converted lists drop straight into media_transform()", {
  x <- robyn_spend()
  args <- from_robyn(x, theta = 0.3, alpha = 1.5, gamma = 0.5)
  expect_named(args, c("adstock", "saturation"))
  expect_equal(do.call(media_transform, c(list(x), args)),
               transform_with(x, args))
})

# ---- the Hill inflexion point ------------------------------------------------

test_that("the range inflexion reproduces Robyn 3.11.1", {
  # Always-on spend keeps the adstocked series above 0, so the two
  # conventions give different inflexion points.
  x <- round(400 + 150 * sin(seq_len(52) / 3))
  args <- from_robyn(x, theta = 0.3, alpha = 1.4, gamma = 0.4,
                     inflexion = "range")
  expect_equal(transform_with(x, args),
               robyn_hill(robyn_geometric(x, 0.3), alpha = 1.4, gamma = 0.4,
                          inflexion = "range"),
               tolerance = 1e-10)
  current <- from_robyn(x, theta = 0.3, alpha = 1.4, gamma = 0.4)
  expect_gt(args$saturation$half_max, current$saturation$half_max)
  expect_equal(transform_with(x, current),
               robyn_hill(robyn_geometric(x, 0.3), alpha = 1.4, gamma = 0.4),
               tolerance = 1e-10)
})

test_that("the two inflexion conventions agree when the window reaches 0", {
  x <- robyn_spend()
  expect_identical(x[1], 0)
  a <- from_robyn(x, theta = 0.5, alpha = 2, gamma = 0.3, window = 1:30)
  b <- from_robyn(x, theta = 0.5, alpha = 2, gamma = 0.3, window = 1:30,
                  inflexion = "range")
  expect_equal(a$saturation$half_max, b$saturation$half_max,
               tolerance = 1e-12)
})

test_that("only rows inside the window set the inflexion point", {
  x <- robyn_spend()
  x[60] <- 50000
  inside <- from_robyn(x, theta = 0, alpha = 1, gamma = 0.5)
  outside <- from_robyn(x, theta = 0, alpha = 1, gamma = 0.5, window = 1:59)
  expect_equal(inside$saturation$half_max, 25000)
  expect_equal(outside$saturation$half_max, max(x[1:59]) / 2)
})

test_that("gamma = 1 puts the inflexion point at the window's maximum", {
  x <- robyn_spend()
  args <- from_robyn(x, theta = 0.2, alpha = 3, gamma = 1)
  expect_equal(args$saturation$half_max, max(robyn_geometric(x, 0.2)),
               tolerance = 1e-12)
})

test_that("a one-row series converts as Robyn transforms it", {
  args <- from_robyn(5, theta = 0.5, alpha = 1, gamma = 1)
  expect_equal(transform_with(5, args), 0.5)
  w <- from_robyn(5, adstock = "weibull_cdf", shape = 2, scale = 0.5)
  expect_identical(w$adstock$max_lag, 1L)
  expect_equal(transform_with(5, w), 5)
})

# ---- Weibull adstock ---------------------------------------------------------

test_that("the Weibull CDF reproduces Robyn's adstock", {
  x <- robyn_spend()
  cases <- list(c(0.5, 0.05), c(1.3, 0.08), c(2.5, 0.2), c(1, 1),
                c(8, 0.02))
  for (p in cases) {
    args <- from_robyn(x, adstock = "weibull_cdf", shape = p[1], scale = p[2])
    expect_equal(transform_with(x, args),
                 robyn_weibull(x, p[1], p[2], type = "cdf"),
                 tolerance = 1e-10,
                 info = paste("shape", p[1], "scale", p[2]))
  }
})

test_that("the Weibull CDF with Hill saturation reproduces Robyn's series", {
  x <- robyn_spend()
  window <- 5:56
  args <- from_robyn(x, adstock = "weibull_cdf", shape = 1.3, scale = 0.08,
                     alpha = 1.8, gamma = 0.7, window = window)
  adstocked <- robyn_weibull(x, 1.3, 0.08, type = "cdf")[window]
  expect_equal(transform_with(x, args)[window],
               robyn_hill(adstocked, alpha = 1.8, gamma = 0.7),
               tolerance = 1e-10)
})

test_that("Weibull scale converts to Robyn's rounded quantile of the length", {
  args <- from_robyn(robyn_spend(), adstock = "weibull_cdf", shape = 1.3,
                     scale = 0.08)
  # The 8% quantile of 1 to 60 is 5.72, which rounds to 6.
  expect_identical(args$adstock$scale, 6)
  expect_identical(args$adstock$max_lag, 60L)
  expect_identical(args$adstock[c("kernel", "type", "normalise")],
                   list(kernel = "weibull", type = "cdf", normalise = FALSE))
  # 1 + 0.375 * 4 = 2.5, which round() takes to the even neighbour, as in
  # Robyn.
  half <- from_robyn(rep(1, 5), adstock = "weibull_cdf", shape = 1,
                     scale = 0.375)
  expect_identical(half$adstock$scale, 2)
  # The same scale fitted on a longer series is a longer carryover.
  long <- from_robyn(rep(1, 101), adstock = "weibull_cdf", shape = 1,
                     scale = 0.375)
  expect_identical(long$adstock$scale, 38)
})

test_that("a Weibull shape or scale of 0 switches carryover off, as in Robyn", {
  x <- robyn_spend()
  for (p in list(c(0, 0.1), c(1.5, 0))) {
    args <- from_robyn(x, adstock = "weibull_cdf", shape = p[1], scale = p[2],
                       alpha = 1, gamma = 0.5)
    expect_identical(args$adstock, list(kernel = "none"))
    expect_equal(args$saturation$half_max, max(x) * 0.5)
    expect_equal(transform_with(x, args),
                 robyn_hill(robyn_weibull(x, p[1], p[2], type = "cdf"),
                            alpha = 1, gamma = 0.5),
                 tolerance = 1e-10)
  }
})

test_that("the Weibull PDF is refused, and adstock_filter() reproduces it", {
  x <- robyn_spend()
  expect_error(from_robyn(x, adstock = "weibull_pdf", shape = 0.6, scale = 0.3),
               "no exact mediamix equivalent")
  # Robyn rescales the density to run from 0 to 1, which adstock_weibull()
  # does not. With a heavy tail the smallest weight is far from 0 and the
  # series differ by several per cent; adstock_filter() with Robyn's kernel
  # matches it.
  periods <- round(stats::quantile(seq_along(x), 0.3, names = FALSE))
  d <- stats::dweibull(seq_along(x), shape = 0.6, scale = periods)
  kernel <- (d - min(d)) / (max(d) - min(d))
  robyn <- robyn_weibull(x, 0.6, 0.3, type = "pdf")
  ours <- adstock_weibull(x, shape = 0.6, scale = periods,
                          max_lag = length(x), type = "pdf",
                          normalise = FALSE)
  expect_gt(max(abs(ours / robyn - 1)[robyn > 0]), 0.01)
  expect_equal(adstock_filter(x, weights = kernel), robyn, tolerance = 1e-10)
})

# ---- invalid input -----------------------------------------------------------

test_that("adstock parameters must match the adstock type", {
  x <- robyn_spend()
  expect_error(from_robyn(x), "needs `theta`")
  expect_error(from_robyn(x, theta = 0.3, shape = 1, scale = 0.1),
               "Weibull adstock parameters")
  expect_error(from_robyn(x, theta = 0.3, shape = 1), "`adstock` is")
  expect_error(from_robyn(x, adstock = "weibull_cdf", theta = 0.3,
                          shape = 1, scale = 0.1),
               "geometric adstock parameter")
  expect_error(from_robyn(x, adstock = "weibull_cdf", shape = 1),
               "both `shape` and `scale`")
  expect_error(from_robyn(x, adstock = "weibull_cdf", scale = 0.1),
               "both `shape` and `scale`")
  expect_error(from_robyn(x, adstock = "weibull", shape = 1, scale = 0.1),
               "should be one of")
})

test_that("hyperparameters outside Robyn's limits are refused", {
  x <- robyn_spend()
  expect_error(from_robyn(x, theta = 1), "\\[0, 1\\)")
  expect_error(from_robyn(x, theta = -0.1), "`theta`")
  expect_error(from_robyn(x, theta = c(0.1, 0.2)), "single")
  expect_error(from_robyn(x, theta = "0.3"), "single")
  expect_error(from_robyn(x, theta = NA_real_), "single")
  expect_error(from_robyn(x, adstock = "weibull_cdf", shape = -1, scale = 0.1),
               "`shape`")
  expect_error(from_robyn(x, adstock = "weibull_cdf", shape = 1, scale = 1.5),
               "`scale`")
  expect_error(from_robyn(x, theta = 0.3, alpha = 0, gamma = 0.5), "`alpha`")
  expect_error(from_robyn(x, theta = 0.3, alpha = 2, gamma = 0), "`gamma`")
  expect_error(from_robyn(x, theta = 0.3, alpha = 2, gamma = 1.2), "`gamma`")
})

test_that("alpha and gamma come together", {
  x <- robyn_spend()
  expect_error(from_robyn(x, theta = 0.3, alpha = 2), "together")
  expect_error(from_robyn(x, theta = 0.3, gamma = 0.5), "together")
})

test_that("the series is required where the conversion depends on it", {
  expect_error(from_robyn(theta = 0.3, alpha = 2, gamma = 0.5),
               "needed to convert `gamma`")
  expect_error(from_robyn(adstock = "weibull_cdf", shape = 1, scale = 0.1),
               "needed to convert a Weibull `scale`")
})

test_that("the media series must be complete and non-negative", {
  expect_error(from_robyn(c(1, NA, 3), theta = 0.3), "missing")
  expect_error(from_robyn(c(1, Inf, 3), theta = 0.3), "finite")
  expect_error(from_robyn(c(1, -2, 3), theta = 0.3), "negative")
  expect_error(from_robyn(numeric(0), theta = 0.3), "at least one")
  expect_error(from_robyn("a", theta = 0.3), "numeric vector")
})

test_that("window must hold row positions of x", {
  x <- robyn_spend(10)
  hill <- function(window) {
    from_robyn(x, theta = 0.3, alpha = 2, gamma = 0.5, window = window)
  }
  expect_error(hill(0:3), "row positions")
  expect_error(hill(c(1, 11)), "from 1 to 10")
  expect_error(hill(c(1.5, 2)), "whole numbers")
  expect_error(hill(c(1, NA)), "row positions")
  expect_error(hill(integer(0)), "row positions")
  expect_error(hill(rep(TRUE, 10)), "row positions")
  expect_error(hill("1"), "row positions")
})

test_that("an all-zero window has no inflexion point", {
  expect_error(from_robyn(c(0, 0, 0, 5), theta = 0.3, alpha = 2, gamma = 0.5,
                          window = 1:3),
               "inflexion point is 0")
})
