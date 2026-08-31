# Tests for R/contributions.R: contributions(), roi(), mroi(),
# response_curve(), spend_for().

test_that("contributions() sums exactly to intercept + sum(beta_j * x_j)", {
  media <- data.frame(tv = c(10, 20, 30), search = c(5, 15, 25))
  coefs <- c("(Intercept)" = 100, tv = 2, search = 3)

  out <- contributions(media, coefs)
  totals <- as.numeric(tapply(out$contribution, out$period, sum))
  expected <- 100 + 2 * media$tv + 3 * media$search
  expect_equal(totals, expected)

  # explicit intercept overrides a missing one
  coefs_no_int <- c(tv = 2, search = 3)
  out2 <- contributions(media, coefs_no_int, intercept = 50)
  totals2 <- as.numeric(tapply(out2$contribution, out2$period, sum))
  expect_equal(totals2, 50 + 2 * media$tv + 3 * media$search)

  # a fitted lm() works too, via stats::coef()
  set.seed(1)
  n <- 40
  df <- data.frame(tv = runif(n, 0, 100), search = runif(n, 0, 50))
  df$revenue <- 10 + 2 * df$tv + 3 * df$search + rnorm(n, 0, 0.01)
  fit <- stats::lm(revenue ~ tv + search, data = df)
  out3 <- contributions(df[c("tv", "search")], fit)
  totals3 <- as.numeric(tapply(out3$contribution, out3$period, sum))
  expect_equal(totals3, stats::predict(fit), tolerance = 1e-8, ignore_attr = TRUE)
})

test_that("contributions() errors when no media column matches a model coefficient", {
  media <- data.frame(radio = c(1, 2, 3), print = c(4, 5, 6))
  coefs <- c("(Intercept)" = 100, tv = 2, search = 3)
  expect_error(contributions(media, coefs), "No column of.*matches a model coefficient")
})

test_that("a named numeric vector works in place of a fitted model", {
  media <- data.frame(tv = c(1, 2, 3))
  coefs <- c("(Intercept)" = 5, tv = 10)
  out <- contributions(media, coefs)
  expect_setequal(out$channel, c("tv", "(baseline)"))
  tv_contrib <- out$contribution[out$channel == "tv"]
  expect_equal(tv_contrib, 10 * media$tv)
})

test_that("contributions() reports a term the model has but the media data lacks, without breaking the sum", {
  media <- data.frame(tv = c(1, 2, 3))
  coefs <- c("(Intercept)" = 5, tv = 10, price = -1)
  expect_message(contributions(media, coefs), "not decomposed|not among the media")
  out <- suppressMessages(contributions(media, coefs))
  # baseline absorbs the intercept only; price's effect is simply not decomposed
  expect_setequal(out$channel, c("tv", "(baseline)"))
})

# ---- roi() / mroi(): marginal ROI strictly below average ROI ----------------

test_that("roi() computes contribution / spend per channel", {
  contrib <- data.frame(channel = c("tv", "search", "(baseline)"),
                         contribution = c(500, 300, 1000))
  spend <- data.frame(tv = c(50, 50), search = c(30, 30))
  r <- roi(contrib, spend)

  expect_false("(baseline)" %in% r$channel)
  expect_equal(r$roi[r$channel == "tv"], 500 / 100)
  expect_equal(r$roi[r$channel == "search"], 300 / 60)
  # ordered by descending roi
  expect_true(all(diff(r$roi) <= 0))

  expect_error(roi(data.frame(x = 1), spend), "channel.*contribution")
  expect_error(roi(c(tv = 100), 1), "must be named")
})

test_that("marginal ROI is strictly less than average ROI under a saturating curve", {
  coefficient <- 5000
  half_max <- 100
  shape <- 1  # concave everywhere, so mroi < roi holds unambiguously
  spend_level <- 150

  avg_roi <- coefficient * saturate_hill(spend_level, half_max = half_max,
                                         shape = shape) / spend_level
  marginal <- mroi(spend_level = spend_level, coefficient = coefficient,
                    half_max = half_max, shape = shape)

  expect_lt(marginal, avg_roi)
  expect_gt(marginal, 0)
})

test_that("response_curve()'s marginal column is decreasing for a concave (shape = 1) curve", {
  coefficient <- 5000
  half_max <- 100
  rc <- response_curve(spend = seq(10, 500, by = 10), coefficient = coefficient,
                        half_max = half_max, shape = 1)

  expect_identical(names(rc), c("spend", "response", "marginal"))
  expect_true(all(diff(rc$marginal) <= 1e-9))
  expect_true(all(diff(rc$response) >= 0))  # response itself keeps rising
})

test_that("spend_for() round-trips against response_curve()/saturate_hill()", {
  coefficient <- 5000
  half_max <- 100
  shape <- 1
  target_spend <- 120
  target_resp <- coefficient * saturate_hill(target_spend, half_max = half_max,
                                             shape = shape)

  back <- spend_for(target = target_resp, coefficient = coefficient,
                     half_max = half_max, shape = shape)
  expect_equal(back, target_spend, tolerance = 1e-6)
})

test_that("spend_for() returns NA with a warning when the target is unreachable", {
  coefficient <- 5000
  half_max <- 100
  shape <- 1
  # hill is bounded by `coefficient`; ask for far more than that ceiling
  expect_warning(
    spend_for(target = 1e9, coefficient = coefficient, half_max = half_max,
              shape = shape, max_spend = 1000),
    "not reachable"
  )
  val <- suppressWarnings(
    spend_for(target = 1e9, coefficient = coefficient, half_max = half_max,
              shape = shape, max_spend = 1000)
  )
  expect_true(is.na(val))
})
