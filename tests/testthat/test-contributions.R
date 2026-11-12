# Tests for R/contributions.R: contributions(), roi(), mroi(),
# response_curve(), spend_for(); and for the argument checks of
# marginal_roi() in R/marginal-roi.R.

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

# ---- contributions(): which prediction the baseline comes from ---------------

contrib_fixture <- function() {
  data(mm_weekly, package = "mediamix", envir = environment())
  north <- mm_weekly[mm_weekly$geo == "north", ]
  media <- data.frame(tv = adstock_geometric(north$tv, 0.8),
                      search = adstock_geometric(north$search, 0.3))
  controls <- data.frame(week = seq_len(nrow(north)), price = north$price)
  list(north = north, media = media, controls = controls,
       data = cbind(revenue = north$revenue, media, controls))
}

period_totals <- function(x) as.numeric(tapply(x$contribution, x$period, sum))

test_that("a period the model was not fitted on never gets the training fit", {
  fx <- contrib_fixture()
  first <- 1:78
  second <- 79:156
  fit <- stats::lm(revenue ~ ., data = fx$data[first, ])
  holdout <- fx$data[second, ]

  # Same number of rows as the fit, but other weeks. Without the controls the
  # prediction cannot be made, so the baseline is the intercept, with a message.
  expect_message(no_controls <- contributions(fx$media[second, ], fit),
                 "does not hold the rows")
  expect_equal(no_controls$contribution[no_controls$channel == "(baseline)"],
               rep(unname(stats::coef(fit)[["(Intercept)"]]), 78))

  with_newdata <- contributions(fx$media[second, ], fit, newdata = holdout)
  expect_equal(period_totals(with_newdata),
               unname(stats::predict(fit, newdata = holdout)))

  # A model of the media alone can predict from `media` itself.
  media_only <- stats::lm(fx$north$revenue[first] ~ ., data = fx$media[first, ])
  own <- contributions(fx$media[second, ], media_only)
  expect_equal(period_totals(own),
               unname(stats::predict(media_only, newdata = fx$media[second, ])))
})

test_that("newdata is checked against media and the model", {
  fx <- contrib_fixture()
  fit <- stats::lm(revenue ~ ., data = fx$data)
  other <- fx$data
  other$tv <- fx$north$tv
  expect_error(contributions(fx$media, fit, newdata = other),
               "different values")
  expect_error(contributions(fx$media, fit, newdata = fx$data[1:5, ]),
               "one row per row")
  expect_error(contributions(fx$media, stats::coef(fit), newdata = fx$data),
               "fitted model")
})

test_that("rows dropped for a missing KPI keep the model's other terms", {
  fx <- contrib_fixture()
  d <- fx$data
  d$revenue[c(10, 40)] <- NA
  fit <- stats::lm(revenue ~ tv + search + price, data = d)
  expect_message(out <- contributions(fx$media, fit), "left out of the fit")
  totals <- period_totals(out)
  pred <- unname(stats::predict(fit, newdata = d))
  expect_equal(totals[-c(10, 40)], pred[-c(10, 40)])
  expect_true(all(is.na(totals[c(10, 40)])))

  excluded <- stats::lm(revenue ~ tv + search + price, data = d,
                        na.action = stats::na.exclude)
  expect_equal(suppressMessages(contributions(fx$media, excluded)),
               suppressMessages(contributions(fx$media, fit)))

  everything <- contributions(fx$media, fit, newdata = d)
  expect_equal(period_totals(everything), pred)
})

test_that("a back-ticked coefficient is matched, and spare columns are reported", {
  fx <- contrib_fixture()
  d <- data.frame(revenue = fx$north$revenue, `TV Spend` = fx$media$tv,
                  search = fx$media$search, check.names = FALSE)
  fit <- stats::lm(revenue ~ `TV Spend` + search, data = d)
  out <- contributions(d[c("TV Spend", "search")], fit)
  expect_setequal(unique(out$channel), c("TV Spend", "search", "(baseline)"))
  expect_equal(period_totals(out), unname(stats::fitted(fit)))

  logged <- stats::lm(revenue ~ log1p(`TV Spend`) + search, data = d)
  expect_message(contributions(d[c("TV Spend", "search")], logged),
                 "match.*no model coefficient")
})

test_that("a non-identity link warns and keeps one scale", {
  fx <- contrib_fixture()
  counts <- round(fx$north$revenue / 100)
  fit <- stats::glm(counts ~ ., data = cbind(fx$media, fx$controls),
                    family = stats::poisson())
  expect_warning(out <- contributions(fx$media, fit), "link")
  expect_equal(period_totals(out), unname(fit$linear.predictors))
  gaussian <- stats::glm(fx$north$revenue ~ .,
                         data = cbind(fx$media, fx$controls))
  expect_no_warning(contributions(fx$media, gaussian))
})

test_that("one row, intercept handling and argument messages", {
  fx <- contrib_fixture()
  fit <- stats::lm(fx$north$revenue ~ ., data = fx$media)
  one <- contributions(fx$media[1, ], fit)
  expect_equal(sum(one$contribution),
               unname(stats::predict(fit, newdata = fx$media[1, ])))

  expect_message(contributions(fx$media, fit, intercept = 5), "not used")
  expect_message(contributions(data.frame(tv = 1:3), c(tv = 2)),
                 "intercept is taken as 0")
  expect_error(contributions(fx$media, fit, by = 1:3), "row of `media`")
  err <- tryCatch(contributions(fx$media, "not a model"), error = identity)
  expect_identical(conditionCall(err)[[1]], as.name("contributions"))
})

test_that("mixed models are decomposed with their fixed effects", {
  skip_if_not_installed("nlme")
  lme <- getExportedValue("nlme", "lme")
  fixef <- getExportedValue("nlme", "fixef")
  data(mm_weekly, package = "mediamix", envir = environment())
  d <- mm_weekly
  d$tv_t <- adstock_geometric(d$tv, 0.8, by = d$geo)
  fit <- lme(revenue ~ tv_t + search + price, random = ~ 1 | geo, data = d)
  out <- suppressMessages(contributions(d[c("tv_t", "search")], fit))
  expect_equal(out$contribution[out$channel == "tv_t"],
               fixef(fit)[["tv_t"]] * d$tv_t)
  expect_equal(period_totals(out), as.numeric(stats::fitted(fit)))
})

# ---- roi(): spend is read as numbers over the same periods -------------------

test_that("roi() reads factor spend by its labels and rejects text", {
  contrib <- data.frame(channel = c("tv", "search"), contribution = c(500, 300))
  spend <- data.frame(tv = factor(c(50, 50)), search = c(30, 30),
                      date = as.Date(c("2024-01-01", "2024-01-08")))
  expect_message(r <- roi(contrib, spend), "date")
  expect_equal(r$roi[r$channel == "tv"], 5)
  expect_error(roi(contrib, data.frame(tv = c("1,000", "2"))),
               "could not be read as a number")
  expect_error(roi(contrib, c(tv = "a")), "numeric")
})

test_that("roi() warns when contributions and spend cover different periods", {
  fx <- contrib_fixture()
  fit <- stats::lm(fx$north$revenue ~ ., data = fx$media)
  contrib <- contributions(fx$media, fit)
  spend <- fx$north[c("tv", "search")]
  expect_no_warning(roi(contrib, spend))
  expect_warning(roi(contrib, rbind(spend, spend)), "156 periods")
})

# ---- roi(by =, index =, periods =) ----------------------------------------------

# Two geographies of twelve weeks. The regressors are transforms of spend, so
# each group's ROI differs from the others.
roi_panel <- function() {
  set.seed(21)
  n <- 12
  d <- data.frame(geo = rep(c("east", "west"), each = n),
                  week = rep(as.Date("2024-01-01") + 7 * (0:(n - 1)), 2),
                  tv = round(runif(2 * n, 100, 900)),
                  search = round(runif(2 * n, 50, 300)))
  media <- data.frame(tv = sqrt(d$tv), search = log1p(d$search))
  coefs <- c("(Intercept)" = 40, tv = 3, search = 7)
  list(d = d, media = media, coefs = coefs,
       contrib = contributions(media, coefs, index = d$week, by = d$geo))
}

roi_by_hand <- function(p, rows, ch) {
  sum(p$coefs[[ch]] * p$media[[ch]][rows]) / sum(p$d[[ch]][rows])
}

test_that("roi(by =) gives one row per channel per group", {
  p <- roi_panel()
  r <- roi(p$contrib, p$d[c("tv", "search")], by = p$d$geo)
  expect_identical(names(r), c("group", "channel", "contribution", "spend",
                               "roi"))
  expect_identical(r$group, c("east", "east", "west", "west"))
  for (i in seq_len(nrow(r))) {
    rows <- which(p$d$geo == r$group[i])
    expect_equal(r$roi[i], roi_by_hand(p, rows, r$channel[i]))
    expect_equal(r$spend[i], sum(p$d[[r$channel[i]]][rows]))
  }
  # Channels are in descending ROI within each group.
  expect_true(all(diff(r$roi[r$group == "east"]) <= 0))
  expect_true(all(diff(r$roi[r$group == "west"]) <= 0))

  # A column of spend, a factor and a data frame of keys name the same groups.
  expect_equal(roi(p$contrib, p$d[c("geo", "tv", "search")], by = "geo"), r)
  expect_equal(roi(p$contrib, p$d[c("tv", "search")], by = factor(p$d$geo)),
               r)
  keyed <- contributions(p$media, p$coefs, by = p$d["geo"])
  expect_equal(roi(keyed, p$d[c("tv", "search")], by = p$d["geo"]), r)

  # Without by, the groups are pooled as before.
  pooled <- roi(p$contrib, p$d[c("tv", "search")])
  expect_identical(names(pooled), c("channel", "contribution", "spend", "roi"))
  expect_equal(pooled$roi[pooled$channel == "tv"],
               roi_by_hand(p, seq_len(nrow(p$d)), "tv"))
})

test_that("groups found in only one input are reported and left out", {
  p <- roi_panel()
  extra <- rbind(p$d, transform(p$d[1:3, ], geo = "north"))
  expect_message(r <- roi(p$contrib, extra[c("tv", "search")], by = extra$geo),
                 "north")
  expect_setequal(r$group, c("east", "west"))
  expect_error(roi(p$contrib, p$d[c("tv", "search")],
                   by = rep("north", nrow(p$d))),
               "No group appears in both")
  # A group short of rows is caught when periods are not matched.
  expect_warning(roi(p$contrib, p$d[-1, c("tv", "search")], by = p$d$geo[-1]),
                 "east \\(12 periods, 11 rows\\)")
})

test_that("index matches spend to contributions period by period", {
  p <- roi_panel()
  spend <- p$d[c("week", "geo", "tv", "search")]
  later <- transform(spend, week = week + 7 * 12)
  r <- roi(p$contrib, spend, by = "geo", index = "week")
  expect_equal(r, roi(p$contrib, p$d[c("tv", "search")], by = p$d$geo))
  # Periods with no contributions are left out, and row order does not matter.
  both <- rbind(later, spend)[sample(2 * nrow(spend)), ]
  expect_message(shuffled <- roi(p$contrib, both, by = "geo", index = "week"),
                 "12 periods of `spend` are not in `contributions`")
  expect_equal(shuffled, r)
  # The column used as index is not taken for a channel.
  expect_no_message(roi(p$contrib, spend, by = "geo", index = "week"))
  expect_error(roi(p$contrib, later, by = "geo", index = "week"),
               "No period appears in both")
  # Ungrouped, one spend row per week covers the pooled contributions.
  national <- aggregate(cbind(tv, search) ~ week, data = p$d, FUN = sum)
  expect_equal(roi(p$contrib, national, index = "week"),
               roi(p$contrib, p$d[c("tv", "search")]))
})

test_that("periods keeps the same periods of contributions and spend", {
  p <- roi_panel()
  spend <- p$d[c("week", "geo", "tv", "search")]
  last4 <- sort(unique(p$d$week))[9:12]
  r <- roi(p$contrib, spend, by = "geo", index = "week", periods = last4)
  for (i in seq_len(nrow(r))) {
    rows <- which(p$d$geo == r$group[i] & p$d$week %in% last4)
    expect_equal(r$roi[i], roi_by_hand(p, rows, r$channel[i]))
  }
  # Dates may be given as text in ISO format.
  expect_equal(roi(p$contrib, spend, by = "geo", index = "week",
                   periods = format(last4)), r)
  # With totals for spend, only the contributions are cut.
  east <- p$d$geo == "east" & p$d$week %in% last4
  totals <- c(tv = sum(p$d$tv[east]), search = sum(p$d$search[east]))
  east_contrib <- p$contrib[p$contrib$group == "east", ]
  one <- roi(east_contrib, totals, periods = last4)
  expect_equal(one$roi[one$channel == "tv"], roi_by_hand(p, which(east), "tv"))
  # Spend with none of the periods asked for.
  expect_error(roi(p$contrib, spend[spend$week < min(last4), ], by = "geo",
                   index = "week", periods = last4),
               "No value of `periods` is in `index`")
})

test_that("a blank group label is a group of its own", {
  p <- roi_panel()
  blank <- ifelse(p$d$geo == "east", "", p$d$geo)
  contrib <- contributions(p$media, p$coefs, by = blank)
  r <- roi(contrib, p$d[c("tv", "search")], by = blank)
  expect_identical(unique(r$group), c("", "west"))
  expect_equal(r$roi[r$group == "" & r$channel == "tv"],
               roi_by_hand(p, which(blank == ""), "tv"))
  expect_warning(roi(contrib, p$d[-1, c("tv", "search")], by = blank[-1]),
                 "In 1 group")
})

test_that("roi()'s new arguments are validated", {
  p <- roi_panel()
  spend <- p$d[c("week", "geo", "tv", "search")]
  plain <- data.frame(channel = c("tv", "search"), contribution = c(5, 3))
  expect_error(roi(p$contrib, spend, periods = p$d$week[1]), "needs `index`")
  expect_error(roi(p$contrib, spend, index = "week", periods = as.Date(NA)),
               "missing values")
  expect_error(roi(p$contrib, spend, index = "week", periods = list(1)),
               "vector of one or more periods")
  expect_error(roi(p$contrib, spend, index = "week",
                   periods = as.Date("2030-01-01")),
               "No value of `periods`")
  expect_error(roi(plain, spend, by = "geo"), "group column")
  expect_error(roi(plain, spend, index = "week"), "period column")
  expect_error(roi(c(tv = 5), spend, by = "geo"), "data frame from")
  expect_error(roi(p$contrib, c(tv = 5, search = 3), by = "geo"),
               "data frame with one\\s+row per period")
  expect_error(roi(p$contrib, spend, by = "region"), "\"region\"")
  expect_error(roi(p$contrib, spend, by = p$d$geo[-1]), "one value per")
  bad <- p$d$geo
  bad[3] <- NA
  expect_error(roi(p$contrib, spend, by = bad), "missing values")
  expect_error(roi(p$contrib, spend, index = replace(p$d$week, 2, NA)),
               "missing values")
  err <- tryCatch(roi(p$contrib, spend, by = "region"), error = identity)
  expect_identical(conditionCall(err)[[1]], as.name("roi"))
})

test_that("a single row of spend can be labelled by a value or a column", {
  p <- roi_panel()
  row1 <- p$contrib[p$contrib$period == p$d$week[1] &
                      p$contrib$group == "east", ]
  r <- roi(row1, p$d[1, c("tv", "search")], by = "east")
  expect_identical(unique(r$group), "east")
  expect_equal(r$roi[r$channel == "tv"], roi_by_hand(p, 1, "tv"))
  expect_equal(roi(row1, p$d[1, c("geo", "tv", "search")], by = "geo"), r)
})

# ---- mroi(), response_curve(), spend_for() edge cases ------------------------

test_that("the slope step scales with the curve, so tiny spend scales work", {
  h <- 1e-9
  x <- c(0.5, 1, 2) * h
  analytic <- 5200 * 2 * x * h^2 / (x^2 + h^2)^2
  rc <- response_curve(x, 5200, half_max = h, shape = 2)
  expect_equal(rc$marginal, analytic, tolerance = 1e-3)
  expect_equal(mroi(h, 5200, half_max = h, shape = 2), 5200 / (2 * h),
               tolerance = 1e-3)
})

test_that("type = 'none' is a linear response and unknown types fail cleanly", {
  expect_equal(mroi(5, 2, type = "none"), 2)
  expect_equal(response_curve(c(0, 10), 2, type = "none")$response, c(0, 20))
  expect_equal(spend_for(10, 2, type = "none", max_spend = 100), 5,
               tolerance = 1e-8)
  expect_error(mroi(5, 2, type = "hil", half_max = 5), "must be one of")
  expect_error(response_curve(c(-1, 1), 1, half_max = 1), "non-negative")
})

test_that("spend_for() handles tiny targets and refuses unreachable ones", {
  expect_equal(spend_for(5e-9, 0.05, half_max = 100), 1e-5, tolerance = 1e-4)
  expect_equal(spend_for(0, 2, half_max = 5), 0)
  expect_error(spend_for(-1, 2, half_max = 5), "can only raise")
  expect_error(spend_for(1, -2, half_max = 5), "can only lower")
  expect_error(spend_for(1, 0, half_max = 5), "coefficient of 0")
})

# ---- marginal_roi() argument checks -------------------------------------------

test_that("marginal_roi() bounds extend and names spend in the by message", {
  x <- c(10, 0, 5, 20)
  expect_error(marginal_roi(x, 2, extend = 1e9), "between 0 and")
  expect_error(marginal_roi(x, 2, extend = Inf), "finite whole number")
  expect_error(marginal_roi(x, 2, by = 1:3), "element of `spend`")
  expect_equal(marginal_roi(x, 2, extend = 3)$spend, 35)
})
