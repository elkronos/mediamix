# Tests for the planning helpers in R/plan.R: transform_plan(),
# simulate_plan() and reallocate().

north_plan <- function() {
  data(mm_weekly, package = "mediamix", envir = environment())
  mm_weekly[mm_weekly$geo == "north", ]
}

truth_spec <- function(channels) {
  data(mm_weekly, package = "mediamix", envir = environment())
  truth <- attr(mm_weekly, "truth")
  lapply(stats::setNames(channels, channels), function(ch) list(
    adstock = list(decay = truth$decay[[ch]]),
    saturation = list(half_max = truth$half_max[[ch]],
                      shape = truth$shape[[ch]])
  ))
}

truth_beta <- function() {
  data(mm_weekly, package = "mediamix", envir = environment())
  attr(mm_weekly, "truth")$beta
}

# ---- transform_plan() ------------------------------------------------------------

test_that("transform_plan() transforms each channel with its own entry and keeps the rest", {
  north <- north_plan()
  channels <- c("tv", "search")
  spec <- truth_spec(channels)
  out <- transform_plan(north, spec)
  expect_identical(names(out), names(north))
  expect_identical(dim(out), dim(north))
  for (ch in channels) {
    expect_equal(out[[ch]],
                 media_transform(north[[ch]], adstock = spec[[ch]]$adstock,
                                 saturation = spec[[ch]]$saturation))
  }
  others <- setdiff(names(north), channels)
  expect_identical(out[others], north[others])
})

test_that("an element left out of a spec entry takes media_transform()'s default", {
  plan <- data.frame(x = c(0, 100, 50, 0, 200))
  out <- transform_plan(plan, list(x = list(saturation = list(half_max = 80))))
  expect_equal(out$x, media_transform(plan$x, saturation = list(half_max = 80)))
  expect_equal(transform_plan(plan, list(x = list(adstock = NULL)))$x, plan$x)
  expect_equal(transform_plan(plan, list(x = list()))$x, media_transform(plan$x))
})

test_that("a spec entry's order is honoured, and its warning names the channel", {
  plan <- data.frame(tv = c(0, 500, 800, 300), search = c(100, 0, 50, 20))
  spec <- list(
    tv = list(adstock = list(decay = 0.5), saturation = list(half_max = 300),
              order = "saturate_then_adstock"),
    search = list(adstock = list(decay = 0.2))
  )
  w <- tryCatch(transform_plan(plan, spec), warning = identity)
  expect_match(conditionMessage(w), "channel \"tv\"")
  expect_match(conditionMessage(w), "before adstock")
  out <- suppressWarnings(transform_plan(plan, spec))
  expect_equal(out$tv, suppressWarnings(media_transform(
    plan$tv, adstock = list(decay = 0.5), saturation = list(half_max = 300),
    order = "saturate_then_adstock")))
})

test_that("by groups rows the same way given as column names, a vector or a data frame", {
  data(mm_weekly, envir = environment())
  spec <- truth_spec(c("tv", "social"))
  by_name <- transform_plan(mm_weekly, spec, by = "geo")
  expect_equal(by_name, transform_plan(mm_weekly, spec, by = mm_weekly$geo))
  expect_equal(by_name, transform_plan(mm_weekly, spec, by = mm_weekly["geo"]))
  expect_equal(by_name$tv,
               media_transform(mm_weekly$tv, adstock = spec$tv$adstock,
                               saturation = spec$tv$saturation,
                               by = mm_weekly$geo))
  pooled <- transform_plan(mm_weekly, spec)
  expect_false(isTRUE(all.equal(pooled$tv, by_name$tv)))
})

test_that("transform_plan() names what is wrong with plan, spec or by", {
  plan <- data.frame(tv = c(1, 2, 3), geo = c("a", "a", "b"),
                     label = c("x", "y", "z"))
  ok <- list(adstock = list(decay = 0.5))
  expect_error(transform_plan(as.list(plan), list(tv = ok)), "data frame",
               class = "rlang_error")
  expect_error(transform_plan(plan[0, ], list(tv = ok)), "no rows")
  expect_error(transform_plan(plan, list(tv2 = ok)), "no column \"tv2\"")
  expect_error(transform_plan(plan, list(label = ok)), "must be numeric")
  expect_error(transform_plan(plan, list(geo = ok)), "must be numeric")
  expect_error(
    transform_plan(plan, list(tv = list(adstock = list(decay = 0.5),
                                        saturaton = list(half_max = 2)))),
    "unknown element: \"saturaton\""
  )
  expect_error(transform_plan(plan, list(ok)), "named after")
  expect_error(transform_plan(plan, list(tv = ok, tv = ok)), "more than once")
  expect_error(transform_plan(plan, list(tv = 0.5)), "must be a list")
  expect_error(transform_plan(plan, list(tv = list(0.5))), "must be named")
  expect_error(transform_plan(plan, list(tv = list(adstock = ok$adstock,
                                                   adstock = NULL))),
               "more than once")
  expect_error(
    transform_plan(plan, list(tv = list(adstock = list(decay = 0.5,
                                                       by = plan$geo)))),
    "must not contain `by`"
  )
  expect_error(transform_plan(plan, list()), "no channels")
  expect_error(transform_plan(plan, "tv"), "must be a list")
  expect_error(transform_plan(plan, list(tv = ok), by = c("a", "b")),
               "one element per row")
  expect_error(transform_plan(plan, list(tv = ok), by = "region"),
               "Not columns of `plan`: \"region\"")
  expect_error(transform_plan(plan, list(tv = ok), by = "tv"),
               "channel column")
  expect_error(transform_plan(plan, list(tv = ok), by = c("a", NA, "b")),
               "missing values")
})

test_that("an error in one channel's transform names the channel and keeps the cause", {
  plan <- data.frame(tv = c(1, 2, 3), search = c(1, NA, 3))
  err <- tryCatch(
    transform_plan(plan, list(tv = list(saturation = list(type = "hill")))),
    error = identity
  )
  expect_match(conditionMessage(err), "channel \"tv\"")
  expect_match(conditionMessage(err), "half_max")
  expect_error(transform_plan(plan, list(search = list())), "\"search\"")
  # Missing spend follows the na_action of the entry's adstock list.
  out <- transform_plan(plan, list(search = list(
    adstock = list(decay = 0.5, na_action = "zero"))))
  expect_equal(out$search, adstock_geometric(c(1, 0, 3), 0.5))
})

test_that("transform_plan() handles one row, zero spend, data.tables and tibbles", {
  one <- data.frame(tv = 100, geo = "a")
  expect_equal(transform_plan(one, list(tv = list()))$tv, 50)
  zero <- data.frame(tv = rep(0, 4))
  expect_equal(
    transform_plan(zero, list(tv = list(saturation = list(half_max = 10))))$tv,
    rep(0, 4)
  )
  dt <- data.table::data.table(tv = c(100, 0, 50), geo = "a")
  out <- transform_plan(dt, list(tv = list(adstock = list(decay = 0.5))))
  expect_s3_class(out, "data.table")
  expect_equal(out$tv, adstock_geometric(c(100, 0, 50), 0.5))
  expect_identical(dt$tv, c(100, 0, 50))
  expect_no_warning(out[, extra := 1])
  skip_if_not_installed("tibble")
  tb <- tibble::tibble(tv = c(100, 0, 50), geo = "a")
  expect_s3_class(transform_plan(tb, list(tv = list())), "tbl_df")
})

# ---- simulate_plan() -------------------------------------------------------------

test_that("simulate_plan() gives each channel its coefficient times its transformed total", {
  north <- north_plan()
  channels <- c("tv", "search", "display")
  spec <- truth_spec(channels)
  beta <- truth_beta()
  sim <- simulate_plan(north, spec, beta)
  expect_identical(names(sim), c("channel", "spend", "response"))
  expect_identical(sim$channel, channels)
  media <- transform_plan(north, spec)
  expect_equal(sim$response, unname(beta[channels] * colSums(media[channels])))
  expect_equal(sim$spend, unname(colSums(north[channels])))
  # The same quantity marginal_roi() divides by spend.
  m <- marginal_roi(north$tv, beta[["tv"]], adstock = spec$tv$adstock,
                    saturation = spec$tv$saturation)
  expect_equal(sim$response[1], m$contribution)
})

test_that("a baseline adds the change from it, and a plan against itself changes nothing", {
  north <- north_plan()
  spec <- truth_spec(c("search", "social", "display"))
  beta <- truth_beta()
  same <- simulate_plan(north, spec, beta, baseline = north)
  expect_identical(names(same), c("channel", "spend", "response",
                                  "spend_change", "response_change"))
  expect_equal(same$spend_change, rep(0, 3))
  expect_equal(same$response_change, rep(0, 3))

  amount <- 0.1 * sum(north$display)
  moved <- reallocate(north, "display", "social", amount)
  change <- simulate_plan(moved, spec, beta, baseline = north)
  expect_equal(change$spend_change, c(0, amount, -amount))
  # The walkthrough's media_revenue(), written out.
  revenue <- function(p) {
    sum(vapply(names(spec), function(ch) {
      beta[[ch]] * sum(media_transform(p[[ch]], adstock = spec[[ch]]$adstock,
                                       saturation = spec[[ch]]$saturation))
    }, numeric(1)))
  }
  expect_equal(sum(change$response_change), revenue(moved) - revenue(north))
})

test_that("simulate_plan() transforms within groups and extends each group", {
  data(mm_weekly, envir = environment())
  spec <- truth_spec(c("tv", "search"))
  beta <- truth_beta()
  grouped <- simulate_plan(mm_weekly, spec, beta, by = "geo")
  hand <- vapply(names(spec), function(ch) {
    beta[[ch]] * sum(media_transform(mm_weekly[[ch]],
                                     adstock = spec[[ch]]$adstock,
                                     saturation = spec[[ch]]$saturation,
                                     by = mm_weekly$geo))
  }, numeric(1))
  expect_equal(grouped$response, unname(hand))
  long <- simulate_plan(mm_weekly, spec, beta, by = "geo", extend = 20)
  expect_true(all(long$response > grouped$response))
  m <- marginal_roi(mm_weekly$tv, beta[["tv"]], adstock = spec$tv$adstock,
                    saturation = spec$tv$saturation, by = mm_weekly$geo,
                    extend = 20)
  expect_equal(long$response[1], m$contribution)
})

test_that("a baseline is grouped by its own columns, or by the same vector", {
  data(mm_weekly, envir = environment())
  spec <- truth_spec("social")
  beta <- truth_beta()
  first_year <- mm_weekly[mm_weekly$date < as.Date("2024-01-01"), ]
  r <- simulate_plan(mm_weekly, spec, beta, by = "geo", baseline = first_year)
  expect_equal(r$response_change,
               r$response - simulate_plan(first_year, spec, beta,
                                          by = "geo")$response)
  expect_error(simulate_plan(mm_weekly, spec, beta, by = mm_weekly$geo,
                             baseline = first_year),
               "applies to both")
  expect_error(simulate_plan(mm_weekly, spec, beta, by = "geo",
                             baseline = first_year[c("date", "social")]),
               "Not columns of `baseline`")
})

test_that("simulate_plan() reads coefficients by name and refuses missing ones", {
  plan <- data.frame(tv = c(100, 0, 50), `TV Spend` = c(1, 2, 3),
                     check.names = FALSE)
  spec <- list(tv = list(adstock = list(decay = 0.5)))
  full <- c("(Intercept)" = 10, price = -2, tv = 3)
  expect_equal(simulate_plan(plan, spec, full)$response,
               3 * sum(adstock_geometric(plan$tv, 0.5)))
  expect_error(simulate_plan(plan, spec, c(price = 1)),
               "no value for 1 channel")
  expect_error(simulate_plan(plan, spec, c(tv = NA_real_)),
               "missing or infinite")
  expect_error(simulate_plan(plan, spec, 3), "named by channel")
  ticked <- list(`TV Spend` = list(adstock = NULL))
  expect_equal(simulate_plan(plan, ticked, c("`TV Spend`" = 2))$response, 12)
})

test_that("simulate_plan() refuses missing, negative or infinite spend", {
  spec <- list(tv = list(adstock = list(decay = 0.5)))
  expect_error(simulate_plan(data.frame(tv = c(1, NA)), spec, c(tv = 1)),
               "1 missing value")
  expect_error(simulate_plan(data.frame(tv = c(1, -1)), spec, c(tv = 1)),
               "negative")
  expect_error(simulate_plan(data.frame(tv = c(1, Inf)), spec, c(tv = 1)),
               "infinite")
  expect_error(simulate_plan(data.frame(tv = 1), spec, c(tv = 1), extend = -1),
               "extend")
  expect_equal(simulate_plan(data.frame(tv = 100), spec, c(tv = 2))$response,
               100)
  expect_equal(simulate_plan(data.frame(tv = c(0, 0)), spec,
                             c(tv = 2))$response, 0)
})

# ---- reallocate() ------------------------------------------------------------------

test_that("reallocate() moves the amount and keeps both channels' flighting", {
  north <- north_plan()
  amount <- 2000
  moved <- reallocate(north, "display", "social", amount)
  expect_equal(sum(moved$display), sum(north$display) - amount)
  expect_equal(sum(moved$social), sum(north$social) + amount)
  on <- north$display > 0
  expect_equal(moved$display[on] / north$display[on],
               rep(1 - amount / sum(north$display), sum(on)))
  expect_true(all(moved$display[!on] == 0))
  live <- north$social > 0
  expect_equal(moved$social[live] / north$social[live],
               rep(1 + amount / sum(north$social), sum(live)))
  expect_true(all(moved$social[!live] == 0))
  others <- setdiff(names(north), c("display", "social"))
  expect_identical(moved[others], north[others])
})

test_that("reallocate() moves nothing for zero, all of it at most, and says why not", {
  plan <- data.frame(a = c(10, 0, 30), b = c(5, 5, 0), c = c(0, 0, 0))
  expect_identical(reallocate(plan, "a", "b", 0), plan)
  all_in <- reallocate(plan, "a", "b", 40)
  expect_equal(all_in$a, c(0, 0, 0))
  expect_equal(all_in$b, c(25, 25, 0))
  expect_equal(reallocate(data.frame(a = 10, b = 2), "a", "b", 4),
               data.frame(a = 6, b = 6))
  expect_error(reallocate(plan, "a", "b", 41), "more than \"a\" spends")
  expect_error(reallocate(plan, "a", "c", 1), "no spend")
  expect_error(reallocate(plan, "a", "c", 1), "1e-6")
  expect_identical(reallocate(plan, "a", "c", 0), plan)
  expect_error(reallocate(plan, "a", "b", -1), "swap")
  expect_error(reallocate(plan, "a", "a", 1), "same column")
  expect_error(reallocate(plan, "a", "z", 1), "no column \"z\"")
  expect_error(reallocate(plan, c("a", "b"), "c", 1), "one column name")
  expect_error(reallocate(plan, "a", "b", c(1, 2)), "one number")
  expect_error(reallocate(plan, "a", "b", NA), "finite number")
  expect_error(reallocate(plan, "a", "b", c(x = 1)), "no `by`")
  plan$a[2] <- NA
  expect_error(reallocate(plan, "a", "b", 1), "missing value")
})

test_that("with by, reallocate() moves the amount within each group or named groups only", {
  data(mm_weekly, envir = environment())
  each <- reallocate(mm_weekly, "display", "social", 1000, by = "geo")
  taken <- tapply(mm_weekly$display - each$display, mm_weekly$geo, sum)
  expect_equal(as.numeric(taken), rep(1000, 3))
  expect_equal(each, reallocate(mm_weekly, "display", "social", 1000,
                                by = mm_weekly$geo))

  north_only <- reallocate(mm_weekly, "display", "social", c(north = 1000),
                           by = "geo")
  gained <- tapply(north_only$social - mm_weekly$social, mm_weekly$geo, sum)
  expect_equal(as.numeric(gained[c("north", "central", "south")]),
               c(1000, 0, 0))
  expect_error(reallocate(mm_weekly, "display", "social", c(west = 1),
                          by = "geo"), "\"west\"")
  expect_error(reallocate(mm_weekly, "display", "social", c(1, 2),
                          by = "geo"), "named by group")

  # Without by, the amount is taken from every group in proportion to spend.
  overall <- reallocate(mm_weekly, "display", "social", 3000)
  share <- tapply(mm_weekly$display, mm_weekly$geo, sum) /
    sum(mm_weekly$display)
  taken <- tapply(mm_weekly$display - overall$display, mm_weekly$geo, sum)
  expect_equal(as.numeric(taken), as.numeric(3000 * share))

  dark <- mm_weekly
  dark$social[dark$geo == "south"] <- 0
  expect_error(reallocate(dark, "display", "social", 10, by = "geo"),
               "group \"south\"")
  expect_error(reallocate(dark, "display", "social", 1e6, by = "geo"),
               "more than")
})
