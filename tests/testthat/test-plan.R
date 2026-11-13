# Tests for the planning helpers: transform_plan(), simulate_plan() and
# reallocate() in R/plan.R, allocate_budget() in R/allocate.R, its plot method
# in R/plot-allocate.R, and marginal_roi() over a data frame.

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

# A small plan with three flighted channels and concave curves by default.
toy_plan <- function() {
  t <- seq_len(30)
  data.frame(week = t,
             a = 100 + 40 * sin(t / 2),
             b = 60 * (1 + cos(t / 3)),
             c = 30 + 20 * (t %% 4 == 0))
}

toy_spec <- function(shape = c(a = 1, b = 1, c = 1)) {
  list(
    a = list(adstock = list(decay = 0.6),
             saturation = list(half_max = 80, shape = shape[["a"]])),
    b = list(adstock = list(decay = 0.3),
             saturation = list(half_max = 50, shape = shape[["b"]])),
    c = list(adstock = list(decay = 0.1),
             saturation = list(half_max = 40, shape = shape[["c"]]))
  )
}

toy_beta <- c(a = 50, b = 40, c = 30)

# A channel's response at a total spend of `level`, written out from the
# transforms rather than through the functions under test.
toy_response <- function(plan, spec, beta, ch, level) {
  x <- plan[[ch]]
  z <- adstock_geometric(x * (level / sum(x)), decay = spec[[ch]]$adstock$decay)
  beta[[ch]] * sum(saturate_hill(z, half_max = spec[[ch]]$saturation$half_max,
                                 shape = spec[[ch]]$saturation$shape))
}

# Draw `expr` on a null device and return the calls it made to the base
# graphics functions in `fns`, with their arguments evaluated and matched.
record_graphics <- function(expr, fns = c("points", "text", "rect", "segments",
                                          "axis", "mtext", "title")) {
  log <- new.env(parent = emptyenv())
  log$calls <- list()
  graphics_ns <- asNamespace("graphics")
  wrap <- function(fn) {
    orig <- getExportedValue("graphics", fn)
    def <- if (fn %in% c("points", "text")) {
      utils::getFromNamespace(paste0(fn, ".default"), "graphics")
    } else {
      orig
    }
    function(...) {
      if (!identical(topenv(parent.frame()), graphics_ns)) {
        call <- as.call(c(list(as.name(fn)), list(...)))
        args <- as.list(match.call(def, call))[-1L]
        log$calls[[length(log$calls) + 1L]] <- list(fn = fn, args = args)
      }
      orig(...)
    }
  }
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  mocks <- lapply(stats::setNames(fns, fns), wrap)
  do.call(local_mocked_bindings, c(mocks, list(.package = "graphics")))
  force(expr)
  log$calls
}

recorded_text <- function(calls) {
  unlist(lapply(calls, function(cl) {
    switch(cl$fn,
           text = cl$args$labels,
           mtext = cl$args$text,
           title = cl$args$main,
           axis = if (is.character(cl$args$labels)) cl$args$labels,
           NULL)
  }))
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

# ---- marginal_roi() over a data frame ----------------------------------------------

test_that("marginal_roi() over a data frame matches one call per channel", {
  north <- north_plan()
  channels <- c("tv", "search", "display")
  spec <- truth_spec(channels)
  beta <- truth_beta()
  table <- marginal_roi(north, beta, spec = spec)
  expect_identical(names(table),
                   c("channel", "spend", "contribution", "roi", "mroi"))
  expect_identical(table$channel, channels)
  for (ch in channels) {
    one <- marginal_roi(north[[ch]], beta[[ch]], adstock = spec[[ch]]$adstock,
                        saturation = spec[[ch]]$saturation)
    expect_equal(table[table$channel == ch, -1], one, ignore_attr = TRUE)
  }
  # Without spec, the names of coefficient choose the channels and every
  # channel shares the transform.
  shared <- marginal_roi(north, beta[c("search", "social")],
                         adstock = list(decay = 0.3),
                         saturation = list(half_max = 400))
  one <- marginal_roi(north$social, beta[["social"]],
                      adstock = list(decay = 0.3),
                      saturation = list(half_max = 400))
  expect_identical(shared$channel, c("search", "social"))
  expect_equal(shared[2, -1], one, ignore_attr = TRUE)
  expect_identical(names(marginal_roi(north$tv, 1)),
                   c("spend", "contribution", "roi", "mroi"))
})

test_that("marginal_roi() over a data frame takes rows, by and extend", {
  data(mm_weekly, envir = environment())
  spec <- truth_spec(c("tv", "social"))
  beta <- truth_beta()
  late <- seq_len(nrow(mm_weekly)) %% 156 > 130
  table <- marginal_roi(mm_weekly, beta, spec = spec, by = "geo", rows = late,
                        extend = 10)
  one <- marginal_roi(mm_weekly$tv, beta[["tv"]], adstock = spec$tv$adstock,
                      saturation = spec$tv$saturation, by = mm_weekly$geo,
                      rows = late, extend = 10)
  expect_equal(table[1, -1], one, ignore_attr = TRUE)
  expect_equal(table, marginal_roi(mm_weekly, beta, spec = spec,
                                   by = mm_weekly$geo, rows = late,
                                   extend = 10))
})

test_that("marginal_roi() over a data frame reports dark channels and refuses ambiguity", {
  plan <- data.frame(a = c(10, 20, 30), b = c(0, 0, 0), c = c(5, 0, 0))
  spec <- list(a = list(adstock = list(decay = 0.5)),
               b = list(adstock = list(decay = 0.5)))
  expect_message(r <- marginal_roi(plan, c(a = 1, b = 2, x = 9), spec = spec),
                 "\"b\"")
  expect_identical(r$channel, c("a", "b"))
  expect_true(is.na(r$mroi[2]))
  expect_true(is.na(r$roi[2]))
  expect_message(w <- marginal_roi(plan, c(a = 1, c = 1), rows = 2:3), "\"c\"")
  expect_true(is.na(w$mroi[2]))
  expect_equal(w$roi[2], sum(adstock_geometric(plan$c, 0.5)) / 5)
  expect_error(marginal_roi(plan, c(a = 1), spec = spec), "no value")
  expect_error(marginal_roi(plan, c(a = 1, b = 1),
                            adstock = list(decay = 0.2), spec = spec),
               "once")
  expect_error(marginal_roi(plan$a, 1, spec = spec), "data frame")
  expect_error(marginal_roi(plan, 1), "named by channel")
  expect_error(marginal_roi(plan, c(z = 1)), "no column \"z\"")
  expect_error(marginal_roi(plan, c(a = 1), rows = 5), "index periods")
  expect_error(marginal_roi(plan, c(a = 1), by = 1:2), "one element per row")
})

# ---- allocate_budget() ------------------------------------------------------------

test_that("on two channels the grid search matches a brute-force search", {
  plan <- toy_plan()
  spec <- toy_spec()[c("a", "b")]
  total <- sum(plan$a) + sum(plan$b)
  steps <- 40
  res <- allocate_budget(plan, spec, toy_beta, steps = steps)
  step <- total / steps
  value <- vapply(0:steps, function(j) {
    toy_response(plan, spec, toy_beta, "a", j * step) +
      toy_response(plan, spec, toy_beta, "b", (steps - j) * step)
  }, numeric(1))
  best <- which.max(value) - 1
  expect_equal(res$allocation$recommended_spend, c(best, steps - best) * step)
  expect_equal(sum(res$allocation$recommended_response), max(value))
  expect_equal(res$step, step)
  expect_identical(res$steps, 40L)
})

test_that("on three channels the grid search matches a brute-force search", {
  plan <- toy_plan()
  spec <- toy_spec()
  channels <- c("a", "b", "c")
  total <- sum(plan[channels])
  steps <- 24
  res <- allocate_budget(plan, spec, toy_beta, steps = steps)
  step <- total / steps
  r <- lapply(channels, function(ch) {
    vapply(0:steps, function(j) toy_response(plan, spec, toy_beta, ch, j * step),
           numeric(1))
  })
  grid <- expand.grid(i = 0:steps, j = 0:steps)
  grid <- grid[grid$i + grid$j <= steps, ]
  value <- r[[1]][grid$i + 1] + r[[2]][grid$j + 1] +
    r[[3]][steps - grid$i - grid$j + 1]
  best <- grid[which.max(value), ]
  expect_equal(res$allocation$recommended_spend,
               c(best$i, best$j, steps - best$i - best$j) * step)
})

test_that("the grid answer is within a step of the continuous optimum, with equal marginal returns", {
  plan <- toy_plan()
  spec <- toy_spec()[c("a", "b")]
  total <- sum(plan$a) + sum(plan$b)
  f <- function(s) {
    toy_response(plan, spec, toy_beta, "a", s) +
      toy_response(plan, spec, toy_beta, "b", total - s)
  }
  opt <- stats::optimize(f, c(0, total), maximum = TRUE, tol = 1e-8)$maximum
  expect_true(opt > 0.1 * total && opt < 0.9 * total)
  res <- allocate_budget(plan, spec, toy_beta, steps = 400)
  expect_lte(abs(res$allocation$recommended_spend[1] - opt), res$step)
  expect_equal(res$allocation$mroi[1], res$allocation$mroi[2],
               tolerance = 0.01)
})

test_that("an S-shaped channel gets the budget a brute-force search gives it", {
  plan <- toy_plan()
  spec <- toy_spec(shape = c(a = 4, b = 1, c = 1))[c("a", "b")]
  beta <- c(a = 400, b = 150)
  total <- sum(plan$a) + sum(plan$b)
  steps <- 40
  step <- total / steps
  gain <- function(ch) {
    toy_response(plan, spec, beta, ch, step) -
      toy_response(plan, spec, beta, ch, 0)
  }
  # The first increment returns far less on the S-curve, so a search that
  # adds one increment at a time where it returns most would never fund it.
  expect_lt(gain("a"), gain("b") / 100)
  value <- vapply(0:steps, function(j) {
    toy_response(plan, spec, beta, "a", j * step) +
      toy_response(plan, spec, beta, "b", (steps - j) * step)
  }, numeric(1))
  best <- which.max(value) - 1
  expect_true(best > 0 && best < steps)
  res <- allocate_budget(plan, spec, beta, steps = steps)
  expect_equal(res$allocation$recommended_spend, c(best, steps - best) * step)
})

test_that("allocate_budget() keeps to the bounds and spends exactly the total", {
  plan <- toy_plan()
  spec <- toy_spec()
  now <- colSums(plan[c("a", "b", "c")])
  free <- allocate_budget(plan, spec, toy_beta, steps = 60)
  expect_equal(sum(free$allocation$recommended_spend), sum(now))
  expect_equal(free$total, sum(now))
  expect_equal(free$allocation$current_spend, unname(now))

  lower <- c(a = 0.9 * now[["a"]])
  upper <- c(b = 0.5 * now[["b"]], c = 1.1 * now[["c"]])
  held <- allocate_budget(plan, spec, toy_beta, total = 1.2 * sum(now),
                          lower = lower, upper = upper, steps = 60)
  rec <- stats::setNames(held$allocation$recommended_spend,
                         held$allocation$channel)
  expect_gte(rec[["a"]], lower[["a"]])
  expect_lte(rec[["b"]], upper[["b"]])
  expect_lte(rec[["c"]], upper[["c"]])
  expect_equal(sum(rec), 1.2 * sum(now))

  floor <- allocate_budget(plan, spec, toy_beta, lower = 1000, steps = 30)
  expect_true(all(floor$allocation$recommended_spend >= 1000))
  # Upper bounds that leave less than a step of room still spend the total.
  tight <- allocate_budget(plan, spec, toy_beta, upper = now + 0.1,
                           steps = 30)
  expect_equal(sum(tight$allocation$recommended_spend), sum(now))
  expect_true(all(tight$allocation$recommended_spend <= now + 0.1))
})

test_that("the recommended plan rescales each channel and simulates to the reported response", {
  plan <- toy_plan()
  spec <- toy_spec()
  res <- allocate_budget(plan, spec, toy_beta, steps = 30)
  for (ch in c("a", "b", "c")) {
    rec <- res$allocation$recommended_spend[res$allocation$channel == ch]
    expect_equal(res$plan[[ch]], plan[[ch]] * (rec / sum(plan[[ch]])))
  }
  expect_identical(res$plan$week, plan$week)
  expect_equal(simulate_plan(res$plan, spec, toy_beta)$response,
               res$allocation$recommended_response)
  expect_equal(simulate_plan(plan, spec, toy_beta)$response,
               res$allocation$current_response)
  expect_equal(res$allocation$mroi,
               marginal_roi(res$plan, toy_beta, spec = spec)$mroi)
})

test_that("a channel with no spend stays at zero, and a lower bound on it is an error", {
  plan <- toy_plan()
  plan$d <- 0
  spec <- c(toy_spec(), list(d = list(saturation = list(half_max = 10))))
  beta <- c(toy_beta, d = 100)
  expect_message(res <- allocate_budget(plan, spec, beta, steps = 20),
                 "\"d\" has no spend")
  expect_equal(res$allocation$recommended_spend[4], 0)
  expect_true(is.na(res$allocation$mroi[4]))
  expect_identical(res$plan$d, plan$d)
  expect_error(suppressMessages(
    allocate_budget(plan, spec, beta, lower = c(d = 5))), "pattern")
})

test_that("allocate_budget() names impossible bounds and bad arguments", {
  plan <- toy_plan()
  spec <- toy_spec()
  now <- sum(plan[c("a", "b", "c")])
  expect_error(allocate_budget(plan, spec, toy_beta, upper = 1),
               "upper bounds add up")
  expect_error(allocate_budget(plan, spec, toy_beta, lower = c(a = now + 1)),
               "lower bounds add up")
  expect_error(allocate_budget(plan, spec, toy_beta, lower = c(a = 10),
                               upper = c(a = 5)), "below")
  expect_error(allocate_budget(plan, spec, toy_beta, lower = c(z = 1)),
               "\"z\"")
  expect_error(allocate_budget(plan, spec, toy_beta, lower = c(1, 2)),
               "no names")
  expect_error(allocate_budget(plan, spec, toy_beta, upper = c(a = -1)),
               "negative")
  expect_error(allocate_budget(plan, spec, toy_beta, lower = c(a = Inf)),
               "finite")
  expect_error(allocate_budget(plan, spec, toy_beta, lower = c(a = NA)),
               "numeric vector")
  expect_error(allocate_budget(plan, spec, toy_beta, total = -1), "total")
  expect_error(allocate_budget(plan, spec, toy_beta, steps = 0), "steps")
  expect_error(allocate_budget(plan, spec, toy_beta, steps = 2.5), "steps")
  expect_error(allocate_budget(plan, spec, c(a = 1)), "no value")
})

test_that("allocate_budget() handles a zero budget, one row, one channel and groups", {
  plan <- toy_plan()
  spec <- toy_spec()
  none <- allocate_budget(plan, spec, toy_beta, total = 0)
  expect_equal(none$allocation$recommended_spend, c(0, 0, 0))
  expect_true(all(is.na(none$allocation$mroi)))
  one_row <- allocate_budget(data.frame(a = 10, b = 30), spec[c("a", "b")],
                             toy_beta, steps = 20)
  expect_equal(sum(one_row$allocation$recommended_spend), 40)
  single <- allocate_budget(plan, spec["a"], toy_beta, steps = 5)
  expect_equal(single$allocation$recommended_spend, sum(plan$a))

  data(mm_weekly, envir = environment())
  wk <- truth_spec(c("search", "social"))
  beta <- truth_beta()
  grouped <- allocate_budget(mm_weekly, wk, beta, by = "geo", steps = 20)
  expect_equal(simulate_plan(grouped$plan, wk, beta, by = "geo")$response,
               grouped$allocation$recommended_response)
  pooled <- allocate_budget(mm_weekly, wk, beta, steps = 20)
  expect_false(isTRUE(all.equal(pooled$allocation$current_response,
                                grouped$allocation$current_response)))
})

test_that("a transform warning is shown once however many levels are searched", {
  spec <- toy_spec()[c("a", "b")]
  spec$a$order <- "saturate_then_adstock"
  seen <- character(0)
  withCallingHandlers(
    allocate_budget(toy_plan(), spec, toy_beta, steps = 10),
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(seen, 1L)
  expect_match(seen, "channel \"a\"")
})

test_that("allocations print, convert to a data frame and tidy", {
  res <- allocate_budget(toy_plan(), toy_spec(), toy_beta, steps = 10)
  expect_s3_class(res, "mm_allocation")
  expect_identical(as.data.frame(res), res$allocation)
  expect_identical(names(res$allocation),
                   c("channel", "current_spend", "recommended_spend",
                     "current_response", "recommended_response", "mroi"))
  printed <- paste(capture.output(table <- capture.output(print(res)),
                                  type = "message"), collapse = " ")
  expect_match(printed, "Budget allocation")
  expect_true(any(grepl("recommended_spend", table)))
  skip_if_not_installed("generics")
  expect_identical(generics::tidy(res), res$allocation)
})

# ---- plot.mm_allocation() ---------------------------------------------------------

test_that("the allocation plot restores par, leaves the RNG alone and returns x invisibly", {
  res <- allocate_budget(toy_plan(), toy_spec(), toy_beta, steps = 10)
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit({
    grDevices::dev.off()
    unlink(f)
  })
  op <- graphics::par(no.readonly = TRUE)
  set.seed(42)
  seed <- .Random.seed
  out <- withVisible(plot(res))
  expect_identical(.Random.seed, seed)
  expect_false(out$visible)
  expect_identical(out$value, res)
  expect_identical(graphics::par("mar"), op$mar)
  expect_identical(graphics::par("oma"), op$oma)
  expect_identical(graphics::par("mfrow"), op$mfrow)
})

test_that("the allocation plot labels every channel and takes main and col", {
  res <- allocate_budget(toy_plan(), toy_spec(), toy_beta, steps = 10)
  calls <- record_graphics(plot(res, main = "Next quarter",
                                col = c("black", "grey")))
  text <- recorded_text(calls)
  expect_true(all(c("a", "b", "c") %in% text))
  expect_true(all(c("Next quarter", "Total spend", "Change in response") %in%
                    text))
  expect_true(any(grepl("The same total budget", text)))
  rects <- Filter(function(cl) identical(cl$fn, "rect"), calls)
  fills <- unlist(lapply(rects, function(cl) cl$args$col))
  expect_true(all(fills %in% c("black", "grey")))
  changes <- res$allocation$recommended_response -
    res$allocation$current_response
  expect_identical(sort(unique(fills)),
                   sort(unique(ifelse(changes >= 0, "black", "grey"))))

  more <- allocate_budget(toy_plan(), toy_spec(), toy_beta,
                          total = 1.5 * sum(toy_plan()[c("a", "b", "c")]),
                          steps = 10)
  expect_true(any(grepl("A total budget of",
                        recorded_text(record_graphics(plot(more))))))
})

test_that("the allocation plot names the columns it needs", {
  res <- allocate_budget(toy_plan(), toy_spec(), toy_beta, steps = 10)
  edited <- res
  edited$allocation <- edited$allocation[c("channel", "current_spend")]
  expect_error(plot(edited), "recommended_spend", class = "rlang_error")
  edited$allocation <- NULL
  expect_error(plot(edited), "data frame", class = "rlang_error")
})

test_that("marginal_roi() takes by inside adstock, as media_transform() does", {
  x <- c(100, 0, 50, 80, 0, 20)
  g <- rep(c("north", "south"), each = 3)
  expect_equal(
    marginal_roi(x, 2, adstock = list(decay = 0.5, by = g), extend = 2),
    marginal_roi(x, 2, adstock = list(decay = 0.5), by = g, extend = 2)
  )
  d <- data.frame(tv = x, search = rev(x))
  expect_equal(
    marginal_roi(d, c(tv = 2, search = 1),
                 adstock = list(decay = 0.5, by = g), extend = 2),
    marginal_roi(d, c(tv = 2, search = 1), adstock = list(decay = 0.5),
                 by = g, extend = 2)
  )
  expect_error(
    marginal_roi(x, 2, adstock = list(decay = 0.5, by = g), by = g),
    "both as an argument and inside"
  )
})
