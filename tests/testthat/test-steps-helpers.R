# Tests for step_adstock_channels() (R/steps-helpers.R): one step per channel,
# each with its own selector, parameters and tuning ids.

# Adstock of `cn` over each geo's whole series, aligned to `data`'s rows.
per_geo <- function(data, cn, decay) {
  out <- numeric(nrow(data))
  for (g in unique(data$geo)) {
    rows <- which(data$geo == g)
    rows <- rows[order(data$date[rows])]
    out[rows] <- adstock_geometric(data[[cn]][rows], decay = decay)
  }
  out
}

test_that("each channel gets its own step, selector, id and decay", {
  skip_if_not_installed("recipes")
  data(mm_weekly)
  tr <- mm_weekly[mm_weekly$date < as.Date("2025-07-01"), ]
  te <- mm_weekly[mm_weekly$date >= as.Date("2025-07-01"), ]
  channels <- c("tv", "search", "display")
  decays <- c(tv = 0.85, search = 0.15, display = 0.4)

  rec <- recipes::recipe(revenue ~ ., data = tr) |>
    step_adstock_channels(channels, index = "date", by = "geo",
                          decay = decays[c("search", "display", "tv")])
  expect_length(rec$steps, 3L)
  expect_identical(vapply(rec$steps, function(s) s$decay, numeric(1)),
                   unname(decays))
  ids <- vapply(rec$steps, `[[`, character(1), "id")
  expect_true(all(startsWith(ids, paste0("adstock_", channels, "_"))))

  prepped <- recipes::prep(rec)
  expect_identical(lapply(prepped$steps, `[[`, "columns"), as.list(channels))
  baked <- expect_no_warning(recipes::bake(prepped, new_data = te))
  for (cn in channels) {
    expect_equal(baked[[cn]],
                 per_geo(mm_weekly, cn, decays[[cn]])[mm_weekly$date >=
                                                        as.Date("2025-07-01")],
                 tolerance = 1e-10, info = cn)
  }
  # the columns no step selected are untouched
  expect_identical(baked$video, te$video)

  # the same as writing one step per channel by hand
  by_hand <- recipes::recipe(revenue ~ ., data = tr) |>
    step_adstock(tv, index = "date", by = "geo", decay = 0.85) |>
    step_adstock(search, index = "date", by = "geo", decay = 0.15) |>
    step_adstock(display, index = "date", by = "geo", decay = 0.4) |>
    recipes::prep()
  expect_identical(baked, recipes::bake(by_hand, new_data = te))
})

test_that("the helper builds what the per-channel loop in vignette('tidymodels') builds", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("hardhat")
  skip_if_not_installed("dials")
  data(mm_weekly)
  channels <- c("tv", "video", "search", "social", "display")
  train <- mm_weekly[mm_weekly$date < as.Date("2025-07-01"), ]

  loop <- recipes::recipe(revenue ~ ., data = train)
  for (ch in channels) {
    loop <- step_adstock(loop, all_of(!!ch), index = "date", by = "geo",
                         decay = hardhat::tune(paste0("decay_", ch)))
  }
  helper <- recipes::recipe(revenue ~ ., data = train) |>
    step_adstock_channels(channels, index = "date", by = "geo",
                          decay = hardhat::tune())
  expect_identical(hardhat::extract_parameter_set_dials(helper)$id,
                   hardhat::extract_parameter_set_dials(loop)$id)

  # With the placeholders filled in, every step selects its own channel.
  fixed <- function(rec) {
    for (i in seq_along(rec$steps)) rec$steps[[i]]$decay <- 0.5
    recipes::prep(rec)
  }
  expect_identical(lapply(fixed(loop)$steps, `[[`, "columns"),
                   as.list(channels))
  expect_identical(recipes::juice(fixed(helper)), recipes::juice(fixed(loop)))
})

test_that("shared arguments reach every step", {
  skip_if_not_installed("recipes")
  d <- data.frame(y = 1, t = 1:6, a = c(1, 0, 0, 2, 0, 0),
                  b = c(0, 3, 0, 0, 1, 0))
  rec <- recipes::recipe(y ~ ., data = d) |>
    step_adstock_channels(c("a", "b"), index = "t", kernel = "delayed",
                          decay = 0.5, peak = 1, max_lag = 3,
                          start = "steady", keep_original_cols = TRUE,
                          prefix = "ad_") |>
    recipes::prep()
  out <- recipes::juice(rec)
  for (cn in c("a", "b")) {
    x <- d[[cn]]
    expected <- adstock_delayed(x, decay = 0.5, peak = 1, max_lag = 3,
                                state = adstock_steady_state(x, max_lag = 3))
    expect_equal(out[[paste0("ad_", cn)]], expected, tolerance = 1e-10)
    expect_identical(out[[cn]], x)
  }
})

test_that("tune() gives each channel a parameter of its own", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("hardhat")
  skip_if_not_installed("dials")
  tune <- hardhat::tune
  data(mm_weekly)

  rec <- recipes::recipe(revenue ~ ., data = mm_weekly) |>
    step_adstock_channels(c("tv", "search"), index = "date", by = "geo",
                          decay = tune()) |>
    step_saturation(tv, search, half_max = tune(), shape = tune())
  ps <- hardhat::extract_parameter_set_dials(rec)
  expect_identical(ps$id, c("decay_tv", "decay_search", "half_max", "shape"))
  expect_identical(ps$name[1:2], c("decay", "decay"))
  step_ids <- vapply(rec$steps[1:2], `[[`, character(1), "id")
  expect_identical(ps$component_id[1:2], step_ids)
  expect_false(any(dials::has_unknowns(ps$object)))

  # tunable() offers decay and max_lag in each step, and tune_args() finds
  # the placeholder in each under its own id.
  tn <- generics::tunable(rec)
  expect_identical(tn$component_id[tn$name == "decay"][1:2], step_ids)
  # Recent recipes releases set `tunable` to FALSE when a tune() id differs
  # from the parameter's name, so the rows are selected by parameter here.
  ta <- generics::tune_args(rec)
  expect_identical(ta$id[ta$component == "step_adstock" & ta$name == "decay"],
                   c("decay_tv", "decay_search"))

  # An id given to tune() becomes the stem, and a named list mixes tuned and
  # fixed channels.
  rec2 <- recipes::recipe(revenue ~ ., data = mm_weekly) |>
    step_adstock_channels(c("tv", "search", "social"), index = "date",
                          by = "geo", kernel = "delayed",
                          decay = list(tv = tune(), search = 0.3,
                                       social = tune("social_carry")),
                          peak = tune("lag"), max_lag = 6)
  ps2 <- hardhat::extract_parameter_set_dials(rec2)
  expect_setequal(ps2$id, c("decay_tv", "social_carry", "lag_tv",
                            "lag_search", "lag_social"))
  expect_identical(rec2$steps[[2]]$decay, 0.3)
  # the delayed kernel's ranges are kept per step
  peak_range <- unname(unlist(ps2$object[[which(ps2$id == "lag_tv")]]$range))
  expect_equal(peak_range, c(0, 4))
  decay_range <- unname(unlist(ps2$object[[which(ps2$id == "decay_tv")]]$range))
  expect_equal(decay_range, c(0.05, 0.95))
})

test_that("finalising each channel's parameter bakes each channel at its own decay", {
  # The same substitution tune::finalize_recipe() makes: the grid value for
  # each id goes into the step that owns it, under the argument's name.
  skip_if_not_installed("recipes")
  skip_if_not_installed("hardhat")
  skip_if_not_installed("dials")
  data(mm_weekly)
  north <- mm_weekly[mm_weekly$geo == "north", ]
  north <- north[order(north$date), ]

  rec <- recipes::recipe(revenue ~ ., data = north[1:104, ]) |>
    step_adstock_channels(c("tv", "search"), index = "date",
                          decay = hardhat::tune())
  ps <- hardhat::extract_parameter_set_dials(rec)
  grid <- data.frame(decay_tv = c(0.85, 0.15), decay_search = c(0.15, 0.85))
  for (i in seq_len(nrow(grid))) {
    cand <- rec
    for (j in seq_len(nrow(ps))) {
      k <- which(vapply(cand$steps, `[[`, character(1), "id") ==
                   ps$component_id[j])
      value <- stats::setNames(list(grid[[ps$id[j]]][i]), ps$name[j])
      cand$steps[[k]] <- do.call(stats::update, c(list(cand$steps[[k]]), value))
    }
    baked <- recipes::bake(recipes::prep(cand), new_data = north[105:156, ])
    expect_equal(baked$tv,
                 adstock_geometric(north$tv, grid$decay_tv[i])[105:156],
                 tolerance = 1e-10, info = i)
    expect_equal(baked$search,
                 adstock_geometric(north$search, grid$decay_search[i])[105:156],
                 tolerance = 1e-10, info = i)
  }
})

test_that("tune_grid() tunes two channels' decays under their own ids", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("hardhat")
  skip_if_not_installed("dials")
  skip_if_not_installed("rsample")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("workflows")
  skip_if_not_installed("yardstick")
  skip_if_not_installed("tune")
  data(mm_weekly)
  north <- mm_weekly[mm_weekly$geo == "north", ]
  north <- north[order(north$date), c("date", "revenue", "tv", "search")]

  rec <- recipes::recipe(revenue ~ ., data = north) |>
    recipes::update_role(date, new_role = "index") |>
    step_adstock_channels(c("tv", "search"), index = "date",
                          decay = hardhat::tune())
  wf <- workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(parsnip::linear_reg())
  expect_identical(hardhat::extract_parameter_set_dials(wf)$id,
                   c("decay_tv", "decay_search"))

  folds <- rsample::rolling_origin(north, initial = 104, assess = 13,
                                   skip = 12, cumulative = TRUE)
  grid <- data.frame(decay_tv = c(0.85, 0.15), decay_search = c(0.15, 0.85))
  res <- tune::tune_grid(wf, resamples = folds, grid = grid,
                         metrics = yardstick::metric_set(yardstick::rmse))
  scores <- tune::collect_metrics(res)
  expect_identical(nrow(scores), 2L)

  # Each candidate's mean RMSE, refitted by hand from the whole-series adstock
  # of each channel at its own decay. Warm starts make every assessment set
  # equal to that series.
  by_hand <- function(decay_tv, decay_search) {
    x <- data.frame(revenue = north$revenue,
                    tv = adstock_geometric(north$tv, decay = decay_tv),
                    search = adstock_geometric(north$search,
                                               decay = decay_search))
    mean(vapply(folds$splits, function(sp) {
      fit <- stats::lm(revenue ~ tv + search, data = x[sp$in_id, ])
      held <- rsample::complement(sp)
      pred <- stats::predict(fit, newdata = x[held, ])
      sqrt(mean((x$revenue[held] - pred)^2))
    }, numeric(1)))
  }
  expected <- c(by_hand(0.85, 0.15), by_hand(0.15, 0.85))
  got <- scores$mean[match(grid$decay_tv, scores$decay_tv)]
  expect_equal(got, expected, tolerance = 1e-8)
  expect_identical(scores$decay_search[match(grid$decay_tv, scores$decay_tv)],
                   grid$decay_search)
})

test_that("a single channel, and per-channel max_lag, shape and scale", {
  skip_if_not_installed("recipes")
  d <- data.frame(y = 1, t = 1:8, a = c(5, 0, 0, 0, 5, 0, 0, 0),
                  b = c(0, 0, 4, 0, 0, 0, 4, 0))
  one <- recipes::recipe(y ~ ., data = d) |>
    step_adstock_channels("a", index = "t", decay = c(a = 0.3)) |>
    recipes::prep()
  expect_length(one$steps, 1L)
  expect_equal(recipes::juice(one)$a, adstock_geometric(d$a, 0.3),
               tolerance = 1e-10)

  wb <- recipes::recipe(y ~ ., data = d) |>
    step_adstock_channels(c("a", "b"), index = "t", kernel = "weibull",
                          shape = c(a = 1.5, b = 3), scale = 2,
                          max_lag = list(b = 3, a = 5)) |>
    recipes::prep()
  out <- recipes::juice(wb)
  expect_equal(out$a, adstock_weibull(d$a, shape = 1.5, scale = 2, max_lag = 5),
               tolerance = 1e-10)
  expect_equal(out$b, adstock_weibull(d$b, shape = 3, scale = 2, max_lag = 3),
               tolerance = 1e-10)
})

test_that("missing media in one channel is handled within that channel's step", {
  skip_if_not_installed("recipes")
  d <- data.frame(y = 1, t = 1:5, a = c(1, NA, 3, 4, 5), b = c(2, 2, 2, 2, 2))
  rec <- recipes::recipe(y ~ ., data = d) |>
    step_adstock_channels(c("a", "b"), index = "t", decay = 0.5) |>
    recipes::prep()
  expect_equal(recipes::juice(rec)$a,
               adstock_geometric(c(1, 0, 3, 4, 5), decay = 0.5),
               tolerance = 1e-10)
  expect_error(
    recipes::prep(recipes::recipe(y ~ ., data = d) |>
                    step_adstock_channels(c("a", "b"), index = "t",
                                          na_action = "error")),
    "missing values: \"a\""
  )
})

test_that("channels is checked", {
  skip_if_not_installed("recipes")
  d <- data.frame(y = 1, t = 1:5, a = 1:5, b = 5:1)
  rec <- recipes::recipe(y ~ ., data = d)
  expect_error(step_adstock_channels(rec, 1:2, index = "t"),
               "must be a character vector")
  expect_error(step_adstock_channels(rec, character(0), index = "t"),
               "must be a character vector")
  expect_error(step_adstock_channels(rec, c("a", NA), index = "t"),
               "missing or empty")
  expect_error(step_adstock_channels(rec, c("a", ""), index = "t"),
               "missing or empty")
  expect_error(step_adstock_channels(rec, c("a", "b", "a"), index = "t"),
               "names \"a\" more than once")
  # a name that is not a column is found when the recipe is prepared
  expect_error(
    recipes::prep(step_adstock_channels(rec, c("a", "radio"), index = "t")),
    "radio"
  )
})

test_that("the other arguments must be named step_adstock() arguments", {
  skip_if_not_installed("recipes")
  d <- data.frame(y = 1, t = 1:5, a = 1:5, b = 5:1)
  rec <- recipes::recipe(y ~ ., data = d)
  expect_error(step_adstock_channels(rec, "a", "b", index = "t"),
               "must be named")
  expect_error(step_adstock_channels(rec, "a", index = "t", decai = 0.4),
               "has no argument `decai`")
  expect_error(step_adstock_channels(rec, "a", index = "t", id = "x"),
               "`id` is set for each step")
  expect_error(step_adstock_channels(rec, "a", index = "t", skip = TRUE,
                                     columns = "a"),
               "`skip` and `columns` are set")
})

test_that("per-channel values must name each channel once", {
  skip_if_not_installed("recipes")
  d <- data.frame(y = 1, t = 1:5, a = 1:5, b = 5:1)
  rec <- recipes::recipe(y ~ ., data = d)
  add <- function(...) step_adstock_channels(rec, c("a", "b"), index = "t", ...)

  expect_error(add(decay = c(0.2, 0.4)), "single value, or one value per")
  expect_error(add(decay = list(0.2)), "single value, or one value per")
  expect_error(add(decay = c(a = 0.2)), "Missing: \"b\"")
  expect_error(add(decay = c(a = 0.2, b = 0.4, c = 0.1)),
               "Not in `channels`: \"c\"")
  expect_error(add(decay = c(a = 0.2, b = 0.4, a = 0.1)),
               "Named more than once: \"a\"")
  # an invalid value names the channel whose step it was meant for
  err <- tryCatch(add(decay = c(a = 0.2, b = 2)), error = function(e) e)
  expect_match(conditionMessage(err), "channel \"b\"")
  expect_match(conditionMessage(err), "`decay` must be in \\[0, 1\\]")
  # a step that cannot be tuned that way says so for the first channel
  skip_if_not_installed("hardhat")
  expect_error(add(shape = hardhat::tune()),
               "`shape` cannot be tuned")
})
