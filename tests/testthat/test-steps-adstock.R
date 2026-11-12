# Tests for step_adstock() (R/steps-adstock.R): the recipes layer's contract
# for carrying filter state across the train/test boundary.

# ---- invariant 4: warm start on contiguous data == transforming the full series

test_that("warm-start on contiguous data equals transforming the full series (mm_weekly, by geo)", {
  skip_if_not_installed("recipes")
  requireNamespace("recipes", quietly = TRUE)

  data(mm_weekly)
  mm_weekly <- mm_weekly[order(mm_weekly$geo, mm_weekly$date), ]
  split_date <- sort(unique(mm_weekly$date))[100]
  train <- mm_weekly[mm_weekly$date <= split_date, ]
  test <- mm_weekly[mm_weekly$date > split_date, ]
  # the remainder really is contiguous with the training period
  expect_equal(as.numeric(min(test$date) - split_date), 7)

  channels <- c("tv", "video", "search", "social", "display")
  rec <- recipes::recipe(revenue ~ ., data = train) |>
    step_adstock(tv, video, search, social, display, index = "date",
                 by = "geo", decay = 0.65) |>
    recipes::prep()

  baked <- recipes::bake(rec, new_data = test)
  baked <- baked[order(baked$geo, baked$date), ]

  for (cn in channels) {
    full_by_geo <- unsplit(
      lapply(split(mm_weekly[[cn]], mm_weekly$geo), adstock_geometric, decay = 0.65),
      mm_weekly$geo
    )
    expected <- full_by_geo[mm_weekly$date > split_date]
    # order expected the same way (by geo, date) as `baked`
    ord <- order(mm_weekly$geo[mm_weekly$date > split_date],
                 mm_weekly$date[mm_weekly$date > split_date])
    expect_equal(baked[[cn]], expected[ord], tolerance = 1e-8, info = cn)
  }

  expect_identical(nrow(baked), nrow(test))
})

# ---- invariant 5: bake() on the training data equals juice() (overlap, cold-start)

test_that("bake(rec, new_data = training) equals juice(rec): the overlap case cold-starts", {
  skip_if_not_installed("recipes")

  data(mm_weekly)
  north <- mm_weekly[mm_weekly$geo == "north", ]
  north <- north[order(north$date), ]

  rec <- recipes::recipe(revenue ~ ., data = north) |>
    step_adstock(tv, video, index = "date", decay = 0.7) |>
    recipes::prep()

  baked_train <- recipes::bake(rec, new_data = north)
  juiced <- recipes::juice(rec)

  expect_identical(nrow(baked_train), nrow(juiced))
  expect_equal(baked_train$tv, juiced$tv, tolerance = 1e-10)
  expect_equal(baked_train$video, juiced$video, tolerance = 1e-10)

  # and this is *not* what a (wrong) warm start on the training data would give:
  # warm-starting on top of the already-learned terminal state would double
  # the effective carryover, which is not what juice()/prep() produced
  wrong_warm <- adstock_geometric(north$tv, decay = 0.7,
                                   state = adstock_state(north$tv, decay = 0.7))
  expect_false(isTRUE(all.equal(juiced$tv, wrong_warm)))
})

# ---- index required ----------------------------------------------------------

test_that("omitting index errors at prep time", {
  skip_if_not_installed("recipes")
  d <- data.frame(y = 1:5, x = c(1, 2, 3, 4, 5), t = 1:5)
  rec <- recipes::recipe(y ~ ., data = d) |> step_adstock(x, decay = 0.5)
  expect_error(recipes::prep(rec), "index.*required")
})

# ---- duplicated index ---------------------------------------------------------

test_that("a duplicated index errors at prep time", {
  skip_if_not_installed("recipes")
  d <- data.frame(y = 1:5, x = c(1, 2, 3, 4, 5), t = c(1, 2, 2, 4, 5))
  rec <- recipes::recipe(y ~ ., data = d) |>
    step_adstock(x, index = "t", decay = 0.5)
  expect_error(recipes::prep(rec), "repeated values")
})

# ---- unevenly spaced index warns ----------------------------------------------

test_that("an unevenly spaced index warns at prep time", {
  skip_if_not_installed("recipes")
  d <- data.frame(y = 1:6, x = c(1, 2, 3, 4, 5, 6), t = c(1, 2, 3, 10, 11, 12))
  rec <- recipes::recipe(y ~ ., data = d) |>
    step_adstock(x, index = "t", decay = 0.5)
  expect_warning(recipes::prep(rec), "unevenly spaced")
})

# ---- unseen group warns and cold-starts ---------------------------------------

test_that("an unseen group warns and cold-starts at bake time", {
  skip_if_not_installed("recipes")
  train <- data.frame(y = 1:6, x = c(10, 20, 30, 40, 50, 60), t = 1:6,
                       g = rep("A", 6))
  rec <- recipes::recipe(y ~ ., data = train) |>
    step_adstock(x, index = "t", by = "g", decay = 0.5) |>
    recipes::prep()

  new_group <- data.frame(y = 1:3, x = c(100, 200, 300), t = 7:9,
                           g = rep("B", 3))
  expect_warning(recipes::bake(rec, new_data = new_group), "not seen during")

  baked <- suppressWarnings(recipes::bake(rec, new_data = new_group))
  fresh <- adstock_geometric(c(100, 200, 300), decay = 0.5)
  expect_equal(baked$x, fresh, tolerance = 1e-10)
})

# ---- a gap warns and cold-starts -----------------------------------------------

test_that("a gap after the training period warns and cold-starts at bake time", {
  skip_if_not_installed("recipes")
  train <- data.frame(y = 1:6, x = c(10, 20, 30, 40, 50, 60), t = 1:6,
                       g = rep("A", 6))
  rec <- recipes::recipe(y ~ ., data = train) |>
    step_adstock(x, index = "t", by = "g", decay = 0.5) |>
    recipes::prep()

  gapped <- data.frame(y = 1:3, x = c(100, 200, 300), t = 20:22,
                        g = rep("A", 3))
  expect_warning(recipes::bake(rec, new_data = gapped),
                 "[Gg]ap|Cold-starting")

  baked <- suppressWarnings(recipes::bake(rec, new_data = gapped))
  fresh <- adstock_geometric(c(100, 200, 300), decay = 0.5)
  expect_equal(baked$x, fresh, tolerance = 1e-10)
})

# ---- carry_over = "warm": errors on a gap, but preps successfully -------------

test_that("carry_over = 'warm' errors on a genuine gap but preps successfully", {
  skip_if_not_installed("recipes")
  train <- data.frame(y = 1:6, x = c(10, 20, 30, 40, 50, 60), t = 1:6,
                       g = rep("A", 6))
  rec <- recipes::recipe(y ~ ., data = train) |>
    step_adstock(x, index = "t", by = "g", decay = 0.5, carry_over = "warm")

  # prep() re-bakes the training data internally (an overlap, not a gap), so
  # this must succeed even though carry_over = "warm" is otherwise strict
  prepped <- expect_no_error(recipes::prep(rec))

  gapped <- data.frame(y = 1:3, x = c(100, 200, 300), t = 20:22,
                        g = rep("A", 3))
  expect_error(recipes::bake(prepped, new_data = gapped),
               'carry_over = "warm"')

  unseen <- data.frame(y = 1:3, x = c(100, 200, 300), t = 7:9,
                        g = rep("B", 3))
  expect_error(recipes::bake(prepped, new_data = unseen),
               'carry_over = "warm"')

  # but a genuinely contiguous continuation warm-starts without error or warning
  contig <- data.frame(y = 1:3, x = c(100, 200, 300), t = 7:9, g = rep("A", 3))
  baked <- expect_no_warning(recipes::bake(prepped, new_data = contig))
  expect_equal(baked$x,
               adstock_geometric(c(10, 20, 30, 40, 50, 60, 100, 200, 300),
                                  decay = 0.5)[7:9],
               tolerance = 1e-10)
})

# ---- carry_over = "cold": always a fresh filter -------------------------------

test_that("carry_over = 'cold' always equals a fresh filter on the new data alone", {
  skip_if_not_installed("recipes")
  train <- data.frame(y = 1:6, x = c(10, 20, 30, 40, 50, 60), t = 1:6,
                       g = rep("A", 6))
  rec <- recipes::recipe(y ~ ., data = train) |>
    step_adstock(x, index = "t", by = "g", decay = 0.5, carry_over = "cold") |>
    recipes::prep()

  contig <- data.frame(y = 1:3, x = c(100, 200, 300), t = 7:9, g = rep("A", 3))
  baked <- expect_no_warning(recipes::bake(rec, new_data = contig))
  fresh <- adstock_geometric(c(100, 200, 300), decay = 0.5)
  expect_equal(baked$x, fresh, tolerance = 1e-10)
})

# ---- skip must remain FALSE ----------------------------------------------------

test_that("skip = TRUE errors, because adstock must run at prediction time", {
  skip_if_not_installed("recipes")
  d <- data.frame(y = 1:5, x = c(1, 2, 3, 4, 5), t = 1:5)
  expect_error(
    step_adstock(recipes::recipe(y ~ ., data = d), x, index = "t", skip = TRUE),
    "skip.*FALSE"
  )
})

# ---- tidy(): one row per column per group --------------------------------------

test_that("tidy.step_adstock() returns one row per column per group", {
  skip_if_not_installed("recipes")
  set.seed(1)
  d <- data.frame(y = 1:12, x1 = runif(12), x2 = runif(12), t = 1:12,
                   g = rep(c("A", "B"), each = 6))
  rec <- recipes::recipe(y ~ ., data = d) |>
    step_adstock(x1, x2, index = "t", by = "g", decay = 0.4) |>
    recipes::prep()

  td <- generics::tidy(rec, number = 1)
  expect_identical(nrow(td), 4L)  # 2 columns x 2 groups
  expect_identical(sort(unique(td$terms)), c("x1", "x2"))
  expect_identical(sort(unique(td$group)), c("A", "B"))
  expect_true(all(td$decay == 0.4))
  expect_true(is.numeric(td$state))
  expect_false(anyNA(td$state))

  # untrained step: one row per selected term, with no learned state yet
  rec_unprepped <- recipes::recipe(y ~ ., data = d) |>
    step_adstock(x1, x2, index = "t", by = "g", decay = 0.4)
  td_unprepped <- generics::tidy(rec_unprepped, number = 1)
  expect_identical(sort(td_unprepped$terms), c("x1", "x2"))
  expect_true(all(is.na(td_unprepped$state)))
})

# ---- invariant 3: bake() preserves row count and order -------------------------

test_that("bake.step_adstock preserves row count and row order", {
  skip_if_not_installed("recipes")
  set.seed(2)
  n_per_group <- 20
  # a proper two-series panel: each group has its own contiguous, evenly
  # spaced time index, so no "unevenly spaced" warning fires
  d <- data.frame(
    y = rnorm(2 * n_per_group),
    x = runif(2 * n_per_group, 0, 100),
    t = rep(seq_len(n_per_group), 2),
    g = rep(c("A", "B"), each = n_per_group)
  )
  d <- d[sample(nrow(d)), ]  # shuffle rows so index order != row order
  rownames(d) <- NULL

  rec <- recipes::recipe(y ~ ., data = d) |>
    step_adstock(x, index = "t", by = "g", decay = 0.5) |>
    recipes::prep()

  baked <- expect_no_warning(recipes::bake(rec, new_data = d))
  expect_identical(nrow(baked), nrow(d))
  expect_identical(baked$t, d$t)
  expect_identical(as.character(baked$g), as.character(d$g))
})

# ---- tunable() ------------------------------------------------------------------

test_that("tunable.step_adstock() returns the documented data frame", {
  skip_if_not_installed("recipes")
  set.seed(7)
  d <- data.frame(y = 1:10, x = runif(10), t = 1:10)
  rec <- recipes::recipe(y ~ ., data = d) |>
    step_adstock(x, index = "t", decay = 0.5) |>
    recipes::prep()

  tb <- generics::tunable(rec$steps[[1]])
  expect_identical(names(tb), c("name", "call_info", "source", "component",
                                 "component_id"))
  expect_setequal(tb$name, c("decay", "max_lag"))
  expect_true(all(tb$source == "recipe"))
  expect_true(all(tb$component == "step_adstock"))
  expect_identical(unique(tb$component_id), rec$steps[[1]]$id)
  expect_identical(tb$call_info[[1]]$fun, "carryover_decay")
  expect_identical(tb$call_info[[2]]$fun, "carryover_max_lag")
})

# ---- dials parameter objects ----------------------------------------------------

test_that("carryover_decay() and carryover_max_lag() are valid dials params usable in grid_regular()", {
  skip_if_not_installed("dials")
  decay_p <- carryover_decay()
  lag_p <- carryover_max_lag()
  expect_s3_class(decay_p, "quant_param")
  expect_s3_class(lag_p, "quant_param")

  vals <- dials::value_seq(decay_p, 5)
  expect_length(vals, 5)
  expect_true(all(vals >= 0 & vals <= 0.95))

  g <- dials::grid_regular(decay_p, lag_p, levels = 3)
  expect_identical(nrow(g), 9L)
  expect_true(all(g$decay >= 0 & g$decay <= 0.95))
  expect_true(all(g$max_lag >= 1 & g$max_lag <= 26))
})

# ---- new data that overlaps the end of training ---------------------------------

# Adstock of `cn` filtered over each geo's whole series, aligned to `data`'s rows.
whole_series <- function(data, cn, ...) {
  out <- numeric(nrow(data))
  for (g in unique(data$geo)) {
    rows <- which(data$geo == g)
    rows <- rows[order(data$date[rows])]
    out[rows] <- adstock_geometric(data[[cn]][rows], ...)
  }
  out
}

test_that("new data that repeats the end of training gets the whole-series values", {
  skip_if_not_installed("recipes")
  data(mm_weekly)
  north <- mm_weekly[mm_weekly$geo == "north", ]
  north <- north[order(north$date), ]
  full <- adstock_geometric(north$tv, decay = 0.85)

  for (carry in c("auto", "warm")) {
    rec <- recipes::recipe(revenue ~ ., data = north[1:104, ]) |>
      step_adstock(tv, index = "date", decay = 0.85, carry_over = carry) |>
      recipes::prep()
    baked <- expect_no_warning(recipes::bake(rec, new_data = north[104:156, ]))
    expect_equal(baked$tv, full[104:156], tolerance = 1e-10, info = carry)
  }

  # carry_over = "cold" still restarts from zero
  cold <- recipes::recipe(revenue ~ ., data = north[1:104, ]) |>
    step_adstock(tv, index = "date", decay = 0.85, carry_over = "cold") |>
    recipes::prep()
  expect_equal(recipes::bake(cold, new_data = north[104:156, ])$tv,
               adstock_geometric(north$tv[104:156], decay = 0.85),
               tolerance = 1e-10)
})

test_that("a panel assessment set with lag rows matches the whole series per geo", {
  skip_if_not_installed("recipes")
  data(mm_weekly)
  dates <- sort(unique(mm_weekly$date))
  train <- mm_weekly[mm_weekly$date <= dates[104], ]
  # four weeks of training repeated at the start, as rolling_origin(lag = 4)
  assess <- mm_weekly[mm_weekly$date > dates[100], ]

  for (lag in c(Inf, 4)) {
    rec <- recipes::recipe(revenue ~ ., data = train) |>
      step_adstock(tv, video, index = "date", by = "geo", decay = 0.6,
                   max_lag = lag) |>
      recipes::prep()
    baked <- expect_no_warning(recipes::bake(rec, new_data = assess))
    for (cn in c("tv", "video")) {
      expected <- whole_series(mm_weekly, cn, decay = 0.6, max_lag = lag)
      expect_equal(baked[[cn]], expected[mm_weekly$date > dates[100]],
                   tolerance = 1e-10, info = paste(cn, lag))
    }
  }
})

test_that("rsample::rolling_origin(lag =) assessment sets are warm-started exactly", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("rsample")
  data(mm_weekly)
  north <- mm_weekly[mm_weekly$geo == "north", ]
  north <- north[order(north$date), ]
  full <- adstock_geometric(north$tv, decay = 0.85)

  folds <- rsample::rolling_origin(north, initial = 104, assess = 13,
                                   skip = 12, lag = 4, cumulative = TRUE)
  for (i in seq_len(nrow(folds))) {
    sp <- folds$splits[[i]]
    rec <- recipes::recipe(revenue ~ ., data = rsample::analysis(sp)) |>
      step_adstock(tv, index = "date", decay = 0.85, carry_over = "warm") |>
      recipes::prep()
    assess <- rsample::assessment(sp)
    baked <- recipes::bake(rec, new_data = assess)
    expect_equal(baked$tv, full[match(assess$date, north$date)],
                 tolerance = 1e-10)
  }
})

test_that("baking a block from the middle of training reproduces the prepared rows", {
  skip_if_not_installed("recipes")
  data(mm_weekly)
  north <- mm_weekly[mm_weekly$geo == "north", ]
  north <- north[order(north$date), ]
  for (lag in c(Inf, 3)) {
    rec <- recipes::recipe(revenue ~ ., data = north[1:104, ]) |>
      step_adstock(tv, index = "date", decay = 0.7, max_lag = lag,
                   normalise = FALSE) |>
      recipes::prep()
    baked <- expect_no_warning(recipes::bake(rec, new_data = north[50:60, ]))
    expect_equal(baked$tv, recipes::juice(rec)$tv[50:60], tolerance = 1e-10,
                 info = lag)
  }
})

# ---- a block that does not line up with the training periods --------------------

test_that("a block starting part-way through a period warns and cold-starts, or errors under warm", {
  skip_if_not_installed("recipes")
  tr <- data.frame(date = seq(as.Date("2024-01-01"), by = "week",
                              length.out = 20),
                   x = 100 + 10 * sin(1:20), y = 1)
  # weekly, but starting the day after the last training Monday
  te <- data.frame(date = seq(max(tr$date) + 1, by = "week", length.out = 6),
                   x = 100, y = 1)

  rec <- recipes::recipe(y ~ ., data = tr) |>
    step_adstock(x, index = "date", decay = 0.8) |>
    recipes::prep()
  expect_warning(baked <- recipes::bake(rec, new_data = te),
                 "does not line up")
  expect_equal(baked$x, adstock_geometric(te$x, decay = 0.8), tolerance = 1e-10)

  rec_warm <- recipes::recipe(y ~ ., data = tr) |>
    step_adstock(x, index = "date", decay = 0.8, carry_over = "warm") |>
    recipes::prep()
  expect_error(recipes::bake(rec_warm, new_data = te),
               "less than one period after")
})

# ---- the period is inferred within each group ------------------------------------

test_that("geos reporting on different weekdays keep their own weekly period", {
  skip_if_not_installed("recipes")
  mk <- function(start, g) {
    data.frame(date = seq(as.Date(start), by = "week", length.out = 30),
               geo = g, x = 50 + 20 * cos(1:30), y = 1)
  }
  d <- rbind(mk("2024-01-01", "a"), mk("2024-01-04", "b"))
  cut <- as.Date("2024-05-20")

  rec <- expect_no_warning(
    recipes::recipe(y ~ ., data = d[d$date < cut, ]) |>
      step_adstock(x, index = "date", by = "geo", decay = 0.8) |>
      recipes::prep()
  )
  expect_identical(vapply(rec$steps[[1]]$history, `[[`, numeric(1), "period"),
                   c(a = 7, b = 7))
  baked <- expect_no_warning(recipes::bake(rec, new_data = d[d$date >= cut, ]))
  expect_equal(baked$x, whole_series(d, "x", decay = 0.8)[d$date >= cut],
               tolerance = 1e-10)
})

test_that("period sets the spacing when the training data cannot show it", {
  skip_if_not_installed("recipes")
  # one training week per geo: no spacing to infer
  tr <- data.frame(date = as.Date("2024-01-01") + c(0, 0), geo = c("a", "b"),
                   x = c(10, 20), y = 1)
  te <- data.frame(date = as.Date("2024-01-08") + c(0, 0), geo = c("a", "b"),
                   x = c(5, 5), y = 1)
  warm <- c(adstock_geometric(c(10, 5), decay = 0.5)[2],
            adstock_geometric(c(20, 5), decay = 0.5)[2])

  expect_warning(
    rec <- recipes::recipe(y ~ ., data = tr) |>
      step_adstock(x, index = "date", by = "geo", decay = 0.5) |>
      recipes::prep(),
    "could not be inferred"
  )
  expect_warning(recipes::bake(rec, new_data = te), "spacing is unknown")

  for (p in list(7, as.difftime(1, units = "weeks"))) {
    rec_p <- expect_no_warning(
      recipes::recipe(y ~ ., data = tr) |>
        step_adstock(x, index = "date", by = "geo", decay = 0.5, period = p) |>
        recipes::prep()
    )
    baked <- expect_no_warning(recipes::bake(rec_p, new_data = te))
    expect_equal(baked$x, warm, tolerance = 1e-10)
  }

  # a difftime is converted to seconds for a POSIXct index
  hours <- as.POSIXct("2024-01-01 00:00", tz = "UTC") + 3600 * (0:5)
  hourly <- data.frame(t = hours, x = c(1, 0, 0, 0, 2, 0), y = 1)
  rec_h <- recipes::recipe(y ~ ., data = hourly[1:3, ]) |>
    step_adstock(x, index = "t", decay = 0.5,
                 period = as.difftime(1, units = "hours")) |>
    recipes::prep()
  baked_h <- expect_no_warning(recipes::bake(rec_h, new_data = hourly[4:6, ]))
  expect_equal(baked_h$x, adstock_geometric(hourly$x, decay = 0.5)[4:6],
               tolerance = 1e-10)
})

test_that("period is validated", {
  skip_if_not_installed("recipes")
  d <- data.frame(y = 1:5, x = c(1, 2, 3, 4, 5), t = 1:5)
  rec <- recipes::recipe(y ~ ., data = d)
  for (bad in list(-1, 0, NA_real_, Inf, c(1, 2), "week")) {
    expect_error(step_adstock(rec, x, index = "t", period = bad), "period")
  }
  # a difftime needs a date index to give it units
  expect_error(
    recipes::prep(step_adstock(rec, x, index = "t",
                               period = as.difftime(7, units = "days"))),
    "Date"
  )
})

# ---- index and group checks ------------------------------------------------------

test_that("a missing time point aborts at prep and at bake, naming the group", {
  skip_if_not_installed("recipes")
  d <- data.frame(y = 1, x = 1:6, t = c(1:3, 1:3), g = rep(c("A", "B"), each = 3))
  d_na <- d
  d_na$t[5] <- NA
  expect_error(
    recipes::prep(recipes::recipe(y ~ ., data = d_na) |>
                    step_adstock(x, index = "t", by = "g")),
    "In group \"B\""
  )

  rec <- recipes::recipe(y ~ ., data = d) |>
    step_adstock(x, index = "t", by = "g") |>
    recipes::prep()
  new <- data.frame(y = 1, x = 1:2, t = c(4, NA), g = c("A", "A"))
  expect_error(recipes::bake(rec, new_data = new), "missing or infinite")
})

test_that("a missing group label aborts rather than joining a group called NA", {
  skip_if_not_installed("recipes")
  d <- data.frame(y = 1, x = c(100, 50, 25, 50, 25, 12.5), t = c(1:3, 1:3),
                  region = c("NA", "NA", "NA", NA, NA, NA))
  expect_error(
    recipes::prep(recipes::recipe(y ~ ., data = d) |>
                    step_adstock(x, index = "t", by = "region")),
    "missing value"
  )

  ok <- d
  ok$region <- rep(c("NA", "EU"), each = 3)
  rec <- recipes::recipe(y ~ ., data = ok) |>
    step_adstock(x, index = "t", by = "region") |>
    recipes::prep()
  new <- data.frame(y = 1, x = 1, t = 4, region = NA_character_)
  expect_error(recipes::bake(rec, new_data = new), "missing value")
})

test_that("numeric group ids match whatever their storage type", {
  skip_if_not_installed("recipes")
  tr <- data.frame(y = 1, x = c(10, 20, 30, 1, 2, 3), t = c(1:3, 1:3),
                   geo = rep(c(1e5, 2e5), each = 3))
  te <- data.frame(y = 1, x = c(5, 5), t = c(4, 4),
                   geo = c(100000L, 200000L))
  rec <- recipes::recipe(y ~ ., data = tr) |>
    step_adstock(x, index = "t", by = "geo", decay = 0.5) |>
    recipes::prep()
  baked <- expect_no_warning(recipes::bake(rec, new_data = te))
  expect_equal(baked$x, c(adstock_geometric(c(10, 20, 30, 5), 0.5)[4],
                          adstock_geometric(c(1, 2, 3, 5), 0.5)[4]),
               tolerance = 1e-10)
  expect_identical(generics::tidy(rec, number = 1)$group,
                   c("100000", "200000"))
})

test_that("a blank group label is a group like any other", {
  skip_if_not_installed("recipes")
  tr <- data.frame(y = 1, x = c(10, 20, 30, 1, 2, 3), t = c(1:3, 1:3),
                   g = rep(c("", "a"), each = 3))
  te <- data.frame(y = 1, x = c(5, 5), t = c(4, 4), g = c("", "a"))
  rec <- recipes::recipe(y ~ ., data = tr) |>
    step_adstock(x, index = "t", by = "g", decay = 0.5) |>
    recipes::prep()
  baked <- expect_no_warning(recipes::bake(rec, new_data = te))
  expect_equal(baked$x, c(adstock_geometric(c(10, 20, 30, 5), 0.5)[4],
                          adstock_geometric(c(1, 2, 3, 5), 0.5)[4]),
               tolerance = 1e-10)
})

# ---- argument checks -----------------------------------------------------------------

test_that("tol must lie strictly between 0 and 0.5", {
  skip_if_not_installed("recipes")
  d <- data.frame(y = 1:5, x = c(1, 2, 3, 4, 5), t = 1:5)
  rec <- recipes::recipe(y ~ ., data = d)
  for (bad in c(0, 0.5, 1)) {
    expect_error(step_adstock(rec, x, index = "t", tol = bad), "tol")
  }
  expect_no_error(step_adstock(rec, x, index = "t", tol = 0.49))
})

test_that("print() and tidy() work on a step with tune() placeholders", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("hardhat")
  d <- data.frame(y = 1:5, x = c(1, 2, 3, 4, 5), t = 1:5)
  rec <- recipes::recipe(y ~ ., data = d) |>
    step_adstock(x, index = "t", decay = hardhat::tune(),
                 max_lag = hardhat::tune())
  shown <- capture.output(print(rec$steps[[1]]), type = "message")
  expect_match(paste(shown, collapse = " "),
               "decay = tune(), max_lag = tune()", fixed = TRUE)
  td <- generics::tidy(rec, number = 1)
  expect_identical(td$terms, "x")
  expect_true(is.na(td$decay))
})

# ---- messages and tidy() output -------------------------------------------------------

test_that("an ungrouped step does not name an internal group in its messages", {
  skip_if_not_installed("recipes")
  tr <- data.frame(y = 1:6, x = c(10, 20, 30, 40, 50, 60), t = 1:6)
  rec <- recipes::recipe(y ~ ., data = tr) |>
    step_adstock(x, index = "t", decay = 0.5) |>
    recipes::prep()
  gapped <- data.frame(y = 1:3, x = c(100, 200, 300), t = 20:22)
  w <- tryCatch(recipes::bake(rec, new_data = gapped),
                warning = function(w) conditionMessage(w))
  expect_match(w, "gap of 13 periods")
  expect_no_match(w, ".all", fixed = TRUE)

  rec_warm <- recipes::recipe(y ~ ., data = tr) |>
    step_adstock(x, index = "t", decay = 0.5, carry_over = "warm") |>
    recipes::prep()
  e <- tryCatch(recipes::bake(rec_warm, new_data = gapped),
                error = function(e) conditionMessage(e))
  expect_match(e, "starts after a gap")
  expect_no_match(e, ".all", fixed = TRUE)
  expect_no_match(e, "is gap", fixed = TRUE)
})

test_that("messages show multi-column group labels legibly", {
  skip_if_not_installed("recipes")
  d <- data.frame(y = 1, x = 1:4, t = c(1, 2, 2, 3), geo = "north",
                  size = "big")
  e <- tryCatch(
    recipes::prep(recipes::recipe(y ~ ., data = d) |>
                    step_adstock(x, index = "t", by = c("geo", "size"))),
    error = function(e) conditionMessage(e)
  )
  expect_match(e, "north / big", fixed = TRUE)
  expect_no_match(e, "\x1f", fixed = TRUE)
})

test_that("missing and infinite media values are reported by column name", {
  skip_if_not_installed("recipes")
  d <- data.frame(y = 1:5, tv = c(1, NA, 3, 4, 5), t = 1:5)
  expect_error(
    recipes::prep(recipes::recipe(y ~ ., data = d) |>
                    step_adstock(tv, index = "t", na_action = "error")),
    "missing values: \"tv\""
  )
  rec <- recipes::recipe(y ~ ., data = data.frame(y = 1:3, tv = 1:3, t = 1:3)) |>
    step_adstock(tv, index = "t", na_action = "error") |>
    recipes::prep()
  new <- data.frame(y = 1:2, tv = c(NA, 7), t = 4:5)
  expect_error(recipes::bake(rec, new_data = new), "missing values: \"tv\"")

  d_inf <- d
  d_inf$tv[2] <- Inf
  expect_error(
    recipes::prep(recipes::recipe(y ~ ., data = d_inf) |>
                    step_adstock(tv, index = "t")),
    "infinite values: \"tv\""
  )
})

test_that("tidy() returns a tibble, with NA for the group of an ungrouped step", {
  skip_if_not_installed("recipes")
  d <- data.frame(y = 1:6, x = c(1, 2, 3, 4, 5, 6), t = 1:6)
  rec <- recipes::recipe(y ~ ., data = d) |>
    step_adstock(x, index = "t", decay = 0.5) |>
    recipes::prep()
  td <- generics::tidy(rec, number = 1)
  expect_s3_class(td, "tbl_df")
  expect_true(is.na(td$group))
  expect_equal(td$state, as.numeric(adstock_state(d$x, decay = 0.5)))
})

test_that("an empty selection says that nothing matched", {
  skip_if_not_installed("recipes")
  d <- data.frame(y = 1:5, x = c(1, 2, 3, 4, 5), t = 1:5)
  expect_error(
    recipes::prep(recipes::recipe(y ~ ., data = d) |>
                    step_adstock(recipes::all_nominal_predictors(),
                                 index = "t")),
    "matched no columns"
  )
  expect_error(
    recipes::prep(recipes::recipe(y ~ ., data = d) |>
                    step_adstock(t, index = "t")),
    "only to `index` and `by`"
  )
})

test_that("a step prepared without the stored series asks to be prepared again", {
  skip_if_not_installed("recipes")
  d <- data.frame(y = 1:6, x = c(1, 2, 3, 4, 5, 6), t = 1:6)
  rec <- recipes::recipe(y ~ ., data = d) |>
    step_adstock(x, index = "t", decay = 0.5) |>
    recipes::prep()
  rec$steps[[1]]$history <- NULL
  expect_error(recipes::bake(rec, new_data = d), "earlier version")
})

# ---- bake() scales linearly in the number of groups -----------------------------------

test_that("bake() copies each column once, not once per group", {
  skip_if_not_installed("recipes")
  skip_if_not(capabilities("profmem"))
  set.seed(5)
  n_groups <- 60
  d <- data.frame(y = 1, x = runif(n_groups * 3), t = rep(1:3, n_groups),
                  g = sprintf("g%02d", rep(seq_len(n_groups), each = 3)))
  rec <- recipes::recipe(y ~ ., data = d) |>
    step_adstock(x, index = "t", by = "g", decay = 0.5) |>
    recipes::prep()
  new <- d
  new$t <- new$t + 3
  tracemem(new$x)
  on.exit(untracemem(new$x), add = TRUE)
  copies <- capture.output(baked <- recipes::bake(rec, new_data = new))
  expect_lt(sum(grepl("tracemem", copies)), 5L)
  expect_identical(nrow(baked), nrow(new))
})

# ---- shared helpers in R/zzz.R -------------------------------------------------------

test_that(".mm_step_groups orders groups by first appearance, not by locale", {
  groups <- mediamix:::.mm_step_groups(data.frame(g = c("b", "a", "B", "a")),
                                       "g")
  expect_identical(names(groups), c("b", "a", "B"))
  expect_identical(unname(groups), list(1L, c(2L, 4L), 3L))
})

test_that("group keys do not depend on storage type or time zone", {
  key_part <- mediamix:::.mm_key_part
  expect_identical(key_part(1e5, "geo"), key_part(100000L, "geo"))
  expect_identical(key_part(c(1e5, 2.5), "geo"), c("100000", "2.5"))
  utc <- as.POSIXct("2024-01-01 05:00", tz = "UTC")
  nyc <- as.POSIXct("2024-01-01 00:00", tz = "America/New_York")
  expect_identical(key_part(utc, "launch"), key_part(nyc, "launch"))
  expect_error(key_part(c("a", NA), "region"), "\"region\" has 1 missing")
})

test_that("the missing-column message does not assume a recipe step", {
  e <- tryCatch(mediamix:::.mm_assert_cols(data.frame(a = 1), c("a", "zz")),
                error = function(e) conditionMessage(e))
  expect_match(e, "1 column is not in the data")
  expect_no_match(e, "step")
})

test_that("the missing-package message agrees in number", {
  abort_missing <- mediamix:::.mm_abort_missing_recipes
  expect_error(abort_missing(c("recipes", "rlang")), "Install them")
  expect_error(abort_missing("rlang"), "Install it")
  expect_null(abort_missing(character(0)))
})

# ---- kernels and steady-state starts ---------------------------------------------

# Each kernel as step_adstock() arguments, the same kernel as a function of a
# series and a starting state, and the steady state that step_adstock() starts
# a series from under start = "steady".
kernel_cases <- list(
  geometric = list(
    args = list(decay = 0.7),
    run = function(x, state = 0) adstock_geometric(x, decay = 0.7, state = state),
    seed = function(x) adstock_steady_state(x, decay = 0.7)
  ),
  geometric_finite = list(
    args = list(decay = 0.7, max_lag = 5),
    run = function(x, state = 0) {
      adstock_geometric(x, decay = 0.7, max_lag = 5, state = state)
    },
    seed = function(x) adstock_steady_state(x, decay = 0.7, max_lag = 5)
  ),
  weibull_cdf = list(
    args = list(kernel = "weibull", shape = 2, scale = 3, max_lag = 8),
    run = function(x, state = 0) {
      adstock_weibull(x, shape = 2, scale = 3, max_lag = 8, state = state)
    },
    seed = function(x) adstock_steady_state(x, max_lag = 8)
  ),
  weibull_pdf = list(
    args = list(kernel = "weibull", shape = 2, scale = 3, max_lag = 8,
                type = "pdf", normalise = FALSE),
    run = function(x, state = 0) {
      adstock_weibull(x, shape = 2, scale = 3, max_lag = 8, type = "pdf",
                      normalise = FALSE, state = state)
    },
    seed = function(x) adstock_steady_state(x, max_lag = 8)
  ),
  delayed = list(
    args = list(kernel = "delayed", decay = 0.6, peak = 2, max_lag = 8),
    run = function(x, state = 0) {
      adstock_delayed(x, decay = 0.6, peak = 2, max_lag = 8, state = state)
    },
    seed = function(x) adstock_steady_state(x, max_lag = 8)
  )
)

# A one-column step_adstock() recipe on `data` with a kernel case's arguments.
kernel_recipe <- function(data, case, ...) {
  rec <- recipes::recipe(revenue ~ ., data = data)
  do.call(step_adstock, c(list(rec, quote(tv), index = "date"), case$args,
                          list(...)))
}

test_that("every kernel and start bakes an assessment block as the whole series", {
  skip_if_not_installed("recipes")
  data(mm_weekly)
  north <- mm_weekly[mm_weekly$geo == "north", ]
  north <- north[order(north$date), ]

  for (nm in names(kernel_cases)) {
    case <- kernel_cases[[nm]]
    for (start in c("zero", "steady")) {
      info <- paste(nm, start)
      state <- if (start == "steady") case$seed(north$tv[1:104]) else 0
      full <- case$run(north$tv, state = state)
      rec <- recipes::prep(kernel_recipe(north[1:104, ], case, start = start))

      # straight after the analysis block, and repeating its last four weeks
      after <- expect_no_warning(recipes::bake(rec, new_data = north[105:156, ]))
      expect_equal(after$tv, full[105:156], tolerance = 1e-10, info = info)
      lagged <- expect_no_warning(recipes::bake(rec, new_data = north[101:156, ]))
      expect_equal(lagged$tv, full[101:156], tolerance = 1e-10, info = info)

      # re-baking the training data, or a block from its middle
      expect_equal(recipes::bake(rec, new_data = north[1:104, ])$tv,
                   recipes::juice(rec)$tv, tolerance = 1e-10, info = info)
      expect_equal(recipes::juice(rec)$tv, full[1:104], tolerance = 1e-10,
                   info = info)
      expect_equal(recipes::bake(rec, new_data = north[3:20, ])$tv,
                   full[3:20], tolerance = 1e-10, info = info)
    }
  }
})

test_that("every kernel and start warm-starts rolling_origin(lag =) splits exactly", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("rsample")
  data(mm_weekly)
  north <- mm_weekly[mm_weekly$geo == "north", ]
  north <- north[order(north$date), ]
  folds <- rsample::rolling_origin(north, initial = 104, assess = 13,
                                   skip = 12, lag = 4, cumulative = TRUE)

  for (nm in names(kernel_cases)) {
    case <- kernel_cases[[nm]]
    for (start in c("zero", "steady")) {
      # Every analysis set starts in the first week, so each split seeds the
      # series from the same first periods as the whole series does.
      state <- if (start == "steady") case$seed(north$tv) else 0
      full <- case$run(north$tv, state = state)
      for (i in seq_len(nrow(folds))) {
        sp <- folds$splits[[i]]
        rec <- recipes::prep(kernel_recipe(rsample::analysis(sp), case,
                                           start = start, carry_over = "warm"))
        assess <- rsample::assessment(sp)
        baked <- recipes::bake(rec, new_data = assess)
        expect_equal(baked$tv, full[match(assess$date, north$date)],
                     tolerance = 1e-10, info = paste(nm, start, i))
      }
    }
  }
})

test_that("a panel keeps a steady start and a kernel per geo", {
  skip_if_not_installed("recipes")
  data(mm_weekly)
  dates <- sort(unique(mm_weekly$date))
  train <- mm_weekly[mm_weekly$date <= dates[104], ]
  assess <- mm_weekly[mm_weekly$date > dates[100], ]

  for (nm in c("geometric", "delayed")) {
    case <- kernel_cases[[nm]]
    rec <- recipes::prep(do.call(
      step_adstock,
      c(list(recipes::recipe(revenue ~ ., data = train), quote(tv),
             quote(video), index = "date", by = "geo", start = "steady"),
        case$args)
    ))
    baked <- expect_no_warning(recipes::bake(rec, new_data = assess))
    for (cn in c("tv", "video")) {
      expected <- numeric(nrow(mm_weekly))
      for (g in unique(mm_weekly$geo)) {
        rows <- which(mm_weekly$geo == g)
        rows <- rows[order(mm_weekly$date[rows])]
        x <- mm_weekly[[cn]][rows]
        expected[rows] <- case$run(x, state = case$seed(x[1:104]))
      }
      expect_equal(baked[[cn]], expected[mm_weekly$date > dates[100]],
                   tolerance = 1e-10, info = paste(nm, cn))
    }
  }
})

test_that("a cold start under start = 'steady' seeds the block from its own first periods", {
  skip_if_not_installed("recipes")
  data(mm_weekly)
  north <- mm_weekly[mm_weekly$geo == "north", ]
  north <- north[order(north$date), ]
  own_start <- function(case, x) case$run(x, state = case$seed(x))

  for (nm in c("geometric", "weibull_cdf", "delayed")) {
    case <- kernel_cases[[nm]]
    rec <- recipes::prep(kernel_recipe(north[1:100, ], case, start = "steady"))

    gapped <- north[110:130, ]
    expect_warning(baked <- recipes::bake(rec, new_data = gapped), "gap")
    expect_equal(baked$tv, own_start(case, gapped$tv), tolerance = 1e-10,
                 info = nm)

    cold <- recipes::prep(kernel_recipe(north[1:100, ], case, start = "steady",
                                        carry_over = "cold"))
    contig <- north[101:120, ]
    baked_cold <- expect_no_warning(recipes::bake(cold, new_data = contig))
    expect_equal(baked_cold$tv, own_start(case, contig$tv), tolerance = 1e-10,
                 info = nm)

    # A single row is its own steady state.
    one <- suppressWarnings(recipes::bake(rec, new_data = north[140, ]))
    expect_equal(one$tv, own_start(case, north$tv[140]), tolerance = 1e-10,
                 info = nm)
  }

  # An unseen group starts from its own first periods too, and
  # carry_over = "warm" still refuses a gap.
  case <- kernel_cases$geometric
  two <- mm_weekly[mm_weekly$geo %in% c("north", "south"), ]
  two <- two[order(two$geo, two$date), ]
  rec <- recipes::prep(do.call(
    step_adstock,
    c(list(recipes::recipe(revenue ~ ., data = two[two$geo == "north", ]),
           quote(tv), index = "date", by = "geo", start = "steady"),
      case$args)
  ))
  south <- two[two$geo == "south", ][1:30, ]
  expect_warning(baked <- recipes::bake(rec, new_data = south), "not seen")
  expect_equal(baked$tv, own_start(case, south$tv), tolerance = 1e-10)

  rec_warm <- recipes::prep(kernel_recipe(north[1:100, ], case,
                                          start = "steady",
                                          carry_over = "warm"))
  expect_error(recipes::bake(rec_warm, new_data = north[110:130, ]),
               'carry_over = "warm"')
})

test_that("a cold start under start = 'zero' is unchanged for every kernel", {
  skip_if_not_installed("recipes")
  data(mm_weekly)
  north <- mm_weekly[mm_weekly$geo == "north", ]
  north <- north[order(north$date), ]
  for (nm in names(kernel_cases)) {
    case <- kernel_cases[[nm]]
    rec <- recipes::prep(kernel_recipe(north[1:100, ], case))
    expect_warning(baked <- recipes::bake(rec, new_data = north[110:130, ]),
                   "gap of 9 periods")
    expect_equal(baked$tv, case$run(north$tv[110:130]), tolerance = 1e-10,
                 info = nm)
  }
})

test_that("missing media under start = 'steady' count as zero spend", {
  skip_if_not_installed("recipes")
  d <- data.frame(y = 1, t = 1:8, x = c(10, NA, 30, 40, 50, NA, 70, 80))
  rec <- recipes::recipe(y ~ ., data = d[1:5, ]) |>
    step_adstock(x, index = "t", decay = 0.5, start = "steady") |>
    recipes::prep()
  filled <- c(10, 0, 30, 40, 50, 0, 70, 80)
  seed <- adstock_steady_state(filled[1:5], decay = 0.5)
  full <- adstock_geometric(filled, decay = 0.5, state = seed)
  expect_equal(recipes::juice(rec)$x, full[1:5], tolerance = 1e-10)
  expect_equal(recipes::bake(rec, new_data = d[6:8, ])$x, full[6:8],
               tolerance = 1e-10)
})

test_that("a step prepared before the kernel options bakes as geometric, unchanged", {
  skip_if_not_installed("recipes")
  data(mm_weekly)
  new_fields <- c("kernel", "shape", "scale", "type", "peak", "start",
                  "keep_original_cols", "prefix", "seeds")
  strip <- function(step) {
    for (f in new_fields) step[[f]] <- NULL
    step
  }
  tr <- mm_weekly[mm_weekly$date < as.Date("2025-07-01"), ]
  te <- mm_weekly[mm_weekly$date >= as.Date("2025-04-01"), ]

  for (lag in c(Inf, 4)) {
    rec <- recipes::recipe(revenue ~ ., data = tr) |>
      step_adstock(tv, video, index = "date", by = "geo", decay = 0.6,
                   max_lag = lag) |>
      recipes::prep()
    old <- rec
    old$steps[[1]] <- strip(old$steps[[1]])
    expect_false(any(new_fields %in% names(old$steps[[1]])))
    expect_identical(recipes::bake(old, new_data = te),
                     recipes::bake(rec, new_data = te))
    gapped <- te[te$date >= as.Date("2025-10-01"), ]
    expect_identical(suppressWarnings(recipes::bake(old, new_data = gapped)),
                     suppressWarnings(recipes::bake(rec, new_data = gapped)))
    expect_identical(generics::tidy(old, number = 1),
                     generics::tidy(rec, number = 1))
    expect_identical(generics::tunable(old$steps[[1]]),
                     generics::tunable(rec$steps[[1]]))
    expect_identical(capture.output(print(old$steps[[1]]), type = "message"),
                     capture.output(print(rec$steps[[1]]), type = "message"))
  }

  # An unprepared step from before still prepares as geometric.
  unprepped <- recipes::recipe(revenue ~ ., data = tr) |>
    step_adstock(tv, index = "date", by = "geo", decay = 0.6)
  unprepped$steps[[1]] <- strip(unprepped$steps[[1]])
  expect_identical(recipes::bake(recipes::prep(unprepped), new_data = te)$tv,
                   recipes::bake(recipes::prep(recipes::recipe(revenue ~ .,
                                                               data = tr) |>
                                   step_adstock(tv, index = "date", by = "geo",
                                                decay = 0.6)),
                                 new_data = te)$tv)
})

# ---- keeping the raw columns ---------------------------------------------------------

test_that("keep_original_cols = TRUE keeps the raw spend and adds prefixed columns", {
  skip_if_not_installed("recipes")
  data(mm_weekly)
  set.seed(11)
  tr <- mm_weekly[mm_weekly$date < as.Date("2025-07-01"), ]
  te <- mm_weekly[mm_weekly$date >= as.Date("2025-07-01"), ]
  te <- te[sample(nrow(te)), ]

  in_place <- recipes::recipe(revenue ~ ., data = tr) |>
    step_adstock(tv, search, index = "date", by = "geo", decay = 0.6) |>
    recipes::prep()
  kept <- recipes::recipe(revenue ~ ., data = tr) |>
    step_adstock(tv, search, index = "date", by = "geo", decay = 0.6,
                 keep_original_cols = TRUE) |>
    recipes::prep()

  baked <- expect_no_warning(recipes::bake(kept, new_data = te))
  reference <- recipes::bake(in_place, new_data = te)
  expect_identical(nrow(baked), nrow(te))
  expect_identical(baked$tv, te$tv)
  expect_identical(baked$search, te$search)
  expect_identical(baked$adstock_tv, reference$tv)
  expect_identical(baked$adstock_search, reference$search)
  expect_identical(utils::tail(names(baked), 2),
                   c("adstock_tv", "adstock_search"))

  roles <- summary(kept)
  expect_identical(roles$role[roles$variable == "adstock_tv"], "predictor")
  expect_identical(roles$role[roles$variable == "tv"], "predictor")
  expect_identical(recipes::juice(kept)$tv, tr$tv)
})

test_that("prefix and role name the new columns, and the raw spend can leave the predictors", {
  skip_if_not_installed("recipes")
  data(mm_weekly)
  north <- mm_weekly[mm_weekly$geo == "north", ]
  rec <- recipes::recipe(revenue ~ ., data = north) |>
    recipes::update_role(tv, new_role = "spend") |>
    step_adstock(tv, index = "date", decay = 0.6, keep_original_cols = TRUE,
                 prefix = "carry_", role = "media") |>
    recipes::prep()
  roles <- summary(rec)
  expect_identical(roles$role[roles$variable == "carry_tv"], "media")
  expect_identical(roles$role[roles$variable == "tv"], "spend")
  predictors <- names(recipes::bake(rec, new_data = north,
                                    recipes::all_predictors()))
  expect_false(any(c("tv", "carry_tv") %in% predictors))
  expect_equal(recipes::juice(rec)$carry_tv,
               adstock_geometric(north$tv[order(north$date)],
                                 decay = 0.6)[rank(north$date)],
               tolerance = 1e-10)
})

test_that("keep_original_cols leaves missing raw values as they are", {
  skip_if_not_installed("recipes")
  d <- data.frame(y = 1, t = 1:6, x = c(10, NA, 30, 40, NA, 60))
  rec <- recipes::recipe(y ~ ., data = d[1:4, ]) |>
    step_adstock(x, index = "t", decay = 0.5, keep_original_cols = TRUE) |>
    recipes::prep()
  baked <- recipes::bake(rec, new_data = d[5:6, ])
  expect_identical(baked$x, c(NA, 60))
  expect_equal(baked$adstock_x,
               adstock_geometric(c(10, 0, 30, 40, 0, 60), decay = 0.5)[5:6],
               tolerance = 1e-10)
})

test_that("a new column name that is already taken is an error", {
  skip_if_not_installed("recipes")
  d <- data.frame(y = 1, t = 1:5, x = 1:5, adstock_x = 0)
  expect_error(
    recipes::prep(recipes::recipe(y ~ ., data = d) |>
                    step_adstock(x, index = "t", keep_original_cols = TRUE)),
    "would overwrite 1 existing column: \"adstock_x\""
  )
  # bake() on a recipe keeps only the training columns, so a clash in new data
  # can reach the step only when it is baked on its own.
  rec <- recipes::recipe(y ~ ., data = d[-4]) |>
    step_adstock(x, index = "t", keep_original_cols = TRUE) |>
    recipes::prep()
  expect_error(recipes::bake(rec$steps[[1]], new_data = d), "would overwrite")
})

test_that("a zero-row bake keeps the new columns", {
  skip_if_not_installed("recipes")
  d <- data.frame(y = 1, t = 1:5, x = c(1, 2, 3, 4, 5))
  rec <- recipes::recipe(y ~ ., data = d) |>
    step_adstock(x, index = "t", keep_original_cols = TRUE) |>
    recipes::prep()
  empty <- recipes::bake(rec, new_data = d[0, ])
  expect_identical(nrow(empty), 0L)
  expect_identical(names(empty), c("t", "x", "y", "adstock_x"))
  expect_type(empty$adstock_x, "double")
})

# ---- argument checks for the kernel options -----------------------------------------

test_that("kernel arguments are validated when the step is written", {
  skip_if_not_installed("recipes")
  d <- data.frame(y = 1:5, x = c(1, 2, 3, 4, 5), t = 1:5)
  rec <- recipes::recipe(y ~ ., data = d)
  add <- function(...) step_adstock(rec, x, index = "t", ...)

  expect_error(add(kernel = "weibull", max_lag = 4), "needs `shape` and `scale`")
  expect_error(add(kernel = "weibull", shape = 2, max_lag = 4), "needs `scale`")
  expect_error(add(kernel = "weibull", shape = 2, scale = 3),
               "must be finite for `kernel = \"weibull\"`")
  expect_error(add(kernel = "delayed"), "must be finite")
  expect_error(add(kernel = "weibull", shape = 0, scale = 3, max_lag = 4),
               "shape")
  expect_error(add(kernel = "weibull", shape = 2, scale = -1, max_lag = 4),
               "scale")
  expect_error(add(kernel = "delayed", decay = 0, max_lag = 4),
               "must be in \\(0, 1\\)")
  expect_error(add(kernel = "delayed", decay = 1, max_lag = 4), "decay")
  expect_error(add(kernel = "delayed", decay = 0.5, peak = 4, max_lag = 4),
               "`peak` must be in \\[0, 3\\]")
  expect_error(add(peak = -1), "peak")
  expect_error(add(peak = NA_real_), "peak")
  expect_error(add(kernel = "triangle"), "should be one of")
  expect_error(add(type = "hazard"), "should be one of")
  expect_error(add(start = "warm"), "should be one of")
  expect_error(add(decay = 1, start = "steady"), "no steady state")
  expect_no_error(add(decay = 1, max_lag = 4, start = "steady"))
  expect_error(add(keep_original_cols = NA), "keep_original_cols")
  expect_error(add(keep_original_cols = "yes"), "keep_original_cols")
  for (bad in list("", NA_character_, c("a_", "b_"), 1)) {
    expect_error(add(keep_original_cols = TRUE, prefix = bad), "prefix")
  }
  # a value the kernel ignores is not checked against that kernel's limits
  expect_no_error(add(kernel = "weibull", shape = 2, scale = 3, max_lag = 4,
                      decay = 1))
})

test_that("only the kernel's own arguments accept tune()", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("hardhat")
  tune <- hardhat::tune
  d <- data.frame(y = 1:5, x = c(1, 2, 3, 4, 5), t = 1:5)
  rec <- recipes::recipe(y ~ ., data = d)
  add <- function(...) step_adstock(rec, x, index = "t", ...)

  expect_error(add(kernel = "weibull", shape = 2, scale = 3, max_lag = 4,
                   decay = tune()),
               "`decay` cannot be tuned when `kernel = \"weibull\"`")
  expect_error(add(shape = tune()), "`shape` cannot be tuned")
  expect_error(add(kernel = "delayed", max_lag = 6, scale = tune()),
               "`scale` cannot be tuned")
  expect_error(add(peak = tune()), "`peak` cannot be tuned")
  expect_no_error(add(kernel = "weibull", shape = tune(), scale = tune(),
                      max_lag = tune()))
  expect_no_error(add(kernel = "delayed", decay = tune(), peak = tune(),
                      max_lag = tune()))
})

test_that("prep() checks the values tuning writes into the step", {
  skip_if_not_installed("recipes")
  d <- data.frame(y = 1:6, x = c(1, 2, 3, 4, 5, 6), t = 1:6)
  rec <- recipes::recipe(y ~ ., data = d) |>
    step_adstock(x, index = "t", kernel = "delayed", decay = 0.5, max_lag = 4)
  rec$steps[[1]]$peak <- 5
  expect_error(recipes::prep(rec), "`peak` must be in \\[0, 3\\]")
  rec$steps[[1]]$peak <- 1
  rec$steps[[1]]$decay <- 0
  expect_error(recipes::prep(rec), "decay")
})

# ---- print(), tidy() and tunable() with the kernel options ----------------------------

test_that("print() names the kernel, its parameters and the options in use", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("hardhat")
  d <- data.frame(y = 1:5, x = c(1, 2, 3, 4, 5), t = 1:5)
  rec <- recipes::recipe(y ~ ., data = d)
  # print() wraps long titles, so whitespace is squeezed before matching.
  shown <- function(step) {
    out <- capture.output(print(step$steps[[1]]), type = "message")
    gsub("\\s+", " ", paste(out, collapse = " "))
  }
  expect_match(shown(step_adstock(rec, x, index = "t")),
               "Adstock (decay = 0.5, max_lag = Inf) on", fixed = TRUE)
  expect_match(
    shown(step_adstock(rec, x, index = "t", kernel = "weibull", shape = 2,
                       scale = 3, max_lag = 8, type = "pdf")),
    "Adstock (weibull pdf, shape = 2, scale = 3, max_lag = 8) on", fixed = TRUE
  )
  expect_match(
    shown(step_adstock(rec, x, index = "t", kernel = "delayed", decay = 0.6,
                       peak = 2, max_lag = 8, start = "steady",
                       keep_original_cols = TRUE)),
    "(delayed, decay = 0.6, peak = 2, max_lag = 8, steady start, new columns adstock_*)",
    fixed = TRUE
  )
  expect_match(
    shown(step_adstock(rec, x, index = "t", kernel = "weibull",
                       shape = hardhat::tune(), scale = 3, max_lag = 8)),
    "shape = tune(), scale = 3", fixed = TRUE
  )
})

test_that("tidy() reports the kernel, with NA for the parameters it ignores", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("hardhat")
  data(mm_weekly)
  north <- mm_weekly[mm_weekly$geo == "north", ]
  north <- north[order(north$date), ]
  cols <- c("terms", "group", "kernel", "decay", "max_lag", "shape", "scale",
            "type", "peak", "start", "state", "state_length", "id")

  wb <- recipes::recipe(revenue ~ ., data = north) |>
    step_adstock(tv, index = "date", kernel = "weibull", shape = 2,
                 scale = 3, max_lag = 8) |>
    recipes::prep()
  td <- generics::tidy(wb, number = 1)
  expect_identical(names(td), cols)
  expect_identical(td$kernel, "weibull")
  expect_true(is.na(td$decay))
  expect_equal(c(td$max_lag, td$shape, td$scale), c(8, 2, 3))
  expect_identical(td$type, "cdf")
  expect_true(is.na(td$peak))
  expect_identical(td$state_length, 7L)

  dl <- recipes::recipe(revenue ~ ., data = north) |>
    step_adstock(tv, index = "date", kernel = "delayed", decay = 0.6,
                 peak = 2, max_lag = 8, start = "steady")
  td_untrained <- generics::tidy(dl, number = 1)
  expect_identical(names(td_untrained), cols)
  expect_true(all(is.na(td_untrained[c("shape", "scale", "type", "state")])))
  expect_equal(c(td_untrained$decay, td_untrained$peak), c(0.6, 2))
  expect_identical(td_untrained$start, "steady")

  tuned <- recipes::recipe(revenue ~ ., data = north) |>
    step_adstock(tv, index = "date", kernel = "weibull",
                 shape = hardhat::tune(), scale = 3, max_lag = 8)
  expect_true(is.na(generics::tidy(tuned, number = 1)$shape))

  # Under a steady start the recursive state carries the seed's remainder.
  geo <- recipes::recipe(revenue ~ ., data = north[1:104, ]) |>
    step_adstock(tv, index = "date", decay = 0.7, start = "steady") |>
    recipes::prep()
  seed <- adstock_steady_state(north$tv[1:104], decay = 0.7)
  expect_equal(generics::tidy(geo, number = 1)$state,
               as.numeric(adstock_state(north$tv[1:104], decay = 0.7,
                                        state = seed)))
})

test_that("tunable() offers only the kernel's parameters, over valid ranges", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("hardhat")
  tune <- hardhat::tune
  d <- data.frame(y = 1:5, x = c(1, 2, 3, 4, 5), t = 1:5)
  rec <- recipes::recipe(y ~ ., data = d)
  tb <- function(...) generics::tunable(step_adstock(rec, x, index = "t", ...)$steps[[1]])
  funs <- function(t) vapply(t$call_info, `[[`, character(1), "fun")

  geometric <- tb()
  expect_identical(geometric$name, c("decay", "max_lag"))
  expect_identical(funs(geometric), c("carryover_decay", "carryover_max_lag"))

  weibull <- tb(kernel = "weibull", shape = 2, scale = 3, max_lag = 6)
  expect_identical(weibull$name, c("shape", "scale", "max_lag"))
  expect_identical(funs(weibull),
                   c("carryover_shape", "carryover_scale", "carryover_max_lag"))

  delayed <- tb(kernel = "delayed", max_lag = 3)
  expect_identical(delayed$name, c("decay", "peak", "max_lag"))
  expect_identical(funs(delayed),
                   c("carryover_decay", "carryover_peak", "carryover_max_lag"))
  expect_equal(delayed$call_info[[1]]$range, c(0.05, 0.95))
  expect_equal(delayed$call_info[[2]]$range, c(0, 2))
  expect_null(delayed$call_info[[3]]$range)

  both <- tb(kernel = "delayed", peak = tune(), max_lag = tune())
  expect_null(both$call_info[[2]]$range)
  expect_equal(both$call_info[[3]]$range, c(5L, 26L))
  fixed_peak <- tb(kernel = "delayed", peak = 2.5, max_lag = tune())
  expect_equal(fixed_peak$call_info[[3]]$range, c(4L, 26L))
})

test_that("a tuned kernel's parameter set proposes only valid kernels", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("hardhat")
  skip_if_not_installed("dials")
  tune <- hardhat::tune
  data(mm_weekly)
  north <- mm_weekly[mm_weekly$geo == "north", ]
  north <- north[order(north$date), ]
  base <- recipes::recipe(revenue ~ ., data = north[1:104, ])

  specs <- list(
    weibull = base |>
      step_adstock(tv, index = "date", kernel = "weibull",
                   shape = tune("adstock_shape"), scale = tune(),
                   max_lag = tune(), type = "pdf") |>
      step_saturation(tv, half_max = 0.4, shape = tune()),
    delayed = base |>
      step_adstock(tv, index = "date", kernel = "delayed", decay = tune(),
                   peak = tune(), max_lag = tune(), start = "steady")
  )
  for (nm in names(specs)) {
    rec <- specs[[nm]]
    ps <- hardhat::extract_parameter_set_dials(rec)
    expect_false(any(dials::has_unknowns(ps$object)), info = nm)
    # The corners of the search space are where an invalid kernel would be.
    grid <- dials::grid_regular(ps, levels = 2)
    for (i in seq_len(nrow(grid))) {
      cand <- rec
      for (j in seq_len(nrow(ps))) {
        k <- which(vapply(cand$steps, `[[`, character(1), "id") ==
                     ps$component_id[j])
        cand$steps[[k]][[ps$name[j]]] <- grid[[ps$id[j]]][i]
      }
      prepped <- expect_no_error(recipes::prep(cand))
      baked <- recipes::bake(prepped, new_data = north[105:120, ])
      expect_false(anyNA(baked$tv), info = paste(nm, i))
    }
  }
})

test_that("required_pkgs() names mediamix whatever the kernel", {
  skip_if_not_installed("recipes")
  d <- data.frame(y = 1:5, x = c(1, 2, 3, 4, 5), t = 1:5)
  step <- step_adstock(recipes::recipe(y ~ ., data = d), x, index = "t",
                       kernel = "delayed", max_lag = 3,
                       keep_original_cols = TRUE)$steps[[1]]
  expect_identical(generics::required_pkgs(step), "mediamix")
})

# ---- dials parameters for the Weibull and delayed kernels ----------------------------

test_that("carryover_shape(), carryover_scale() and carryover_peak() are valid dials params", {
  skip_if_not_installed("dials")
  params <- list(carryover_shape(), carryover_scale(), carryover_peak())
  ranges <- list(c(0.5, 5), c(0.5, 10), c(0, 4))
  for (i in seq_along(params)) {
    expect_s3_class(params[[i]], "quant_param")
    expect_identical(params[[i]]$type, "double")
    expect_equal(unname(unlist(params[[i]]$range)), ranges[[i]])
  }
  g <- dials::grid_regular(carryover_shape(), carryover_scale(), levels = 3)
  expect_identical(nrow(g), 9L)
  expect_true(all(g$shape >= 0.5 & g$shape <= 5))
  expect_equal(range(dials::value_seq(carryover_peak(), 5)), c(0, 4))
})

test_that("the Weibull kernel has weight somewhere over the whole tuning range", {
  skip_if_not_installed("dials")
  g <- dials::grid_regular(carryover_shape(), carryover_scale(), levels = 5)
  for (type in c("cdf", "pdf")) {
    for (lag in c(1, 2, 13, 26)) {
      for (i in seq_len(nrow(g))) {
        w <- adstock_weights_weibull(lag, shape = g$shape[i],
                                     scale = g$scale[i], type = type)
        expect_equal(sum(w), 1, info = paste(type, lag, i))
      }
    }
  }
})
