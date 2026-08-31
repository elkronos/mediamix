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
