# Tests for step_saturation() (R/steps-saturation.R).

test_that("bounded curve types (hill, exponential, michaelis_menten) produce output in [0, 1)", {
  skip_if_not_installed("recipes")
  set.seed(1)
  d <- data.frame(y = 1:20, x = c(0, runif(19, 0, 100)))

  bounded <- list(hill = 1.2, exponential = 1, michaelis_menten = 1)
  for (ty in names(bounded)) {
    rec <- recipes::recipe(y ~ ., data = d) |>
      step_saturation(x, type = ty, half_max = 0.5, shape = bounded[[ty]]) |>
      recipes::prep()
    out <- recipes::bake(rec, new_data = NULL)
    expect_true(all(out$x >= 0 & out$x < 1), info = ty)
  }

  # power is not bounded to [0, 1): it is concave but unbounded
  rec_power <- recipes::recipe(y ~ ., data = d) |>
    step_saturation(x, type = "power", shape = 0.5) |>
    recipes::prep()
  out_power <- recipes::bake(rec_power, new_data = NULL)
  expect_true(all(out_power$x >= 0))
})

test_that("tidy.step_saturation() reports the learned reference level per column", {
  skip_if_not_installed("recipes")
  set.seed(2)
  d <- data.frame(y = 1:20, tv = runif(20, 0, 500), search = runif(20, 0, 100))
  rec <- recipes::recipe(y ~ ., data = d) |>
    step_saturation(tv, search, half_max = 0.4, shape = 1.3, ref = "max") |>
    recipes::prep()

  td <- generics::tidy(rec$steps[[1]])
  expect_identical(sort(td$terms), c("search", "tv"))
  expect_equal(td$reference[td$terms == "tv"], max(d$tv))
  expect_equal(td$reference[td$terms == "search"], max(d$search))
  expect_true(all(td$half_max == 0.4))
  expect_true(all(td$shape == 1.3))

  # untrained: reference is not yet known
  rec_unprepped <- recipes::recipe(y ~ ., data = d) |>
    step_saturation(tv, search, half_max = 0.4)
  td_unprepped <- generics::tidy(rec_unprepped$steps[[1]])
  expect_true(all(is.na(td_unprepped$reference)))
})

test_that("an all-zero column errors at prep time", {
  skip_if_not_installed("recipes")
  d <- data.frame(y = 1:5, x = rep(0, 5))
  rec <- recipes::recipe(y ~ ., data = d) |> step_saturation(x)
  expect_error(recipes::prep(rec), "non-positive reference")
})

test_that("the reference level is learned from training data only and a higher test value is not clipped", {
  skip_if_not_installed("recipes")
  set.seed(3)
  train <- data.frame(y = 1:20, x = runif(20, 0, 100))
  rec <- recipes::recipe(y ~ ., data = train) |>
    step_saturation(x, type = "hill", half_max = 0.5, shape = 1) |>
    recipes::prep()

  above_ref <- data.frame(y = 1, x = max(train$x) * 5)
  baked <- recipes::bake(rec, new_data = above_ref)
  # further up the (bounded) curve, but not clipped to the training max's output
  train_max_out <- recipes::bake(rec, new_data = data.frame(y = 1, x = max(train$x)))
  expect_gt(baked$x, train_max_out$x)
  expect_lt(baked$x, 1)
})

# ---- invariant 3: bake() preserves row count and order -------------------------

test_that("bake.step_saturation preserves row count and row order", {
  skip_if_not_installed("recipes")
  set.seed(4)
  n <- 30
  d <- data.frame(y = rnorm(n), x = runif(n, 0, 100), idx = seq_len(n))
  d <- d[sample(n), ]
  rownames(d) <- NULL

  rec <- recipes::recipe(y ~ ., data = d) |>
    step_saturation(x, half_max = 0.5) |>
    recipes::prep()

  baked <- recipes::bake(rec, new_data = d)
  expect_identical(nrow(baked), nrow(d))
  expect_identical(baked$idx, d$idx)
})

# ---- tunable() ------------------------------------------------------------------

test_that("tunable.step_saturation() returns the documented data frame", {
  skip_if_not_installed("recipes")
  set.seed(7)
  d <- data.frame(y = 1:10, x = runif(10))
  rec <- recipes::recipe(y ~ ., data = d) |>
    step_saturation(x, half_max = 0.5) |>
    recipes::prep()

  tb <- generics::tunable(rec$steps[[1]])
  expect_identical(names(tb), c("name", "call_info", "source", "component",
                                 "component_id"))
  expect_setequal(tb$name, c("half_max", "shape"))
  expect_true(all(tb$source == "recipe"))
  expect_true(all(tb$component == "step_saturation"))
  expect_identical(unique(tb$component_id), rec$steps[[1]]$id)
  expect_identical(tb$call_info[[1]]$fun, "saturation_half_max")
  expect_identical(tb$call_info[[2]]$fun, "saturation_shape")
})

# ---- dials parameter objects ----------------------------------------------------

test_that("saturation_half_max() and saturation_shape() are valid dials params usable in grid_regular()", {
  skip_if_not_installed("dials")
  hm <- saturation_half_max()
  sh <- saturation_shape()
  expect_s3_class(hm, "quant_param")
  expect_s3_class(sh, "quant_param")

  vals <- dials::value_seq(sh, 4)
  expect_length(vals, 4)
  expect_true(all(vals >= 0.5 & vals <= 3))

  g <- dials::grid_regular(hm, sh, levels = 3)
  expect_identical(nrow(g), 9L)
  expect_true(all(g$half_max >= 0.05 & g$half_max <= 1))
  expect_true(all(g$shape >= 0.5 & g$shape <= 3))
})

# ---- order matters: step_adstock then step_saturation composes as documented ----

test_that("step_adstock followed by step_saturation composes without error and stays row-preserving", {
  skip_if_not_installed("recipes")
  data(mm_weekly)
  north <- mm_weekly[mm_weekly$geo == "north", ]
  rec <- recipes::recipe(revenue ~ ., data = north) |>
    step_adstock(tv, video, search, index = "date", decay = 0.6) |>
    step_saturation(tv, video, search, half_max = 0.4, shape = 1.2) |>
    recipes::prep()

  out <- recipes::bake(rec, new_data = NULL)
  expect_identical(nrow(out), nrow(north))
  expect_true(all(out$tv >= 0 & out$tv < 1))
})
