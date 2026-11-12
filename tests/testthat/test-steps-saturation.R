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
  expect_error(recipes::prep(rec), "no positive values")
})

test_that("a zero quantile on a late-launched channel is not reported as an all-zero column", {
  skip_if_not_installed("recipes")
  # the channel runs only in the last 7 of 156 weeks
  d <- data.frame(y = 1, x = c(rep(0, 149), rep(100, 7)))
  rec <- recipes::recipe(y ~ ., data = d) |> step_saturation(x, ref = "q95")
  err <- tryCatch(recipes::prep(rec), error = function(e) conditionMessage(e))
  expect_match(err, "\"q95\" reference level is not positive")
  expect_match(err, "launched late")
  expect_no_match(err, "zero throughout")
  expect_no_error(recipes::prep(recipes::recipe(y ~ ., data = d) |>
                                  step_saturation(x, ref = "max")))
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

# ---- reference levels per group --------------------------------------------------

# A panel whose geographies differ only in scale.
scaled_panel <- function() {
  set.seed(11)
  x <- runif(30, 50, 100)
  data.frame(y = 1, geo = rep(c("north", "south", "central"), each = 30),
             tv = c(x, x / 20, x * 5))
}

test_that("by gives each group its own reference level", {
  skip_if_not_installed("recipes")
  d <- scaled_panel()
  rec <- recipes::recipe(y ~ ., data = d) |>
    step_saturation(tv, half_max = 0.5, by = "geo") |>
    recipes::prep()
  out <- recipes::juice(rec)
  # proportional geographies land on the same points of the curve
  expect_equal(out$tv[d$geo == "south"], out$tv[d$geo == "north"])
  expect_equal(out$tv[d$geo == "central"], out$tv[d$geo == "north"])

  td <- generics::tidy(rec, number = 1)
  expect_s3_class(td, "tbl_df")
  expect_identical(td$group, c("north", "south", "central"))
  expect_equal(td$reference, tapply(d$tv, d$geo, max)[td$group],
               ignore_attr = TRUE)

  # pooled, the small geography never leaves the bottom of the curve
  pooled <- recipes::recipe(y ~ ., data = d) |>
    step_saturation(tv, half_max = 0.5) |>
    recipes::prep() |>
    recipes::juice()
  expect_lt(max(pooled$tv[d$geo == "south"]), 0.05)
})

test_that("without by the reference is pooled exactly as before", {
  skip_if_not_installed("recipes")
  d <- scaled_panel()
  rec <- recipes::recipe(y ~ ., data = d) |>
    step_saturation(tv, half_max = 0.4, shape = 1.5) |>
    recipes::prep()
  expect_equal(recipes::juice(rec)$tv,
               saturate_hill(d$tv, half_max = 0.4 * max(d$tv), shape = 1.5))
  expect_identical(names(generics::tidy(rec, number = 1)),
                   c("terms", "type", "half_max", "shape", "reference", "id"))
})

test_that("a group unseen at prep uses the pooled reference, with a warning", {
  skip_if_not_installed("recipes")
  d <- scaled_panel()
  rec <- recipes::recipe(y ~ ., data = d) |>
    step_saturation(tv, half_max = 0.5, by = "geo") |>
    recipes::prep()
  new <- data.frame(y = 1, geo = c("north", "west"), tv = c(60, 60))
  expect_warning(baked <- recipes::bake(rec, new_data = new),
                 "not seen during")
  expect_equal(baked$tv[1], saturate_hill(60, half_max = 0.5 * max(d$tv[1:30]),
                                          shape = 1))
  expect_equal(baked$tv[2], saturate_hill(60, half_max = 0.5 * max(d$tv),
                                          shape = 1))
})

test_that("a group with no spend on a channel falls back to the pooled reference", {
  skip_if_not_installed("recipes")
  d <- scaled_panel()
  d$tv[d$geo == "south"] <- 0
  expect_warning(
    rec <- recipes::recipe(y ~ ., data = d) |>
      step_saturation(tv, half_max = 0.5, by = "geo") |>
      recipes::prep(),
    "pooled reference level"
  )
  td <- generics::tidy(rec, number = 1)
  expect_equal(td$reference[td$group == "south"], max(d$tv))
})

test_that("missing group labels abort", {
  skip_if_not_installed("recipes")
  d <- scaled_panel()
  d$geo[3] <- NA
  expect_error(
    recipes::prep(recipes::recipe(y ~ ., data = d) |>
                    step_saturation(tv, by = "geo")),
    "missing value"
  )
})

# ---- only the parameters a curve uses are tunable --------------------------------

test_that("tunable() lists only the parameters the chosen curve uses", {
  skip_if_not_installed("recipes")
  d <- data.frame(y = 1:10, x = runif(10))
  rec <- recipes::recipe(y ~ ., data = d)
  uses <- list(hill = c("half_max", "shape"), exponential = "half_max",
               michaelis_menten = "half_max", power = "shape")
  for (ty in names(uses)) {
    shape <- if (ty == "power") 0.5 else 1
    tb <- generics::tunable(step_saturation(rec, x, type = ty,
                                            shape = shape)$steps[[1]])
    expect_identical(tb$name, uses[[ty]], info = ty)
  }
})

test_that("tune() on an argument the curve ignores is rejected", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("hardhat")
  d <- data.frame(y = 1:10, x = runif(10))
  rec <- recipes::recipe(y ~ ., data = d)
  tune <- hardhat::tune
  expect_error(step_saturation(rec, x, type = "exponential", shape = tune()),
               "cannot be tuned")
  expect_error(step_saturation(rec, x, type = "michaelis_menten",
                               shape = tune()),
               "cannot be tuned")
  expect_error(step_saturation(rec, x, type = "power", half_max = tune()),
               "cannot be tuned")
  expect_no_error(step_saturation(rec, x, type = "power", shape = tune()))
  expect_no_error(step_saturation(rec, x, type = "exponential",
                                  half_max = tune()))
})

test_that("tidy() works on a step with tune() placeholders and reports ignored parameters as NA", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("hardhat")
  d <- data.frame(y = 1:10, x = runif(10))
  rec <- recipes::recipe(y ~ ., data = d) |>
    step_saturation(x, half_max = hardhat::tune(), shape = hardhat::tune())
  td <- generics::tidy(rec, number = 1)
  expect_true(is.na(td$half_max))
  expect_true(is.na(td$shape))

  expo <- recipes::recipe(y ~ ., data = d) |>
    step_saturation(x, type = "exponential", half_max = 0.3, shape = 2) |>
    recipes::prep()
  td_expo <- generics::tidy(expo, number = 1)
  expect_equal(td_expo$half_max, 0.3)
  expect_true(is.na(td_expo$shape))
})

# ---- messages and edge cases -------------------------------------------------------

test_that("infinite media values abort at prep and at bake", {
  skip_if_not_installed("recipes")
  d <- data.frame(y = 1:4, x = c(100, 50, 25, 10))
  rec <- recipes::recipe(y ~ ., data = d) |>
    step_saturation(x) |>
    recipes::prep()
  expect_error(recipes::bake(rec, new_data = data.frame(y = 1, x = Inf)),
               "infinite values: \"x\"")
  d$x[2] <- Inf
  expect_error(recipes::prep(recipes::recipe(y ~ ., data = d) |>
                               step_saturation(x)),
               "infinite values")
})

test_that("an empty selection preps, bakes nothing and tidies to zero rows", {
  skip_if_not_installed("recipes")
  d <- data.frame(y = 1:4, x = c(100, 50, 25, 10))
  rec <- recipes::recipe(y ~ ., data = d) |>
    step_saturation(recipes::all_nominal_predictors()) |>
    recipes::prep()
  expect_equal(recipes::juice(rec)$x, d$x)
  td <- generics::tidy(rec, number = 1)
  expect_s3_class(td, "tbl_df")
  expect_identical(nrow(td), 0L)
})
