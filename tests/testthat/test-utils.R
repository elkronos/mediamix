test_that("a grouping value containing the key separator is refused", {
  by <- data.frame(a = c("x\x1fy", "x"), b = c("z", "y\x1fz"))
  expect_error(
    adstock_geometric(c(100, 100), decay = 0.5, by = by),
    "unit separator"
  )
  # One grouping column is never joined, so the value is used as it is.
  expect_equal(
    adstock_geometric(c(100, 100), decay = 0.5, by = c("x\x1fy", "x")),
    c(50, 50)
  )
})

test_that("recipe steps refuse a grouping value containing the separator", {
  skip_if_not_installed("recipes")
  d <- data.frame(
    week = rep(1:3, 2),
    a = rep(c("x\x1fy", "x"), each = 3),
    b = rep(c("z", "y\x1fz"), each = 3),
    tv = 1:6,
    y = 1:6
  )
  rec <- recipes::recipe(y ~ ., data = d)
  rec <- step_adstock(rec, tv, index = "week", by = c("a", "b"))
  expect_error(recipes::prep(rec, training = d), "unit separator")
})

test_that("ordinary multi-column keys are unchanged", {
  by <- data.frame(geo = c("north", "north", "south"), brand = c(1, 1, 2))
  out <- adstock_geometric(c(100, 0, 100), decay = 0.5, by = by)
  expect_equal(out, c(50, 25, 50))
})

test_that("count errors suggest Inf only where Inf is accepted", {
  err <- tryCatch(adstock_weights(3e9, 0.5), error = function(e) e)
  expect_s3_class(err, "error")
  expect_false(grepl("Inf", conditionMessage(err), fixed = TRUE))
  expect_match(conditionMessage(err), "too large")
  err_inf <- tryCatch(adstock_geometric(1:3, decay = 0.5, max_lag = 3e9),
                      error = function(e) e)
  expect_match(conditionMessage(err_inf), "Inf", fixed = TRUE)
})

test_that("range messages close an infinite bound only when it is allowed", {
  expect_error(saturate_hill(1, half_max = -1), "\\(0, Inf\\)")
  expect_error(saturate_hill(1, half_max = Inf), "finite")
})
