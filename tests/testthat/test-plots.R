# Plot methods: each must run on the objects the package produces, restore
# graphical parameters, and return its input invisibly.

with_device <- function(expr) {
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit({ grDevices::dev.off(); unlink(f) })
  op <- graphics::par(no.readonly = TRUE)
  out <- withVisible(force(expr))
  expect_identical(graphics::par("mar"), op$mar)
  expect_false(out$visible)
  out$value
}

test_that("mm_palette() is fixed-order and refuses a ninth colour", {
  expect_identical(unname(mm_palette(2)), c("#2a78d6", "#eb6834"))
  expect_length(mm_palette(role = "ink"), 6)
  expect_error(mm_palette(9), "only 8")
})

test_that("carryover, bootstrap and response plots draw", {
  set.seed(1)
  x <- pmax(0, rnorm(80, 500, 200))
  y <- 10 + 0.4 * adstock_geometric(x, 0.6) + rnorm(80, sd = 5)
  tuned <- tune_carryover(x, y, decays = seq(0.1, 0.9, 0.1))
  expect_s3_class(with_device(plot(tuned, truth = 0.6)), "mm_carryover")

  j <- tune_carryover_joint(data.frame(a = x, b = rev(x)), y,
                            decays = c(0.2, 0.6), skip = 9)
  with_device(plot(j, truth = c(a = 0.6)))

  b <- block_bootstrap(data.frame(y = y), function(d) c(m = mean(d$y)),
                       times = 20, seed = 1)
  with_device(plot(b, reference = 0))

  rc <- response_curve(seq(0, 2000, 100), 5, half_max = 500, shape = 2)
  expect_s3_class(rc, "mm_response_curve")
  with_device(plot(rc, observed = c(0, 900)))
})

test_that("contribution and attribution plots draw", {
  data(mm_weekly, envir = environment())
  north <- mm_weekly[mm_weekly$geo == "north", ]
  media <- data.frame(tv = adstock_geometric(north$tv, 0.8),
                      search = north$search)
  fit <- lm(north$revenue ~ ., data = media)
  contrib <- contributions(media, fit, index = north$date)
  expect_s3_class(contrib, "mm_contributions")
  with_device(plot(contrib))
  with_device(plot(contrib, type = "total"))

  data(mm_events, envir = environment())
  paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                       timestamp = "timestamp", conversion = "conversion")
  res <- attribute(paths)
  expect_s3_class(res, "mm_attribution")
  with_device(plot(res, n = 5))
})
