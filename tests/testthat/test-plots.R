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

# Draw `expr` on a null device and return the calls it made to the base
# graphics functions in `fns`, each with its arguments matched to their
# formal names. Calls that graphics makes internally, for example inside
# legend(), are not recorded.
draw_calls <- function(expr, fns = c("points", "text", "lines", "polygon",
                                     "rect", "segments", "axis", "axis.Date",
                                     "axis.POSIXct", "mtext", "title")) {
  log <- new.env(parent = emptyenv())
  log$calls <- list()
  graphics_ns <- asNamespace("graphics")
  wrap <- function(fn) {
    orig <- getExportedValue("graphics", fn)
    def <- if (fn %in% c("points", "lines", "text")) {
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

calls_to <- function(calls, fn) {
  Filter(function(cl) identical(cl$fn, fn), calls)
}

drawn_text <- function(calls) {
  unlist(lapply(calls, function(cl) {
    switch(cl$fn,
           text = cl$args$labels,
           mtext = cl$args$text,
           title = c(cl$args$main, cl$args$xlab, cl$args$ylab),
           axis = if (is.character(cl$args$labels)) cl$args$labels,
           NULL)
  }))
}

left_labels <- function(calls) {
  side2 <- Filter(function(cl) identical(cl$args$side, 2),
                  calls_to(calls, "axis"))
  unlist(lapply(side2, function(cl) cl$args$labels))
}

carryover_fixture <- function() {
  set.seed(1)
  x <- pmax(0, rnorm(80, 500, 200))
  y <- 10 + 0.4 * adstock_geometric(x, 0.6) + rnorm(80, sd = 5)
  list(x = x, y = y)
}

north_media <- function() {
  data(mm_weekly, envir = environment())
  north <- mm_weekly[mm_weekly$geo == "north", ]
  media <- data.frame(tv = adstock_geometric(north$tv, 0.8),
                      search = north$search)
  list(north = north, media = media,
       fit = lm(north$revenue ~ ., data = media))
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

test_that("the joint plot draws one line per max_lag, markers on their line", {
  fx <- carryover_fixture()
  j <- suppressWarnings(tune_carryover_joint(
    data.frame(a = fx$x, b = rev(fx$x)), fx$y, max_lags = c(2, 3, Inf),
    decays = seq(0.1, 0.9, 0.2), skip = 4))
  calls <- draw_calls(plot(j))

  drawn <- calls_to(calls, "lines")
  expect_length(drawn, 2L * 3L)
  for (l in drawn) expect_false(is.unsorted(l$args$x))

  p <- j$profiles[j$profiles$channel == "a", ]
  at <- function(row) p$metric[p$max_lag == row$max_lag & p$decay == row$decay]
  markers <- calls_to(calls, "points")
  expect_equal(markers[[1L]]$args$x, j$best$decay[[1L]])
  expect_equal(markers[[1L]]$args$y, at(j$best[1L, ]))
  one_se <- j$best_1se[1L, ]
  if (one_se$max_lag != j$best$max_lag[[1L]] ||
      one_se$decay != j$best$decay[[1L]]) {
    expect_equal(markers[[2L]]$args$y, at(one_se))
  }
})

test_that("carryover plots draw every max_lag, past the eighth in grey", {
  fx <- carryover_fixture()
  tuned <- suppressWarnings(tune_carryover(fx$x, fx$y,
                                           max_lags = c(1:9, Inf),
                                           decays = seq(0.1, 0.9, 0.2)))
  expect_message(calls <- draw_calls(plot(tuned)), "grey")
  expect_length(calls_to(calls, "lines"), 10L)
})

test_that("carryover plots check columns, return x, take graphical arguments", {
  fx <- carryover_fixture()
  tuned <- tune_carryover(fx$x, fx$y, decays = seq(0.1, 0.9, 0.2))
  j <- tune_carryover_joint(data.frame(a = fx$x, b = rev(fx$x)), fx$y,
                            decays = c(0.2, 0.6), skip = 9)
  expect_identical(with_device(plot(tuned)), tuned)
  expect_identical(with_device(plot(j)), j)

  calls <- draw_calls(plot(tuned, main = "Profile", xlab = "Rate",
                           ylab = "Error", col = "black"))
  expect_true(all(c("Profile", "Rate", "Error") %in% drawn_text(calls)))
  expect_true(all(vapply(calls_to(calls, "lines"),
                         function(cl) cl$args$col, "") == "black"))
  calls <- draw_calls(plot(j, main = "Both channels"))
  expect_true("Both channels" %in% drawn_text(calls))

  edited <- tuned
  edited$results <- edited$results[, c("max_lag", "decay")]
  expect_error(plot(edited), "metric", class = "rlang_error")
  edited <- j
  edited$profiles <- edited$profiles[, c("channel", "decay", "metric")]
  expect_error(plot(edited), "max_lag", class = "rlang_error")
})

test_that("tick and value labels are short, exact and never scientific", {
  num <- mediamix:::.mm_num
  expect_identical(num(c(0, 5e5, 1e6, 1.5e6)), c("0", "0.5M", "1M", "1.5M"))
  expect_identical(num(c(0, 2e9, 4e9)), c("0", "2B", "4B"))
  expect_identical(num(c(1000, 1002, 1004)), c("1,000", "1,002", "1,004"))
  expect_identical(num(c(0, 0.25, 0.5)), c("0", "0.25", "0.5"))
  expect_identical(mediamix:::.mm_short(c(1234567890, 349634, -746, 0.012345)),
                   c("1.23B", "350k", "-746", "0.0123"))
  expect_identical(mediamix:::.mm_pct(c(0, 0.005, 0.01, 0.015)),
                   c("0%", "0.5%", "1%", "1.5%"))
})
