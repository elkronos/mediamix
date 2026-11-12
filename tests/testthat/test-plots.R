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

test_that("the time plot puts periods on their own axis, in order", {
  nm <- north_media()
  n <- nrow(nm$media)

  posix <- contributions(nm$media, nm$fit,
                         index = as.POSIXct(nm$north$date, tz = "UTC"))
  expect_length(calls_to(draw_calls(plot(posix)), "axis.POSIXct"), 1L)

  years <- 1990 + seq_len(n)
  numbered <- contributions(nm$media, nm$fit, index = years)
  poly <- calls_to(draw_calls(plot(numbered)), "polygon")
  expect_equal(range(poly[[1L]]$args$x), range(years))

  dated <- contributions(nm$media, nm$fit, index = nm$north$date)
  backwards <- dated[rev(seq_len(nrow(dated))), ]
  poly <- calls_to(draw_calls(plot(backwards)), "polygon")
  expect_false(is.unsorted(poly[[1L]]$args$x[seq_len(n)]))
})

test_that("a missing contribution or share warns and keeps its channel", {
  nm <- north_media()
  gap <- contributions(nm$media, nm$fit, index = nm$north$date)
  gap$contribution[gap$channel == "tv"][5] <- NA
  expect_warning(calls <- draw_calls(plot(gap, type = "total")),
                 "missing")
  expect_true("tv" %in% left_labels(calls))
  expect_false(any(grepl("NA%", drawn_text(calls))))
  expect_warning(draw_calls(plot(gap)), "missing")

  data(mm_events, envir = environment())
  paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                       timestamp = "timestamp", conversion = "conversion")
  res <- attribute(paths)
  top <- names(which.max(tapply(res$share, res$channel, mean)))
  res$share[res$channel == top & res$rule == "linear"] <- NA
  expect_warning(calls <- draw_calls(plot(res)), "missing")
  expect_true(top %in% left_labels(calls))
})

test_that("data frame plot methods name the columns they need", {
  nm <- north_media()
  contrib <- contributions(nm$media, nm$fit, index = nm$north$date)
  expect_error(plot(contrib[, c("period", "channel")]), "contribution",
               class = "rlang_error")

  data(mm_events, envir = environment())
  paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                       timestamp = "timestamp", conversion = "conversion")
  res <- attribute(paths)
  expect_error(plot(res[, c("rule", "channel")]), "share",
               class = "rlang_error")

  rc <- response_curve(seq(0, 2000, 100), 5, half_max = 500, shape = 2)
  expect_error(plot(rc[, c("spend", "response")]), "marginal",
               class = "rlang_error")

  b <- block_bootstrap(data.frame(a = 1:20 + 0), function(d) c(m = mean(d$a)),
                       times = 10, seed = 1)
  expect_error(plot(b[, c("term", "estimate")]), "lower",
               class = "rlang_error")
})

test_that("degenerate contributions and response curves plot or fail clearly", {
  nm <- north_media()
  contrib <- contributions(nm$media, nm$fit, index = nm$north$date)
  base_only <- contrib[contrib$channel == "(baseline)", ]
  expect_error(plot(base_only), "no media channel", class = "rlang_error")

  negative <- contributions(nm$media, c(tv = -0.5, search = 0.2),
                            intercept = 1000, index = nm$north$date)
  text <- drawn_text(draw_calls(plot(negative, type = "total")))
  expect_false(any(grepl("[0-9]% of the prediction", text)))
  expect_true(any(grepl("against a prediction of", text)))

  zero <- contributions(data.frame(tv = rep(0, 5)), c(tv = 1), intercept = 0)
  text <- drawn_text(draw_calls(plot(zero)))
  expect_false(any(grepl("NaN", text)))

  falling <- response_curve(seq(0, 5000, 100), -2, half_max = 2000, shape = 2)
  text <- drawn_text(draw_calls(plot(falling)))
  expect_true("steepest" %in% text)
})

test_that("data frame plot methods return their input unchanged", {
  fx <- carryover_fixture()
  b <- block_bootstrap(data.frame(a = fx$x, b = fx$y), colMeans, times = 20,
                       seed = 1)
  expect_identical(with_device(plot(b)), b)

  rc <- response_curve(seq(0, 2000, 100), 5, half_max = 500, shape = 2)
  expect_identical(with_device(plot(rc, observed = c(0, 900))), rc)

  nm <- north_media()
  contrib <- contributions(nm$media, nm$fit, index = nm$north$date)
  expect_identical(with_device(plot(contrib)), contrib)
  expect_identical(with_device(plot(contrib, type = "total")), contrib)

  data(mm_events, envir = environment())
  paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                       timestamp = "timestamp", conversion = "conversion")
  res <- attribute(paths)
  expect_identical(with_device(plot(res)), res)
})

test_that("small shares and large totals get readable labels", {
  data(mm_events, envir = environment())
  paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                       timestamp = "timestamp", conversion = "conversion")
  res <- attribute(paths)
  small <- res[res$share < 0.035, ]
  bottom <- Filter(function(cl) identical(cl$args$side, 1),
                   calls_to(draw_calls(plot(small)), "axis"))
  ticks <- bottom[[1L]]$args$labels
  expect_identical(anyDuplicated(ticks), 0L)
  expect_true("0.5%" %in% ticks)

  nm <- north_media()
  big <- contributions(nm$media, nm$fit, index = nm$north$date)
  big$contribution <- big$contribution * 1e5
  text <- drawn_text(draw_calls(plot(big, type = "total")))
  expect_true(any(grepl("^[0-9.]+B$", text)))
  expect_false(any(grepl("e\\+", text)))
})

test_that("long term names widen the margin instead of being clipped", {
  long <- "a_really_long_term_name_for_a_channel"
  d <- data.frame(x = seq(1, 2, length.out = 30),
                  b = seq(2, 3, length.out = 30))
  names(d)[1] <- long
  b <- block_bootstrap(d, colMeans, times = 10, seed = 1)
  calls <- draw_calls(plot(b), fns = c("par", "axis"))
  set <- Filter(function(cl) !is.null(cl$args$mar), calls_to(calls, "par"))
  grDevices::pdf(NULL)
  need <- graphics::strwidth(long, units = "inches", cex = 0.85) /
    (graphics::par("csi") * graphics::par("mex"))
  grDevices::dev.off()
  expect_gt(set[[1L]]$args$mar[2], need)
  expect_true(long %in% left_labels(calls))
})

test_that("the time plot folds channels past the eighth into Other", {
  ten <- as.data.frame(lapply(stats::setNames(1:10, paste0("c", 1:10)),
                              function(i) i * (1 + sin(seq_len(30) + i))))
  wide <- contributions(ten, stats::setNames(rep(1, 10), names(ten)),
                        intercept = 100)
  expect_message(calls <- draw_calls(plot(wide)), "Other")
  expect_length(calls_to(calls, "polygon"), 8L)
  expect_length(calls_to(draw_calls(plot(wide, type = "total")), "rect"), 1L)
})

test_that("the response curve shades spend below the observed range too", {
  rc <- response_curve(seq(0, 2000, 100), 5, half_max = 500, shape = 2)
  rects <- calls_to(draw_calls(plot(rc, observed = c(500, 900))), "rect")
  expect_length(rects, 4L)
  right_edges <- vapply(rects, function(cl) cl$args$xright, numeric(1))
  expect_true(500 %in% right_edges)
})

test_that("data frame plot methods take main, xlab, ylab and col", {
  fx <- carryover_fixture()
  b <- block_bootstrap(data.frame(a = fx$x), function(d) c(m = mean(d$a)),
                       times = 10, seed = 1)
  calls <- draw_calls(plot(b, main = "Mean spend", col = "black"))
  expect_true("Mean spend" %in% drawn_text(calls))
  expect_identical(calls_to(calls, "segments")[[1L]]$args$col, "black")

  nm <- north_media()
  contrib <- contributions(nm$media, nm$fit, index = nm$north$date)
  calls <- draw_calls(plot(contrib, main = "Media", ylab = "Revenue",
                           col = c("black", "grey")))
  expect_true(all(c("Media", "Revenue") %in% drawn_text(calls)))
  fills <- vapply(calls_to(calls, "polygon"), function(cl) cl$args$col, "")
  expect_identical(fills, c("black", "grey"))

  rc <- response_curve(seq(0, 2000, 100), 5, half_max = 500, shape = 2)
  calls <- draw_calls(plot(rc, main = "TV", xlab = "Weekly spend",
                           col = "black"))
  expect_true(all(c("TV", "Weekly spend") %in% drawn_text(calls)))
  expect_identical(calls_to(calls, "lines")[[1L]]$args$col, "black")

  data(mm_events, envir = environment())
  paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                       timestamp = "timestamp", conversion = "conversion")
  calls <- draw_calls(plot(attribute(paths), main = "Rules",
                           xlab = "Share"))
  expect_true(all(c("Rules", "Share") %in% drawn_text(calls)))
})
