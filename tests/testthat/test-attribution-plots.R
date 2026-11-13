# Plot methods for attribute() results with intervals or segments
# (R/plot-attribution.R): each must draw, restore graphical parameters,
# leave the random number stream alone and return its input invisibly.

with_device <- function(expr) {
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit({ grDevices::dev.off(); unlink(f) })
  op <- graphics::par(no.readonly = TRUE)
  out <- withVisible(force(expr))
  expect_identical(graphics::par("mar"), op$mar)
  expect_identical(graphics::par("mfrow"), op$mfrow)
  expect_false(out$visible)
  out$value
}

# Draw `expr` on a null device and return the calls it made to the base
# graphics functions in `fns`, each with its arguments matched to their
# formal names. Calls that graphics makes internally, for example inside
# legend(), are not recorded.
draw_calls <- function(expr, fns = c("points", "text", "segments", "axis",
                                     "mtext", "title", "legend")) {
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

device_paths <- function() {
  data(mm_events, envir = environment())
  ev <- mm_events
  ev$device <- ifelse(grepl("[02468]$", ev$customer_id), "mobile", "desktop")
  build_paths(ev, id = "customer_id", channel = "channel",
              timestamp = "timestamp", conversion = "conversion",
              keep = "device")
}

test_that("interval and segment results plot, restore par and leave the RNG alone", {
  p <- device_paths()
  b <- attribute(p, rules = c("first", "last", "markov"), times = 20, seed = 1)
  s <- attribute(p, rules = c("first", "last"), by = "device")
  sb <- attribute(p, rules = "first", by = "device", times = 20, seed = 1)
  set.seed(5)
  before <- runif(1)
  set.seed(5)
  expect_identical(with_device(plot(b)), b)
  expect_identical(with_device(plot(s)), s)
  expect_identical(with_device(plot(sb, n = 3)), sb)
  expect_identical(runif(1), before)
})

test_that("the interval plot draws each rule's share and interval per channel", {
  p <- device_paths()
  b <- attribute(p, rules = c("first", "last", "markov"), times = 20, seed = 1)
  calls <- draw_calls(plot(b, n = 4))

  mean_share <- tapply(b$share, b$channel, mean)
  top <- names(sort(mean_share, decreasing = TRUE))[1:4]
  expect_setequal(left_labels(calls), top)
  expect_identical(left_labels(calls)[4], top[1])

  lines <- calls_to(calls, "segments")
  dots <- calls_to(calls, "points")
  expect_length(lines, 3L)
  expect_length(dots, 3L)
  first <- b[b$rule == "first" & b$channel %in% top, ]
  expect_setequal(eval(dots[[1L]]$args$x), first$share)
  expect_setequal(eval(lines[[1L]]$args$x0), first$lower)
  expect_setequal(eval(lines[[1L]]$args$x1), first$upper)
  text <- drawn_text(calls)
  expect_true(any(grepl("90% intervals", text)))
  expect_true(any(grepl("20 resamples", text)))
})

test_that("segments get one panel each, titled by column and value", {
  p <- device_paths()
  s <- attribute(p, rules = c("first", "last"), by = "device")
  calls <- draw_calls(plot(s, n = 5))
  titles <- vapply(calls_to(calls, "title"), function(cl) {
    if (is.null(cl$args$main)) "" else cl$args$main
  }, character(1))
  expect_true(all(c("device: desktop", "device: mobile") %in% titles))
  expect_length(left_labels(calls), 5L)
  expect_length(calls_to(calls, "segments"), 0L)
  expect_length(calls_to(calls, "points"), 4L)
  expect_true("Share of conversions by rule and segment" %in%
                drawn_text(calls))

  ev <- data.frame(id = letters[1:6], ch = c("x", "y"), ts = 1,
                   conv = c(1, 0, 1, 1, 0, 1), grp = paste0("g", 1:6))
  six <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                     conversion = "conv", keep = "grp")
  res <- suppressWarnings(attribute(six, rules = "first", by = "grp"))
  expect_message(calls <- draw_calls(plot(res)), "first 4 are drawn")
  titles <- unlist(lapply(calls_to(calls, "title"), function(cl) cl$args$main))
  expect_identical(titles, paste0("grp: g", 1:4))
})

test_that("the plot takes main, xlab and col, and checks its input", {
  p <- device_paths()
  b <- attribute(p, rules = c("first", "last"), times = 20, seed = 1)
  calls <- draw_calls(plot(b, main = "Rules", xlab = "Share",
                           col = c("black", "grey")))
  expect_true(all(c("Rules", "Share") %in% drawn_text(calls)))
  fills <- vapply(calls_to(calls, "points"), function(cl) eval(cl$args$bg),
                  character(1))
  expect_identical(fills, c("black", "grey"))

  gap <- b
  gap$share[1L] <- NA
  expect_warning(calls <- draw_calls(plot(gap)), "missing")
  open <- b
  open$lower[1L] <- NA
  expect_no_error(draw_calls(plot(open)))

  expect_error(plot(b[, c("rule", "channel")]), "share",
               class = "rlang_error")
  expect_error(plot(b, n = 0), "n")

  # A column subset loses the level, and the title stops quoting it.
  bare <- b[, c("rule", "channel", "share", "lower", "upper")]
  expect_null(attr(bare, "level"))
  text <- drawn_text(draw_calls(plot(bare)))
  expect_true("Share of conversions by rule, with bootstrap intervals" %in%
                text)
  expect_false(any(grepl("NA|%%", text)))
})

test_that("each rule keeps its colour whichever rules ran", {
  pal <- mm_palette()
  colours <- mediamix:::.mm_rule_colours
  expect_identical(colours(c("markov", "first")),
                   unname(pal[c("aqua", "blue")]))
  expect_identical(colours(c("first", "last", "markov")),
                   unname(pal[c("blue", "orange", "aqua")]))
  expect_identical(colours(c("linear", "custom")),
                   unname(pal[c("yellow", "violet")]))
  expect_identical(colours(c("first", "last"), col = "black"),
                   c("black", "black"))
})
