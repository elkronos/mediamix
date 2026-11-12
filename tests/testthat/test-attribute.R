# Tests for attribute() and attribution_spread() (R/attribute.R).

test_that("attribute() returns one row per rule x channel and share sums to 1 within each rule", {
  data(mm_events)
  paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                        timestamp = "timestamp", conversion = "conversion",
                        value = "value")
  res <- attribute(paths)

  expect_s3_class(res, "data.frame")
  expect_identical(names(res)[1:4], c("rule", "channel", "conversions", "share"))

  rules <- c("linear", "first", "last", "position", "time_decay", "markov")
  n_channels <- length(unique(paths$channel))
  expect_identical(nrow(res), length(rules) * n_channels)

  shares <- tapply(res$share, res$rule, sum)
  expect_equal(as.numeric(shares), rep(1, length(shares)), tolerance = 1e-8)

  # conversions sum to converting_journeys under every rule (mirrors the
  # credit_*() invariant, one level up)
  conv_sums <- tapply(res$conversions, res$rule, sum)
  expect_equal(as.numeric(conv_sums),
               rep(path_summary(paths)$converting_journeys, length(conv_sums)),
               tolerance = 1e-6)

  # value column present because mm_events carries conversion values
  expect_true("value" %in% names(res))
  expect_true("touches" %in% names(res))
})

test_that("attribute() respects the rules argument and validates it", {
  ev <- data.frame(
    cust = c("a", "a", "b"),
    ch = c("X", "Y", "Z"),
    ts = as.POSIXct("2024-01-01", tz = "UTC") + c(0, 1, 0) * 86400,
    conv = c(0, 1, 1)
  )
  p <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                    conversion = "conv")

  res <- attribute(p, rules = c("first", "last"))
  expect_setequal(unique(res$rule), c("first", "last"))

  expect_error(attribute(p, rules = "not_a_rule"), "should be one of")
})

test_that("attribute() works on a hand-made journey table with a known answer", {
  ev <- data.frame(
    cust = c("a", "b"),
    ch = c("X", "Y"),
    ts = as.POSIXct("2024-01-01", tz = "UTC"),
    conv = c(1, 1)
  )
  p <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                    conversion = "conv")
  res <- attribute(p, rules = "linear")
  expect_setequal(res$channel, c("X", "Y"))
  expect_equal(res$conversions, c(1, 1))
  expect_equal(res$share, c(0.5, 0.5))
})

test_that("a rule named twice is run once, so mean_share is not skewed", {
  ev <- data.frame(cust = c("a", "a", "b"), ch = c("X", "Y", "Z"),
                   ts = as.POSIXct("2024-01-01", tz = "UTC") + c(0, 1, 0) * 86400,
                   conv = c(0, 1, 1))
  p <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                   conversion = "conv")
  twice <- attribute(p, rules = c("first", "first", "last"))
  once <- attribute(p, rules = c("first", "last"))
  expect_identical(nrow(twice), nrow(once))
  expect_equal(attribution_spread(twice), attribution_spread(once))
})

test_that("ties between channels are ordered the same way in every locale", {
  ev <- data.frame(cust = c("a", "b", "c"), ch = c("b", "B", "a"),
                   ts = as.POSIXct("2024-01-01", tz = "UTC"), conv = 1)
  p <- build_paths(ev, id = "cust", channel = "ch", timestamp = "ts",
                   conversion = "conv")
  # Under an English collation sort() puts "a" first; the C order puts
  # capitals first. The result must not depend on the session's locale.
  old <- Sys.getlocale("LC_COLLATE")
  suppressWarnings(Sys.setlocale("LC_COLLATE", "en_US.UTF-8"))
  res <- tryCatch(
    list(attr = suppressWarnings(attribute(p, rules = c("linear", "markov"))),
         pos = channel_positions(p),
         top = top_paths(p)),
    finally = Sys.setlocale("LC_COLLATE", old)
  )
  expect_identical(res$attr$channel, c("B", "a", "b", "B", "a", "b"))
  expect_identical(res$pos$channel, c("B", "a", "b"))
  expect_identical(res$top$path, c("B", "a", "b"))
})

test_that("markov_removal() matches one linear solve per removed channel", {
  # The per-channel solve that one factorisation replaces.
  reference <- function(paths) {
    p <- as.data.frame(paths)
    p <- p[order(p$path_id, p$touch_rank), ]
    chans <- sort(unique(p$channel), method = "radix")
    k <- length(chans)
    code <- match(p$channel, chans) + 1L
    first <- !duplicated(p$path_id)
    last <- !duplicated(p$path_id, fromLast = TRUE)
    from <- c(rep(1L, sum(first)), code[!last], code[last])
    to <- c(code[first], code[-1L][!last[-length(last)]],
            ifelse(p$converted[last], k + 2L, k + 3L))
    counts <- matrix(0, k + 3L, k + 3L)
    for (r in seq_along(from)) {
      counts[from[r], to[r]] <- counts[from[r], to[r]] + 1
    }
    p_conv <- function(removed = NULL) {
      cm <- counts
      if (!is.null(removed)) {
        cm[, k + 3L] <- cm[, k + 3L] + cm[, removed]
        cm[, removed] <- 0
        cm[removed, ] <- 0
      }
      rs <- rowSums(cm)
      live <- which(rs[seq_len(k + 1L)] > 0)
      P <- cm[live, , drop = FALSE] / rs[live]
      b <- solve(diag(length(live)) - P[, live, drop = FALSE], P[, k + 2L])
      b[match(1L, live)]
    }
    base <- p_conv()
    stats::setNames(vapply(seq_len(k) + 1L, function(s) {
      max(0, 1 - p_conv(s) / base)
    }, numeric(1)), chans)
  }
  set.seed(3)
  ev <- data.frame(id = sample(sprintf("c%02d", 1:40), 300, replace = TRUE),
                   ch = sample(c("a", "b", "c", "d", "e"), 300, replace = TRUE),
                   ts = runif(300, 0, 50))
  ev$conv <- as.numeric(runif(300) < 0.1)
  p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                   conversion = "conv", collapse_repeats = FALSE)
  mk <- markov_removal(p)
  ref <- reference(p)
  expect_equal(mk$removal_effect, unname(ref[mk$channel]), tolerance = 1e-10)

  data(mm_events)
  paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                       timestamp = "timestamp", conversion = "conversion")
  mk <- markov_removal(paths)
  ref <- reference(paths)
  expect_equal(mk$removal_effect, unname(ref[mk$channel]), tolerance = 1e-10)
})

test_that("markov_removal() returns the fitted chain behind its removal effects", {
  ev <- data.frame(id = c("j1", "j1", "j2", "j3"), ch = c("a", "b", "a", "b"),
                   ts = c(1, 2, 1, 1), conv = c(0, 1, 0, 1))
  p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                   conversion = "conv")
  mk <- markov_removal(p)
  expect_identical(names(mk), c("channel", "removal_effect", "conversions",
                                "share", "p_without"))
  expect_equal(mk$p_without[mk$channel == "a"], 1 / 3)
  expect_equal(mk$p_without[mk$channel == "b"], 0)
  expect_equal(mk$removal_effect,
               1 - mk$p_without / attr(mk, "p_conversion"))

  states <- list(from = c("(start)", "a", "b"),
                 to = c("a", "b", "(conversion)", "(null)"))
  counts <- matrix(c(2, 0, 0, 1, 1, 0, 0, 0, 2, 0, 1, 0), 3, 4,
                   dimnames = states)
  expect_equal(attr(mk, "transition_counts"), counts)
  expect_equal(attr(mk, "transitions"), counts / rowSums(counts))
  expect_equal(unname(rowSums(attr(mk, "transitions"))), rep(1, 3))
})

test_that("p_without matches removing each channel from the transition matrix", {
  # Redirect every transition into channel s to null and solve the chain.
  without <- function(P, s) {
    k <- nrow(P) - 1L
    Q <- cbind(0, P[, seq_len(k), drop = FALSE])
    Q[, s + 1L] <- 0
    absorb <- P[, k + 1L]
    keep <- setdiff(seq_len(k + 1L), s + 1L)
    solve(diag(length(keep)) - Q[keep, keep, drop = FALSE],
          absorb[keep])[1L]
  }
  data(mm_events)
  paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                       timestamp = "timestamp", conversion = "conversion",
                       collapse_repeats = FALSE)
  mk <- markov_removal(paths)
  P <- attr(mk, "transitions")
  k <- nrow(P) - 1L
  expect_identical(rownames(P)[-1L], colnames(P)[seq_len(k)])
  expect_true(any(diag(P[-1L, seq_len(k)]) > 0))
  ref <- vapply(seq_len(k), function(s) without(P, s), numeric(1))
  expect_equal(mk$p_without, unname(ref[match(mk$channel, rownames(P)[-1L])]),
               tolerance = 1e-10)
  base <- solve(diag(k + 1L) - cbind(0, P[, seq_len(k)]), P[, k + 1L])[1L]
  expect_equal(attr(mk, "p_conversion"), unname(base), tolerance = 1e-10)
  expect_equal(unname(rowSums(attr(mk, "transition_counts"))[1L]),
               length(unique(paths$path_id)))
})

test_that("the Markov chain matches an absorbing chain solved by hand", {
  # Journeys written out as walks, counted and solved here without any of
  # the package's internals.
  walks <- list(c("a", "b"), "a", c("b", "a", "b"), "c", c("a", "c"), "b",
                c("c", "b"))
  converted <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE)
  ev <- do.call(rbind, lapply(seq_along(walks), function(j) {
    ch <- walks[[j]]
    data.frame(id = paste0("j", j), ch = ch, ts = seq_along(ch),
               conv = c(rep(0, length(ch) - 1L), as.numeric(converted[j])))
  }))
  p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                   conversion = "conv", collapse_repeats = FALSE)
  mk <- markov_removal(p)

  states <- c("(start)", "a", "b", "c", "(conversion)", "(null)")
  counts <- matrix(0, 6, 6, dimnames = list(states, states))
  for (j in seq_along(walks)) {
    w <- c("(start)", walks[[j]],
           if (converted[j]) "(conversion)" else "(null)")
    for (t in seq_len(length(w) - 1L)) {
      counts[w[t], w[t + 1L]] <- counts[w[t], w[t + 1L]] + 1
    }
  }
  P <- counts[1:4, ] / rowSums(counts[1:4, ])
  p_conv <- function(P) solve(diag(4) - P[, 1:4], P[, "(conversion)"])[[1L]]
  base <- p_conv(P)
  without <- vapply(c("a", "b", "c"), function(s) {
    P[, "(null)"] <- P[, "(null)"] + P[, s]
    P[, s] <- 0
    p_conv(P)
  }, numeric(1))

  expect_equal(attr(mk, "p_conversion"), base)
  expect_equal(mk$p_without, unname(without[mk$channel]))
  expect_equal(mk$removal_effect, unname(1 - without[mk$channel] / base))
  expect_equal(unname(attr(mk, "transitions")), unname(P[, -1L]))
  expect_equal(unname(attr(mk, "transition_counts")), unname(counts[1:4, -1L]))
  expect_identical(dimnames(attr(mk, "transitions")),
                   list(from = states[1:4], to = states[-1L]))
})

test_that("markov_removal() reports its chain at the edges", {
  none <- build_paths(data.frame(id = c("a", "b"), ch = c("x", "y"),
                                 ts = c(1, 1), conv = c(0, 0)),
                      id = "id", channel = "ch", timestamp = "ts",
                      conversion = "conv")
  mk <- markov_removal(none)
  expect_identical(attr(mk, "p_conversion"), 0)
  expect_identical(mk$p_without, c(0, 0))
  expect_equal(unname(attr(mk, "transitions")[c("x", "y"), "(null)"]), c(1, 1))

  single <- build_paths(data.frame(id = "a", ch = c("x", "y"), ts = c(1, 2),
                                   conv = c(0, 1)),
                        id = "id", channel = "ch", timestamp = "ts",
                        conversion = "conv")
  mk1 <- suppressWarnings(markov_removal(single))
  expect_identical(attr(mk1, "p_conversion"), 1)
  expect_identical(mk1$p_without, c(0, 0))
  expect_true(all(attr(mk1, "transitions")[, "(null)"] == 0))

  empty <- single[0, ]
  mk0 <- markov_removal(empty)
  expect_identical(nrow(mk0), 0L)
  expect_true("p_without" %in% names(mk0))
  expect_null(attr(mk0, "transitions"))
})

test_that("the warning for a table without null paths does not claim a direction", {
  ev <- data.frame(id = c("j1", "j1", "j2"), ch = c("a", "b", "b"),
                   ts = c(1, 2, 1), conv = c(0, 1, 1))
  p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                   conversion = "conv")
  w <- tryCatch(markov_removal(p), warning = function(w) conditionMessage(w))
  expect_match(w, "no non-converting")
  expect_match(w, "up or")
  expect_no_match(w, "overstated")
})

test_that("attribution_spread() summarises attribute() output correctly", {
  data(mm_events)
  paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                        timestamp = "timestamp", conversion = "conversion")
  res <- attribute(paths)
  sp <- attribution_spread(res)

  expect_s3_class(sp, "data.frame")
  expect_identical(sort(names(sp)),
                    sort(c("channel", "min_share", "max_share", "mean_share",
                           "spread", "first_last_ratio")))
  expect_equal(sp$spread, sp$max_share - sp$min_share, tolerance = 1e-10)
  expect_true(all(sp$min_share <= sp$mean_share & sp$mean_share <= sp$max_share))
  # ordered by descending spread
  expect_true(all(diff(sp$spread) <= 0))

  expect_error(attribution_spread(data.frame(x = 1)), "must be the data frame")
})

# ---- segments ------------------------------------------------------------------

# Columns only, without the class and attributes of attribute()'s result.
plain_df <- function(d) {
  out <- data.frame(lapply(d, identity), stringsAsFactors = FALSE,
                    check.names = FALSE)
  rownames(out) <- NULL
  out
}

device_paths <- function(...) {
  data(mm_events, envir = environment())
  ev <- mm_events
  ev$device <- ifelse(grepl("[02468]$", ev$customer_id), "mobile", "desktop")
  build_paths(ev, id = "customer_id", channel = "channel",
              timestamp = "timestamp", conversion = "conversion",
              value = "value", keep = "device", ...)
}

test_that("attribute(by =) attributes each segment's journeys on their own", {
  p <- device_paths()
  res <- attribute(p, by = "device")
  expect_s3_class(res, "mm_attribution_segments")
  expect_s3_class(res, "mm_attribution")
  expect_identical(attr(res, "segments"), "device")
  expect_identical(names(res), c("device", "rule", "channel", "conversions",
                                 "share", "value", "touches"))
  expect_identical(unique(res$device), c("desktop", "mobile"))
  for (dv in c("desktop", "mobile")) {
    one <- attribute(p[p$device == dv, ])
    got <- res[res$device == dv, names(one)]
    expect_equal(plain_df(got), plain_df(one))
  }
  sums <- tapply(res$share, list(res$device, res$rule), sum)
  expect_equal(as.vector(sums), rep(1, length(sums)))
  expect_equal(sum(res$conversions[res$rule == "first"]),
               path_summary(p)$converting_journeys)
})

test_that("segments are sorted by value and keep their type", {
  ev <- data.frame(
    id = c("a", "a", "b", "c", "d"), ch = c("x", "y", "x", "y", "x"),
    ts = c(1, 2, 1, 1, 1), conv = c(0, 1, 1, 0, 1),
    tier = factor(c("gold", "gold", "basic", "gold", "basic"),
                  levels = c("gold", "basic")),
    n = c(2, 2, 10, 2, 10), region = c("s", "s", "s", "n", "n")
  )
  p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                   conversion = "conv", keep = c("tier", "n", "region"))
  by_tier <- attribute(p, rules = "first", by = "tier")
  expect_s3_class(by_tier$tier, "factor")
  expect_identical(levels(by_tier$tier), c("gold", "basic"))
  expect_identical(as.character(unique(by_tier$tier)), c("gold", "basic"))
  expect_identical(unique(attribute(p, rules = "first", by = "n")$n),
                   c(2, 10))

  both <- attribute(p, rules = "first", by = c("tier", "region"))
  expect_identical(names(both)[1:2], c("tier", "region"))
  expect_identical(paste(both$tier, both$region),
                   c("gold n", "gold s", "gold s", "basic n", "basic s"))
  expect_identical(attr(both, "segments"), c("tier", "region"))
  # A segment with no converting journey has zero shares, and a single
  # journey is a segment like any other.
  expect_identical(both$share[both$tier == "gold" & both$region == "n"], 0)
  expect_identical(both$conversions[both$tier == "basic" &
                                      both$region == "n"], 1)
})

test_that("every segment has a value column when the table carries values", {
  ev <- data.frame(id = c("a", "b", "c"), ch = c("x", "y", "x"), ts = 1,
                   conv = c(1, 0, 0), val = c(20, NA, NA),
                   grp = c("buyers", "browsers", "browsers"))
  p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                   conversion = "conv", value = "val", keep = "grp")
  expect_warning(res <- attribute(p, by = "grp"), "buyers")
  expect_identical(res$value[res$grp == "browsers"], rep(0, 12))
  expect_equal(sum(res$value[res$grp == "buyers" & res$rule == "linear"]), 20)
})

test_that("attribute(by =) checks the segment columns", {
  ev <- data.frame(id = c("a", "a", "b"), ch = c("x", "y", "x"),
                   ts = c(1, 2, 1), conv = c(0, 1, 1),
                   device = c("pc", "phone", "pc"))
  p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                   conversion = "conv", keep = "device")
  expect_error(attribute(p, by = 1), "character vector")
  expect_error(attribute(p, by = NA_character_), "character vector")
  expect_error(attribute(p, by = "region"), "not in")
  expect_error(attribute(p, by = "region"), "keep")
  expect_error(attribute(p, by = "channel"), "cannot define segments")
  expect_error(attribute(p, by = "device"), "one value per journey")
  expect_error(attribute(p, by = "device"), "a#1")
  p$grp <- c("g", "g", NA)
  expect_error(attribute(p, by = "grp"), "missing")
  p$grp <- list(1, 1, 2)
  expect_error(attribute(p, by = "grp"), "vector columns")
  # Repeated names count once.
  p$grp <- "all"
  expect_identical(names(attribute(p, rules = "first", by = c("grp", "grp")))[1],
                   "grp")
})

test_that("a segment without non-converting journeys gets one warning naming it", {
  ev <- data.frame(id = c("a", "b", "c", "d"), ch = c("x", "y", "x", "y"),
                   ts = 1, conv = c(1, 1, 0, 1),
                   grp = c("loyal", "loyal", "new", "new"))
  p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                   conversion = "conv", keep = "grp")
  warned <- character(0)
  res <- withCallingHandlers(
    attribute(p, by = "grp"),
    warning = function(w) {
      warned <<- c(warned, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warned, 1L)
  expect_match(warned, "1 segment has no non-converting")
  expect_match(warned, "loyal")
  expect_equal(sum(res$share[res$grp == "loyal" & res$rule == "markov"]), 1)
  expect_no_warning(attribute(p, rules = "first", by = "grp"))
})

test_that("attribute(by =) on an empty table keeps the segment columns", {
  p <- device_paths()[0, ]
  res <- attribute(p, by = "device")
  expect_identical(nrow(res), 0L)
  expect_identical(names(res), c("device", "rule", "channel", "conversions",
                                 "share", "touches"))
  expect_identical(attr(res, "segments"), "device")
  sp <- attribution_spread(res)
  expect_identical(names(sp)[1:2], c("device", "channel"))
  expect_identical(nrow(sp), 0L)
  boot <- attribute(p, by = "device", times = 5, seed = 1)
  expect_identical(names(boot)[5:7], c("share", "lower", "upper"))
})

test_that("attribution_spread() summarises each segment", {
  p <- device_paths()
  res <- attribute(p, by = "device")
  sp <- attribution_spread(res)
  expect_identical(names(sp)[1:2], c("device", "channel"))
  expect_identical(unique(sp$device), c("desktop", "mobile"))
  for (dv in c("desktop", "mobile")) {
    one <- attribution_spread(attribute(p[p$device == dv, ]))
    expect_equal(plain_df(sp[sp$device == dv, -1L]), plain_df(one))
  }
})

# ---- bootstrap intervals -------------------------------------------------------

boot_paths <- function() {
  data(mm_events, envir = environment())
  build_paths(mm_events, id = "customer_id", channel = "channel",
              timestamp = "timestamp", conversion = "conversion",
              value = "value")
}

test_that("times adds percentile intervals without moving the shares", {
  p <- boot_paths()
  rules <- c("first", "last", "markov")
  b <- attribute(p, rules = rules, times = 30, seed = 1)
  expect_s3_class(b, "mm_attribution_intervals")
  expect_s3_class(b, "mm_attribution")
  expect_identical(names(b), c("rule", "channel", "conversions", "share",
                               "lower", "upper", "value", "touches"))
  expect_identical(attr(b, "level"), 0.9)
  expect_identical(attr(b, "times"), 30L)
  expect_equal(plain_df(b[, setdiff(names(b), c("lower", "upper"))]),
               plain_df(attribute(p, rules = rules)))
  expect_true(all(b$lower <= b$upper))
  expect_true(all(b$lower >= 0 & b$upper <= 1))
  # Most points sit inside their own interval.
  expect_gt(mean(b$lower <= b$share & b$share <= b$upper), 0.8)

  expect_identical(attribute(p, rules = rules, times = 30, seed = 1), b)
  wide <- attribute(p, rules = rules, times = 30, seed = 1, level = 0.99)
  expect_true(all(wide$upper - wide$lower >= b$upper - b$lower - 1e-12))
  other <- attribute(p, rules = rules, times = 30, seed = 2)
  expect_false(identical(other$lower, b$lower))
})

test_that("a replicate equals attribute() on the resampled journeys", {
  p <- boot_paths()
  rules <- c("linear", "first", "last", "position", "time_decay", "markov")
  one <- mediamix:::.mm_attribute_rules(p, rules, 0.4, 0.4,
                                         decay_from_half_life(7), 1,
                                         has_value = TRUE)
  ids <- unique(p$path_id)
  rows <- split(seq_len(nrow(p)), factor(p$path_id, levels = ids))
  for (s in 1:2) {
    set.seed(s)
    draw <- sample.int(length(ids), length(ids), replace = TRUE)
    pieces <- lapply(seq_along(draw), function(d) {
      x <- as.data.frame(p)[rows[[draw[d]]], ]
      x$path_id <- paste0(x$path_id, "~", d)
      x
    })
    resampled <- do.call(rbind, pieces)
    rownames(resampled) <- NULL
    resampled <- structure(resampled, class = c("mm_paths", "data.frame"),
                           mm_units = "days")
    ref <- attribute(resampled, rules = rules)

    set.seed(s)
    got <- mediamix:::.mm_share_replicates(p, rules, one$credit, 1L)$shares
    at <- cbind(1L, match(ref$channel, dimnames(got)[[2L]]),
                match(ref$rule, dimnames(got)[[3L]]))
    expect_equal(got[at], ref$share, tolerance = 1e-12)
  }
})

test_that("seed restores the caller's random numbers, and no seed advances them", {
  p <- boot_paths()
  set.seed(123)
  before <- runif(1)
  set.seed(123)
  attribute(p, rules = "first", times = 5, seed = 1)
  expect_identical(runif(1), before)

  set.seed(123)
  attribute(p, rules = "first", times = 5)
  expect_false(identical(runif(1), before))
  set.seed(9)
  a1 <- attribute(p, rules = "first", times = 5)
  set.seed(9)
  a2 <- attribute(p, rules = "first", times = 5)
  expect_identical(a1, a2)

  # Without times nothing is drawn.
  set.seed(123)
  attribute(p, rules = "first")
  expect_identical(runif(1), before)
})

test_that("intervals at the edges: one journey, no conversions, empty draws", {
  single <- build_paths(data.frame(id = "a", ch = c("x", "y"), ts = c(1, 2),
                                   conv = c(0, 1)),
                        id = "id", channel = "ch", timestamp = "ts",
                        conversion = "conv")
  b <- suppressWarnings(attribute(single, times = 10, seed = 1))
  expect_identical(b$lower, b$share)
  expect_identical(b$upper, b$share)

  ev <- data.frame(id = c("a", "b", "c", "d", "e", "f"),
                   ch = c("x", "y", "x", "y", "x", "y"), ts = 1,
                   conv = c(1, 0, 0, 0, 0, 0),
                   grp = c("g", "g", "g", "h", "h", "h"))
  p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                   conversion = "conv", keep = "grp")
  expect_message(
    res <- attribute(p, rules = c("first", "markov"), by = "grp",
                     times = 40, seed = 3),
    "drew no converting journey"
  )
  expect_true(all(is.na(res$lower[res$grp == "h"])))
  expect_identical(res$share[res$grp == "h"], c(0, 0, 0, 0))
  g <- res[res$grp == "g", ]
  expect_true(all(!is.na(g$lower)))
  expect_message(attribute(p[p$grp == "g", ], rules = "first", times = 40,
                           seed = 3),
                 "of 40 replicates drew no converting")
})

test_that("times, level and seed are validated", {
  p <- boot_paths()[1:50, ]
  expect_error(attribute(p, times = 1), "at least 2")
  expect_error(attribute(p, times = -1), "between 0")
  expect_error(attribute(p, times = 2.5), "whole number")
  expect_error(attribute(p, times = 2e6), "between 0")
  expect_error(attribute(p, times = 5, level = 1), "level")
  expect_error(attribute(p, times = 5, level = NA), "level")
  expect_error(attribute(p, times = 5, seed = "a"), "seed")
  expect_warning(attribute(p, seed = 1), "`seed` has no effect")
  expect_warning(attribute(p, level = 0.8, seed = 1),
                 "`level` and `seed` have no effect")
  expect_no_warning(attribute(p, times = 0, rules = "first"))
  # 20 channels under 6 rules for a million replicates would need 1.2e8
  # stored shares; the call aborts before drawing any.
  wide <- build_paths(data.frame(id = "a", ch = paste0("c", 1:20), ts = 1:20,
                                 conv = c(rep(0, 19), 1)),
                      id = "id", channel = "ch", timestamp = "ts",
                      conversion = "conv")
  expect_error(suppressWarnings(attribute(wide, times = 1e6)), "too many")
})
