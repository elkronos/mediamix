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
