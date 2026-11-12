# Tests for block_bootstrap().

test_that("the interval brackets the estimate and the truth for a simple mean", {
  set.seed(1)
  d <- data.frame(y = as.numeric(stats::arima.sim(list(ar = 0.6), n = 300)) + 5)
  b <- block_bootstrap(d, function(d) c(mean = mean(d$y)), times = 300,
                       block_length = 10, seed = 2)
  expect_s3_class(b, "mm_bootstrap")
  expect_true(b$lower < b$estimate && b$estimate < b$upper)
  expect_true(b$lower < 5 && 5 < b$upper)
  expect_identical(dim(attr(b, "replicates")), c(300L, 1L))
})

test_that("blocks keep autocorrelation, so intervals are wider than iid ones", {
  set.seed(3)
  d <- data.frame(y = as.numeric(stats::arima.sim(list(ar = 0.8), n = 400)))
  stat <- function(d) c(mean = mean(d$y))
  iid <- block_bootstrap(d, stat, times = 400, block_length = 1, seed = 4)
  blk <- block_bootstrap(d, stat, times = 400, block_length = 20, seed = 4)
  expect_gt(blk$std_error, 1.5 * iid$std_error)
})

test_that("independent groups keep their sizes and seeds make results reproducible", {
  d <- data.frame(g = rep(c("a", "b"), c(30, 50)), y = seq_len(80))
  sizes <- function(d) c(a = sum(d$g == "a"), b = sum(d$g == "b"))
  b <- block_bootstrap(d, sizes, times = 20, by = "g", block_length = 7,
                       seed = 9, panel = "independent")
  reps <- attr(b, "replicates")
  expect_true(all(reps[, "a"] == 30) && all(reps[, "b"] == 50))
  again <- block_bootstrap(d, sizes, times = 20, by = "g", block_length = 7,
                           seed = 9, panel = "independent")
  expect_identical(attr(again, "replicates"), reps)

  set.seed(123); before <- runif(1)
  set.seed(123); block_bootstrap(d, sizes, times = 5, seed = 1); after <- runif(1)
  expect_identical(before, after)   # the caller's RNG stream is untouched
})

test_that("failed replicates are dropped and reported", {
  d <- data.frame(y = 1:50)
  n_call <- 0
  flaky <- function(d) {
    n_call <<- n_call + 1
    if (n_call %% 4 == 0) stop("boom")
    c(m = mean(d$y))
  }
  expect_message(b <- block_bootstrap(d, flaky, times = 20, seed = 1),
                 "failed")
  expect_lt(attr(b, "times"), 20)
  expect_error(block_bootstrap(d, function(d) "a"), "numeric")
  expect_no_error(capture.output(suppressMessages(print(b))))
})

# ---- panels (BD-01) -----------------------------------------------------------

panel_data <- function(n_t = 60, groups = c("a", "b", "c", "d"), seed = 1) {
  set.seed(seed)
  shock <- as.numeric(stats::arima.sim(list(ar = 0.5), n = n_t))
  do.call(rbind, lapply(groups, function(g) {
    data.frame(geo = g, t = seq_len(n_t), y = shock + stats::rnorm(n_t, sd = 0.2))
  }))
}

test_that("with by, every group takes the same periods by default", {
  d <- panel_data()
  same_periods <- function(d) {
    ts <- split(d$t, d$geo)
    c(same = as.numeric(all(vapply(ts, identical, logical(1), ts[[1L]]))))
  }
  shared <- block_bootstrap(d, same_periods, times = 20, by = "geo",
                            block_length = 6, seed = 1)
  expect_true(all(attr(shared, "replicates")[, "same"] == 1))
  expect_identical(attr(shared, "panel"), "shared")
  indep <- block_bootstrap(d, same_periods, times = 20, by = "geo",
                           block_length = 6, seed = 1, panel = "independent")
  expect_true(all(attr(indep, "replicates")[, "same"] == 0))
})

test_that("a shock common to every group widens the interval of a pooled mean", {
  d <- panel_data(n_t = 120, seed = 2)
  pooled <- function(d) c(mean = mean(d$y))
  shared <- block_bootstrap(d, pooled, times = 200, by = "geo",
                            block_length = 8, seed = 3)
  indep <- block_bootstrap(d, pooled, times = 200, by = "geo",
                           block_length = 8, seed = 3, panel = "independent")
  # Independent draws average the common shock away across the four groups.
  expect_gt(shared$std_error, 1.5 * indep$std_error)
})

test_that("groups of different lengths need index, which aligns them by period", {
  d <- panel_data(n_t = 40, groups = c("a", "b"))
  d <- d[!(d$geo == "b" & d$t <= 10), ]
  expect_error(block_bootstrap(d, function(d) c(m = mean(d$y)), times = 5,
                               by = "geo"), "same number of rows")
  d <- d[sample(nrow(d)), ]
  counts <- function(d) {
    a <- d$t[d$geo == "a"]
    b <- d$t[d$geo == "b"]
    c(n_a = length(a), n_b = length(b),
      aligned = as.numeric(identical(b, a[a > 10])))
  }
  r <- attr(block_bootstrap(d, counts, times = 30, by = "geo", index = "t",
                            block_length = 5, seed = 2), "replicates")
  expect_true(all(r[, "n_a"] == 40))
  expect_true(all(r[, "n_b"] <= 40) && any(r[, "n_b"] != 30))
  expect_true(all(r[, "aligned"] == 1))
  expect_error(block_bootstrap(rbind(d, d[1, ]), counts, times = 5, by = "geo",
                               index = "t"), "repeats a period")
})

test_that("data.table input works with by", {
  d <- panel_data(n_t = 30, groups = c("a", "b"))
  stat <- function(z) c(m = mean(z$y))
  from_dt <- block_bootstrap(data.table::as.data.table(d), stat, times = 20,
                             by = "geo", seed = 1)
  from_df <- block_bootstrap(d, stat, times = 20, by = "geo", seed = 1)
  expect_equal(as.data.frame(from_dt), as.data.frame(from_df))
})

test_that("a dplyr-grouped frame passed without by gets a message", {
  fake <- structure(
    data.frame(geo = c("a", "b"), y = 1:2),
    class = c("grouped_df", "data.frame"),
    groups = data.frame(geo = c("a", "b"))
  )
  expect_message(.mm_note_dplyr_groups(fake, NULL, "block_bootstrap"),
                 'by = "geo"')
  expect_silent(.mm_note_dplyr_groups(fake, "geo", "block_bootstrap"))
  expect_silent(.mm_note_dplyr_groups(as.data.frame(fake), NULL, "x"))
})

# ---- resampling and replicates (BD-02 to BD-06, BD-09) -------------------------

test_that("a statistic that sets its own seed does not make resamples repeat", {
  set.seed(5)
  d <- data.frame(y = stats::rnorm(80))
  reseeding <- function(d) {
    set.seed(123)
    c(m = mean(d$y) + stats::runif(1) * 0)
  }
  b <- block_bootstrap(d, reseeding, times = 50, seed = 1)
  expect_gt(length(unique(round(attr(b, "replicates")[, "m"], 10))), 40L)
})

test_that("missing and infinite terms are counted per term, not hidden", {
  d <- data.frame(y = 1:60)
  n_call <- 0
  patchy <- function(d) {
    n_call <<- n_call + 1
    c(a = mean(d$y), b = if (n_call %% 3 == 0) NA else 1,
      c = if (n_call %% 5 == 0) Inf else 2)
  }
  expect_message(b <- block_bootstrap(d, patchy, times = 30, seed = 1),
                 "b \\(\\d+\\), c \\(\\d+\\)")
  expect_identical(attr(b, "times"), 30L)
  expect_true(all(is.finite(b$std_error)))
  expect_equal(b$std_error[b$term == "c"], 0)
})

test_that("block_length is checked against the series it resamples", {
  d <- data.frame(y = stats::rnorm(40))
  stat <- function(d) c(m = mean(d$y))
  expect_error(block_bootstrap(d, stat, times = 5, block_length = 40),
               "shorter than the series")
  expect_warning(block_bootstrap(d, stat, times = 5, block_length = 25,
                                 seed = 1), "more than half")
  g <- data.frame(geo = rep(c("a", "b"), c(30, 5)), y = 1:35)
  expect_error(block_bootstrap(g, stat, times = 5, block_length = 6,
                               by = "geo", panel = "independent"),
               "5 rows")
})

test_that("replicates are matched to the estimate by name", {
  set.seed(11)
  d <- data.frame(k = sample(c("a", "b"), 80, replace = TRUE),
                  y = stats::rnorm(80))
  # unique() returns the names in whatever order the resample meets them.
  by_level <- function(d) {
    u <- unique(d$k)
    stats::setNames(vapply(u, function(z) {
      mean(d$y[d$k == z]) + if (z == "a") 100 else 0
    }, numeric(1)), u)
  }
  b <- block_bootstrap(d, by_level, times = 50, seed = 1)
  expect_true(all(attr(b, "replicates")[, "a"] > 90))
  expect_true(all(attr(b, "replicates")[, "b"] < 10))
  # A name the estimate does not have fails the replicate.
  expect_error(block_bootstrap(d, function(x) {
    if (identical(x, d)) c(a = 1) else c(z = 1)
  }, times = 5, seed = 1), "does not have")
})

test_that("the seed is validated and the point estimate is computed after it", {
  d <- data.frame(y = stats::rnorm(50))
  stat <- function(d) c(m = mean(d$y))
  for (bad in list(NA, Inf, c(1, 2), 1.5, "1", 3e9)) {
    expect_error(block_bootstrap(d, stat, times = 3, seed = bad),
                 "one finite whole number")
  }
  noisy <- function(d) c(m = mean(d$y) + stats::runif(1))
  set.seed(1); e1 <- block_bootstrap(d, noisy, times = 3, seed = 5)$estimate
  set.seed(99); e2 <- block_bootstrap(d, noisy, times = 3, seed = 5)$estimate
  expect_identical(e1, e2)
})

test_that("counts are bounded before anything is allocated", {
  d <- data.frame(y = stats::rnorm(50))
  stat <- function(d) c(m = mean(d$y))
  expect_error(block_bootstrap(d, stat, times = 1e9), "between 2 and")
  expect_error(block_bootstrap(d, stat, times = 2.5), "whole number")
  expect_error(block_bootstrap(d, stat, times = 3, panel = "both"), "one of")
})

test_that("circular blocks draw the first row as often as any other", {
  d <- data.frame(row = seq_len(60))
  first <- function(d) c(n = sum(d$row == 1))
  moving <- block_bootstrap(d, first, times = 300, block_length = 6, seed = 2)
  circular <- block_bootstrap(d, first, times = 300, block_length = 6,
                              seed = 2, circular = TRUE)
  expect_lt(mean(attr(moving, "replicates")[, "n"]), 0.3)
  expect_gt(mean(attr(circular, "replicates")[, "n"]), 0.7)
  expect_true(attr(circular, "circular"))
})

test_that("when every replicate fails, the first error is reported", {
  d <- data.frame(y = stats::rnorm(40))
  picky <- function(x) if (identical(x, d)) c(m = 1) else stop("singular fit")
  expect_error(block_bootstrap(d, picky, times = 5, seed = 1), "singular fit")
  n_call <- 0
  flaky <- function(d) {
    n_call <<- n_call + 1
    if (n_call == 3) stop("boom at call 3")
    c(m = mean(d$y))
  }
  expect_message(block_bootstrap(d, flaky, times = 10, seed = 1),
                 "boom at call 3")
})

# ---- the result behaves like data (RP-09, BD-09) --------------------------------

test_that("subsetting keeps the replicates in step with the terms", {
  set.seed(7)
  d <- data.frame(a = stats::rnorm(50), b = stats::rnorm(50),
                  c = stats::rnorm(50))
  b <- block_bootstrap(d, colMeans, times = 20, seed = 1)
  reps <- attr(b, "replicates")

  two <- b[b$term != "a", ]
  expect_s3_class(two, "mm_bootstrap")
  expect_identical(attr(two, "replicates"), reps[, c("b", "c")])
  flipped <- b[3:1, ]
  expect_identical(colnames(attr(flipped, "replicates")), c("c", "b", "a"))
  expect_identical(colnames(attr(head(b, 1), "replicates")), "a")
  expect_no_error(capture.output(suppressMessages(print(two))))

  # Without all five columns the result is a plain data frame.
  cols <- b[, 1:2]
  expect_identical(class(cols), "data.frame")
  expect_null(attr(cols, "replicates"))

  plain <- as.data.frame(b)
  expect_identical(class(plain), "data.frame")
  expect_null(attr(plain, "replicates"))

  sliced <- structure(as.data.frame(b)[2, ], class = class(b),
                      replicates = reps)
  rebuilt <- dplyr_reconstruct.mm_bootstrap(sliced, b)
  expect_identical(colnames(attr(rebuilt, "replicates")), "b")
})

test_that("tidy() uses the broom column names", {
  skip_if_not_installed("generics")
  d <- data.frame(a = stats::rnorm(40), b = stats::rnorm(40))
  b <- block_bootstrap(d, colMeans, times = 20, seed = 1)
  td <- generics::tidy(b)
  expect_identical(names(td),
                   c("term", "estimate", "std.error", "conf.low", "conf.high"))
  expect_identical(td$conf.low, b$lower)
})
