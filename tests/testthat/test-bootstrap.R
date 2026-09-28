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

test_that("groups keep their sizes and seeds make results reproducible", {
  d <- data.frame(g = rep(c("a", "b"), c(30, 50)), y = seq_len(80))
  sizes <- function(d) c(a = sum(d$g == "a"), b = sum(d$g == "b"))
  b <- block_bootstrap(d, sizes, times = 20, by = "g", block_length = 7,
                       seed = 9)
  reps <- attr(b, "replicates")
  expect_true(all(reps[, "a"] == 30) && all(reps[, "b"] == 50))
  again <- block_bootstrap(d, sizes, times = 20, by = "g", block_length = 7,
                           seed = 9)
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
