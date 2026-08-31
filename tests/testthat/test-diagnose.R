# Tests for diagnose_media() (R/diagnose.R).
#
# NOTE: every scenario here uses at least THREE media columns. With exactly
# two, diagnose_media() errors -- see the bug reported in the test-suite
# summary (`.mm_vif_table()`'s `cors[i, -i]` drops dimnames when there are
# exactly two columns, because indexing a matrix down to a single row and a
# single column collapses to an unnamed scalar in base R). Using three
# columns is also the more realistic case for this function and lets the
# "deliberately collinear pair" scenario include an innocent third channel.

test_that("diagnose_media() flags collinearity on a deliberately collinear pair", {
  set.seed(1)
  n <- 60
  base <- runif(n, 100, 1000)
  d <- data.frame(
    tv = base + rnorm(n, 0, 1),
    video = base * 1.01 + rnorm(n, 0, 1),   # near-duplicate of tv
    search = runif(n, 100, 1000)             # independent
  )
  diag <- diagnose_media(d, media = c("tv", "video", "search"))

  expect_s3_class(diag, "mm_diagnosis")
  hi_vif <- diag$collinearity$channel[!is.na(diag$collinearity$vif) &
                                         diag$collinearity$vif > 5]
  expect_setequal(hi_vif, c("tv", "video"))
  expect_false("search" %in% hi_vif)
  expect_true(any(grepl("Collinearity", diag$flags)))
  expect_true(any(grepl("tv", diag$flags) & grepl("video", diag$flags)))

  # the independent channel is correctly identified (not flagged)
  search_row <- diag$collinearity[diag$collinearity$channel == "search", ]
  expect_true(isTRUE(search_row$identified))
})

test_that("diagnose_media() flags a near-constant column as low variation", {
  set.seed(2)
  n <- 60
  d <- data.frame(
    tv = rnorm(n, 1000, 1),          # essentially constant: cv ~ 0.001
    search = runif(n, 100, 1000),
    social = runif(n, 200, 900)
  )
  diag <- diagnose_media(d, media = c("tv", "search", "social"))

  tv_row <- diag$variation[diag$variation$channel == "tv", ]
  expect_true(tv_row$low_variation)
  expect_lt(tv_row$cv, 0.15)
  expect_false(diag$variation$low_variation[diag$variation$channel == "search"])
  expect_true(any(grepl("Low variation", diag$flags)))
  expect_true(any(grepl("tv", diag$flags[grepl("Low variation", diag$flags)])))
})

test_that("diagnose_media() flags a mostly-zero column as heavily flighted", {
  set.seed(3)
  n <- 60
  sp <- runif(n, 100, 1000)
  sp[sample(n, floor(n * 0.7))] <- 0   # dark 70% of the time
  d <- data.frame(tv = sp, search = runif(n, 100, 1000),
                   social = runif(n, 200, 900))
  diag <- diagnose_media(d, media = c("tv", "search", "social"))

  tv_row <- diag$variation[diag$variation$channel == "tv", ]
  expect_true(tv_row$heavily_flighted)
  expect_gt(tv_row$zero_share, 0.5)
  expect_false(diag$variation$heavily_flighted[diag$variation$channel == "search"])
  expect_true(any(grepl("Heavily flighted", diag$flags)))
})

test_that("diagnose_media() returns zero flags on clean data", {
  set.seed(4)
  n <- 60
  d <- data.frame(
    tv = runif(n, 100, 1000),
    search = runif(n, 200, 900),
    social = runif(n, 50, 800)
  )
  diag <- diagnose_media(d, media = c("tv", "search", "social"))

  expect_identical(diag$flags, character(0))
  expect_true(all(is.na(diag$collinearity$vif) | diag$collinearity$vif <= 5))
  expect_false(any(diag$variation$low_variation))
  expect_false(any(diag$variation$heavily_flighted))
})

test_that("diagnose_media() computes CPM outliers when spend and impressions are supplied", {
  set.seed(5)
  n <- 40
  imp <- runif(n, 10000, 20000)
  # ~2% jitter around a $5 CPM, so the MAD is non-zero and the guard in
  # .mm_cpm_table() (which requires madv > 0) can actually engage
  spend <- imp / 1000 * 5 * (1 + rnorm(n, 0, 0.02))
  spend[5] <- spend[5] * 20 # one wildly outlying period
  d <- data.frame(
    tv = runif(n, 100, 1000), search = runif(n, 100, 900),
    social = runif(n, 100, 900),
    tv_spend = spend, tv_impressions = imp
  )
  diag <- diagnose_media(d, media = c("tv", "search", "social"),
                          spend = "tv_spend", impressions = "tv_impressions")
  expect_false(is.null(diag$cpm))
  expect_true(diag$cpm$outlier[5])
  expect_true(any(grepl("CPM", diag$flags)))
})

test_that("diagnose_media() validates its inputs", {
  d <- data.frame(tv = 1:5, search = 1:5, social = 1:5)
  expect_error(diagnose_media(as.matrix(d), media = c("tv", "search")),
               "data frame")
  expect_error(diagnose_media(d, media = character(0)), "non-empty character")
  expect_error(diagnose_media(d, media = c("tv", "not_a_column")),
               "not_a_column")
})

test_that("print.mm_diagnosis runs without error", {
  set.seed(6)
  n <- 40
  d <- data.frame(tv = runif(n, 100, 1000), search = runif(n, 200, 900),
                   social = runif(n, 50, 800))
  diag <- diagnose_media(d, media = c("tv", "search", "social"))
  expect_no_error(capture.output(print(diag)))
})
