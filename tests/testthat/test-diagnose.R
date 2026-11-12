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

test_that("a CPM outlier is flagged even when most CPMs are identical", {
  set.seed(7)
  d <- data.frame(tv = runif(10), search = runif(10), social = runif(10),
                  imp = 1000, spend = c(rep(5, 9), 500))
  diag <- diagnose_media(d, media = c("tv", "search", "social"),
                         spend = "spend", impressions = "imp")
  expect_identical(which(diag$cpm$outlier), 10L)
  expect_true(any(grepl("CPM", diag$flags)))
  d$spend[10] <- 6   # a 20% price change is not a join error
  calm <- diagnose_media(d, media = c("tv", "search", "social"),
                         spend = "spend", impressions = "imp")
  expect_false(any(calm$cpm$outlier))
})

test_that("spend is read as numbers, and errors name diagnose_media()", {
  set.seed(8)
  d <- data.frame(tv = runif(10), search = runif(10), social = runif(10),
                  imp = factor(rep(1000, 10)), spend = factor(rep(5, 10)))
  diag <- diagnose_media(d, media = c("tv", "search", "social"),
                         spend = "spend", impressions = "imp")
  expect_equal(diag$cpm$cpm, rep(5, 10))
  d$spend <- c(rep("5", 9), "1,000")
  expect_error(diagnose_media(d, media = c("tv", "search"), spend = "spend",
                              impressions = "imp"), "\"spend\"")
  err <- tryCatch(diagnose_media(d, media = "tv", spend = c("spend", "imp"),
                                 impressions = "imp"), error = identity)
  expect_identical(conditionCall(err)[[1]], as.name("diagnose_media"))
})

test_that("data.table input gives the same diagnosis as a data frame", {
  data(mm_weekly, package = "mediamix", envir = environment())
  ch <- c("tv", "search", "social")
  dt <- data.table::as.data.table(mm_weekly)
  expect_equal(unclass(diagnose_media(dt, media = ch, by = "geo")),
               unclass(diagnose_media(mm_weekly, media = ch, by = "geo")))
  expect_equal(unclass(diagnose_media(dt, media = ch)),
               unclass(diagnose_media(mm_weekly, media = ch)))
})

test_that("a dplyr-grouped frame without by is diagnosed as one series, with a message", {
  set.seed(9)
  d <- data.frame(geo = rep(c("a", "b"), each = 20), tv = runif(40),
                  search = runif(40))
  fake <- structure(d, class = c("grouped_df", "data.frame"),
                    groups = data.frame(geo = c("a", "b")))
  expect_message(grouped <- diagnose_media(fake, media = c("tv", "search")),
                 'by = "geo"')
  expect_equal(unclass(grouped),
               unclass(diagnose_media(d, media = c("tv", "search"))))
  expect_no_message(diagnose_media(fake, media = c("tv", "search"),
                                   by = "geo"))
})

test_that("duplicate media names are refused", {
  d <- data.frame(tv = runif(10), search = runif(10))
  expect_error(diagnose_media(d, media = c("tv", "tv", "search")),
               "each column once")
})

test_that("missing spend under decay is reported", {
  set.seed(10)
  d <- data.frame(tv = runif(60), search = runif(60), social = runif(60))
  d$tv[c(5, 9)] <- NA
  expect_message(diagnose_media(d, media = c("tv", "search", "social"),
                                decay = 0.5), "tv \\(2\\)")
  expect_no_message(diagnose_media(d, media = c("tv", "search", "social")))
})

test_that("as.data.frame() and tidy() give one row per channel", {
  set.seed(11)
  d <- data.frame(tv = runif(60), search = runif(60), social = runif(60))
  diag <- diagnose_media(d, media = c("tv", "search", "social"))
  out <- as.data.frame(diag)
  expect_identical(out$channel, c("tv", "search", "social"))
  expect_true(all(c("cv", "zero_share", "vif", "identified") %in% names(out)))
  expect_equal(out$vif[out$channel == "search"],
               diag$collinearity$vif[diag$collinearity$channel == "search"])
  skip_if_not_installed("generics")
  expect_identical(generics::tidy(diag), out)
})

# ---- the time index (date =) ----------------------------------------------------

weekly_panel <- function(seed = 12) {
  set.seed(seed)
  start <- as.Date("2024-01-01")
  d <- do.call(rbind, lapply(c("north", "south", "west"), function(g) {
    data.frame(geo = g, week = start + 7 * (0:19), tv = runif(20),
               search = runif(20), social = runif(20))
  }))
  d
}

test_that("date counts rows out of order, repeated periods and gaps per series", {
  d <- weekly_panel()
  north <- which(d$geo == "north")
  south <- which(d$geo == "south")
  d[north[4:5], ] <- d[north[5:4], ]           # two weeks swapped
  d <- d[-south[c(8, 9, 15)], ]                # three weeks dropped
  d <- rbind(d, d[d$geo == "west", ][3, ])     # one week given twice
  diag <- diagnose_media(d, media = c("tv", "search", "social"), by = "geo",
                         date = "week")
  dt <- diag$dates
  expect_identical(dt$group, c("north", "south", "west"))
  expect_identical(dt$rows, c(20L, 17L, 21L))
  expect_identical(dt$out_of_order, c(1L, 0L, 1L))
  expect_identical(dt$duplicated, c(0L, 0L, 1L))
  expect_equal(dt$missing, c(0, 3, 0))
  expect_identical(dt$irregular, c(0L, 2L, 0L))
  expect_equal(dt$period, c(7, 7, 7))
  expect_identical(dt$start, rep(as.Date("2024-01-01"), 3))
  expect_identical(dt$end, rep(as.Date("2024-01-01") + 7 * 19, 3))
  expect_match(diag$flags[1], "^Out of time order: 2 row\\(s\\) in 2 series \\(north, west\\)")
  expect_match(diag$flags[2], "^Repeated periods: 1 row\\(s\\) in 1 series \\(west\\)")
  expect_match(diag$flags[3], "^Missing periods: 3 period\\(s\\) in 1 series \\(south\\)")
  expect_true(all(grepl("adjacent periods|one row per period|one period",
                        diag$flags[1:3])))
})

test_that("a clean panel passes, and print() reports the time index", {
  data(mm_weekly, package = "mediamix", envir = environment())
  ch <- c("tv", "search", "social")
  diag <- diagnose_media(mm_weekly, media = ch, by = "geo", date = "date")
  expect_identical(diag$flags, character(0))
  expect_true(all(diag$dates$out_of_order == 0 & diag$dates$duplicated == 0 &
                    diag$dates$missing == 0 & diag$dates$irregular == 0))
  printed <- paste(capture.output(print(diag), type = "message"),
                   collapse = " ")
  expect_match(printed, "every series is in order")
  expect_match(printed, "\\$dates")

  messy <- mm_weekly[-c(4, 9), ]
  printed <- paste(capture.output(
    print(diagnose_media(messy, media = ch, by = "geo", date = "date")),
    type = "message"), collapse = " ")
  expect_match(printed, "1 of 3 series has rows out of order")
})

test_that("without date the result has no dates table and is otherwise the same", {
  d <- weekly_panel()
  d <- d[-5, ]
  ch <- c("tv", "search", "social")
  plain <- diagnose_media(d, media = ch, by = "geo")
  dated <- diagnose_media(d, media = ch, by = "geo", date = "week")
  expect_null(plain$dates)
  expect_true("dates" %in% names(plain))
  expect_identical(plain$variation, dated$variation)
  expect_identical(plain$collinearity, dated$collinearity)
  expect_identical(plain$flags, setdiff(dated$flags, dated$flags[1]))
  printed <- paste(capture.output(print(plain), type = "message"),
                   collapse = " ")
  expect_no_match(printed, "Time index")
  expect_no_match(printed, "\\$dates")
})

test_that("each series is judged on its own spacing", {
  set.seed(13)
  # Weekly series reporting on different weekdays interleave when pooled.
  d <- data.frame(geo = rep(c("mon", "thu"), each = 12),
                  week = c(as.Date("2024-01-01") + 7 * 0:11,
                           as.Date("2024-01-04") + 7 * 0:11),
                  tv = runif(24), search = runif(24))
  diag <- diagnose_media(d, media = c("tv", "search"), by = "geo",
                         date = "week")
  expect_identical(diag$flags, character(0))
  expect_equal(diag$dates$period, c(7, 7))

  # Calendar months of 28 to 31 days are regular; a skipped month is not.
  months <- seq(as.Date("2023-01-01"), by = "month", length.out = 24)
  m <- data.frame(month = months, tv = runif(24), search = runif(24))
  expect_identical(diagnose_media(m, media = c("tv", "search"),
                                  date = "month")$flags, character(0))
  gap <- diagnose_media(m[-7, ], media = c("tv", "search"), date = "month")
  expect_equal(gap$dates$missing, 1)
  expect_match(gap$flags, "^Missing periods: 1 period\\(s\\) are absent")
})

test_that("a step that is not a whole number of periods is uneven", {
  set.seed(14)
  # The reporting day moves from Monday to Thursday after eight weeks.
  week <- as.Date("2024-01-01") + c(7 * 0:7, 52 + 7 * 0:7)
  d <- data.frame(week = week, tv = runif(16), search = runif(16))
  diag <- diagnose_media(d, media = c("tv", "search"), date = "week")
  expect_identical(diag$dates$irregular, 1L)
  expect_equal(diag$dates$missing, 0)
  expect_match(diag$flags, "^Uneven spacing: 1 step\\(s\\) differ")
})

test_that("numeric and POSIXct time columns work, in their own units", {
  set.seed(15)
  hours <- as.POSIXct("2024-03-30 00:00", tz = "UTC") + 3600 * c(0:5, 8:11)
  d <- data.frame(t = hours, n = c(1:6, 9:12), tv = runif(10),
                  search = runif(10))
  by_time <- diagnose_media(d, media = c("tv", "search"), date = "t")$dates
  expect_equal(by_time$period, 3600)
  expect_equal(by_time$missing, 2)
  expect_s3_class(by_time$start, "POSIXct")
  by_number <- diagnose_media(d, media = c("tv", "search"), date = "n")$dates
  expect_equal(by_number$period, 1)
  expect_equal(by_number$missing, 2)
  expect_identical(by_number$end, 12L)
})

test_that("a single row, a series with one period and pooled panels", {
  set.seed(16)
  one <- data.frame(week = as.Date("2024-01-01"), tv = 1, search = 2)
  single <- diagnose_media(one, media = c("tv", "search"), date = "week")
  expect_identical(single$dates$rows, 1L)
  expect_true(is.na(single$dates$period))
  expect_identical(single$dates$irregular, 0L)
  expect_false(any(grepl("order|Repeated|Missing periods|Uneven",
                         single$flags)))

  # A series with one time point borrows the other series' spacing.
  d <- data.frame(geo = c(rep("a", 6), "b"), week = c(1:6, 4),
                  tv = runif(7), search = runif(7))
  short <- diagnose_media(d, media = c("tv", "search"), by = "geo",
                          date = "week")$dates
  expect_equal(short$period, c(1, 1))
  expect_identical(short$missing, c(0, 0))

  # Stacked geographies without `by` repeat every period.
  pooled <- diagnose_media(weekly_panel(), media = c("tv", "search"),
                           date = "week")
  expect_identical(pooled$dates$duplicated, 40L)
  expect_true(any(grepl("Repeated periods.*`by`", pooled$flags)))
})

test_that("date is validated", {
  d <- weekly_panel()
  ch <- c("tv", "search")
  expect_error(diagnose_media(d, media = ch, date = c("week", "geo")),
               "single column name")
  expect_error(diagnose_media(d, media = ch, date = 3), "single column name")
  expect_error(diagnose_media(d, media = ch, date = "day"), "\"day\"")
  d$label <- format(d$week)
  expect_error(diagnose_media(d, media = ch, date = "label"),
               "numeric, <Date> or <POSIXct>")
  d$week[c(5, 30)] <- NA
  err <- tryCatch(diagnose_media(d, media = ch, by = "geo", date = "week"),
                  error = identity)
  expect_s3_class(err, "rlang_error")
  expect_match(conditionMessage(err), "2 missing or infinite values")
  expect_match(conditionMessage(err), "north")
  expect_identical(conditionCall(err)[[1]], as.name("diagnose_media"))
})

test_that("series names with braces print as they are", {
  d <- weekly_panel()
  d$geo[d$geo == "north"] <- "{north}"
  d <- d[-3, ]
  diag <- diagnose_media(d, media = c("tv", "search"), by = "geo",
                         date = "week")
  expect_match(diag$flags, "\\{north\\}")
  printed <- paste(capture.output(print(diag), type = "message"),
                   collapse = " ")
  expect_match(printed, "{north}", fixed = TRUE)
})

test_that("data.table input and the plot method work with date", {
  d <- weekly_panel()[-7, ]
  ch <- c("tv", "search", "social")
  from_df <- diagnose_media(d, media = ch, by = "geo", date = "week")
  from_dt <- diagnose_media(data.table::as.data.table(d), media = ch,
                            by = "geo", date = "week")
  expect_equal(unclass(from_dt), unclass(from_df))
  draw <- function(x) {
    f <- tempfile(fileext = ".pdf")
    grDevices::pdf(f)
    on.exit({ grDevices::dev.off(); unlink(f) })
    plot(x)
  }
  expect_identical(draw(from_df), from_df)
})
