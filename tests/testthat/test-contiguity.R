# Tests for the internal contiguity guard (R/contiguity.R):
# .mm_carry_case, .mm_infer_period, .mm_group_periods, .mm_gap_size,
# .mm_irregular_steps and the index checks.

carry_case <- mediamix:::.mm_carry_case
infer_period <- mediamix:::.mm_infer_period
group_periods <- mediamix:::.mm_group_periods
gap_size <- mediamix:::.mm_gap_size
irregular_steps <- mediamix:::.mm_irregular_steps

case_of <- function(...) carry_case(...)$case
k_of <- function(...) carry_case(...)$k

# ---- every case, plain numeric index ---------------------------------------

test_that(".mm_carry_case classifies every case on a plain numeric index", {
  idx <- 1:10
  period <- infer_period(idx)
  expect_identical(period, 1)

  expect_identical(carry_case(11, idx, period), list(case = "contiguous", k = 10L))
  expect_identical(carry_case(10, idx, period), list(case = "overlap", k = 9L))
  expect_identical(carry_case(5, idx, period), list(case = "overlap", k = 4L))
  # starting on or before the first training point: nothing precedes it
  expect_identical(carry_case(1, idx, period), list(case = "overlap", k = 0L))
  expect_identical(carry_case(-3, idx, period), list(case = "overlap", k = 0L))
  expect_identical(case_of(13, idx, period), "gap")
  expect_identical(case_of(11, NULL, period), "unseen")
  expect_identical(case_of(11, NA_real_, period), "unseen")
  expect_identical(case_of(11, numeric(0), period), "unseen")
})

test_that("a block starting part-way through a period is misaligned, not an overlap", {
  idx <- 1:10
  # after the end of training, but less than one period later
  expect_identical(carry_case(10.5, idx, 1), list(case = "misaligned", k = 10L))
  expect_identical(case_of(10.1, idx, 1), "misaligned")
  # inside the training span, off the training grid
  expect_identical(carry_case(5.5, idx, 1), list(case = "misaligned", k = 5L))
  # within tolerance of one period still continues the series
  expect_identical(case_of(10.8, idx, 1), "contiguous")
  expect_identical(case_of(11.2, idx, 1), "contiguous")
})

test_that("a block on a training time point lines up even where training has a gap", {
  idx <- c(1, 2, 3, 10, 11, 12)
  expect_identical(carry_case(10, idx, 1), list(case = "overlap", k = 3L))
  # off the training points, inside the hole: a gap after the point before it
  expect_identical(carry_case(7, idx, 1), list(case = "gap", k = 3L))
})

test_that("an unknown period is reported only when alignment has to be judged", {
  expect_identical(case_of(11, 10, NA_real_), "unknown_period")
  expect_identical(case_of(11, 10, 0), "unknown_period")
  # starting on the only training point needs no period
  expect_identical(carry_case(10, 10, NA_real_), list(case = "overlap", k = 0L))
})

# ---- Date index -------------------------------------------------------------

test_that(".mm_carry_case classifies every case on a Date index", {
  d <- seq(as.Date("2024-01-01"), by = "day", length.out = 10)
  period <- infer_period(d)
  expect_identical(period, 1)

  idx <- as.numeric(d)
  expect_identical(case_of(as.numeric(d[10] + 1), idx, period), "contiguous")
  expect_identical(carry_case(as.numeric(d[10]), idx, period),
                   list(case = "overlap", k = 9L))
  expect_identical(carry_case(as.numeric(d[3]), idx, period),
                   list(case = "overlap", k = 2L))
  expect_identical(case_of(as.numeric(d[10] + 20), idx, period), "gap")
  expect_identical(case_of(as.numeric(d[10] + 1), NULL, period), "unseen")

  weeks <- as.numeric(seq(as.Date("2024-01-01"), by = "week", length.out = 10))
  # the day after the last training Monday
  expect_identical(case_of(weeks[10] + 1, weeks, 7), "misaligned")
})

# ---- POSIXct index -----------------------------------------------------------

test_that(".mm_carry_case classifies every case on a POSIXct index", {
  pt <- as.POSIXct("2024-01-01", tz = "UTC") + (0:9) * 86400
  period <- infer_period(pt)
  expect_identical(period, 86400)

  idx <- as.numeric(pt)
  expect_identical(case_of(as.numeric(pt[10] + 86400), idx, period),
                   "contiguous")
  expect_identical(case_of(as.numeric(pt[10]), idx, period), "overlap")
  expect_identical(case_of(as.numeric(pt[5]), idx, period), "overlap")
  expect_identical(case_of(as.numeric(pt[10] + 86400 * 10), idx, period),
                   "gap")
  expect_identical(case_of(as.numeric(pt[10] + 3600), idx, period),
                   "misaligned")
  expect_identical(case_of(as.numeric(pt[10] + 86400), NA_real_, period),
                   "unseen")
})

# ---- calendar months: every step contiguous, a 2-month skip is a gap -------

test_that("calendar-monthly steps classify as contiguous; a two-month skip is a gap", {
  months <- seq(as.Date("2020-01-01"), by = "month", length.out = 24)
  period <- infer_period(months)
  # months are 28-31 days; the loose 0.25 relative tolerance absorbs that
  expect_true(period >= 28 && period <= 31)

  idx <- as.numeric(months)
  for (i in 2:length(months)) {
    case <- case_of(idx[i], idx[seq_len(i - 1)], period)
    expect_identical(case, "contiguous",
                     info = sprintf("%s -> %s", months[i - 1], months[i]))
  }

  # a genuine two-month skip
  skip_case <- case_of(as.numeric(as.Date("2020-04-01")),
                       as.numeric(as.Date("2020-01-01")), period)
  expect_identical(skip_case, "gap")
})

# ---- .mm_irregular_steps ------------------------------------------------------

test_that(".mm_irregular_steps finds a deliberately removed period in a weekly series and none in a monthly one", {
  weekly <- seq(as.Date("2024-01-01"), by = "week", length.out = 20)
  weekly_period <- infer_period(weekly)
  expect_identical(irregular_steps(weekly, weekly_period), 0L)

  weekly_gapped <- weekly[-10]   # remove one week -> one irregular step
  expect_identical(irregular_steps(weekly_gapped, weekly_period), 1L)

  monthly <- seq(as.Date("2020-01-01"), by = "month", length.out = 24)
  monthly_period <- infer_period(monthly)
  expect_identical(irregular_steps(monthly, monthly_period), 0L)
})

# ---- .mm_infer_period, .mm_group_periods and .mm_gap_size --------------------

test_that(".mm_infer_period returns NA for fewer than two points and the median step otherwise", {
  expect_true(is.na(infer_period(5)))
  expect_true(is.na(infer_period(numeric(0))))
  expect_identical(infer_period(c(1, 3, 5, 100)), 2)  # median of {2,2,95}
})

test_that(".mm_group_periods infers the period within each group", {
  # two weekly series, on a Monday and on a Thursday: pooled, the dates
  # interleave and the median step is three or four days
  a <- as.numeric(seq(as.Date("2024-01-01"), by = "week", length.out = 10))
  b <- as.numeric(seq(as.Date("2024-01-04"), by = "week", length.out = 10))
  idx <- c(a, b)
  groups <- list(a = 1:10, b = 11:20)
  expect_lt(infer_period(idx), 7)
  expect_identical(group_periods(idx, groups), c(7, 7))

  # a group with one time point borrows the others' period
  expect_identical(group_periods(c(idx, 99), c(groups, list(c = 21L))),
                   c(7, 7, 7))
  # a period given by the user is used for every group
  expect_identical(group_periods(idx, groups, period = 14), c(14, 14))
  # nothing to infer from
  expect_true(all(is.na(group_periods(c(1, 5), list(a = 1L, b = 2L)))))
})

test_that(".mm_gap_size reports how many periods are missing", {
  expect_equal(gap_size(new_index = 20, last_index = 10, period = 1), 9)
  expect_equal(gap_size(new_index = 13, last_index = 10, period = 1), 2)
  # exactly one period ahead -> zero periods missing
  expect_equal(gap_size(new_index = 11, last_index = 10, period = 1), 0)
})

# ---- index checks (also live in contiguity.R) -------------------------------

test_that(".mm_check_duplicated_index errors only when the index has repeats", {
  check_dup <- mediamix:::.mm_check_duplicated_index
  expect_null(check_dup(1:5, NULL))
  expect_error(check_dup(c(1, 2, 2, 3), NULL), "repeated values")
  expect_error(check_dup(c(1, 2, 2, 3), "north"), "north")
})

test_that(".mm_check_index_finite names the groups with a missing time point", {
  check_finite <- mediamix:::.mm_check_index_finite
  groups <- list(north = 1:3, south = 4:6)
  expect_null(check_finite(c(1, 2, 3, 1, 2, 3), groups, TRUE, "date"))
  expect_error(check_finite(c(1, 2, 3, 1, NA, 3), groups, TRUE, "date"),
               "missing or infinite.*south")
  expect_error(check_finite(c(1, Inf, 3), list(.all = 1:3), FALSE, "date"),
               "missing or infinite")
})
