# Tests for the internal contiguity guard (R/contiguity.R):
# .mm_carry_case, .mm_infer_period, .mm_gap_size, .mm_irregular_steps.

carry_case <- mediamix:::.mm_carry_case
infer_period <- mediamix:::.mm_infer_period
gap_size <- mediamix:::.mm_gap_size
irregular_steps <- mediamix:::.mm_irregular_steps

# ---- all four cases, plain numeric index -----------------------------------

test_that(".mm_carry_case classifies all four cases on a plain numeric index", {
  idx <- 1:10
  period <- infer_period(idx)
  expect_identical(period, 1)

  expect_identical(carry_case(11, 10, period), "contiguous")
  expect_identical(carry_case(10, 10, period), "overlap")
  expect_identical(carry_case(5, 10, period), "overlap")
  expect_identical(carry_case(13, 10, period), "gap")
  expect_identical(carry_case(11, NULL, period), "unseen")
  expect_identical(carry_case(11, NA_real_, period), "unseen")
  expect_identical(carry_case(11, numeric(0), period), "unseen")
})

# ---- Date index -------------------------------------------------------------

test_that(".mm_carry_case classifies all four cases on a Date index", {
  d <- seq(as.Date("2024-01-01"), by = "day", length.out = 10)
  period <- infer_period(d)
  expect_identical(period, 1)

  last <- as.numeric(d[10])
  expect_identical(carry_case(as.numeric(d[10] + 1), last, period), "contiguous")
  expect_identical(carry_case(as.numeric(d[10]), last, period), "overlap")
  expect_identical(carry_case(as.numeric(d[3]), last, period), "overlap")
  expect_identical(carry_case(as.numeric(d[10] + 20), last, period), "gap")
  expect_identical(carry_case(as.numeric(d[10] + 1), NULL, period), "unseen")
})

# ---- POSIXct index -----------------------------------------------------------

test_that(".mm_carry_case classifies all four cases on a POSIXct index", {
  pt <- as.POSIXct("2024-01-01", tz = "UTC") + (0:9) * 86400
  period <- infer_period(pt)
  expect_identical(period, 86400)

  last <- as.numeric(pt[10])
  expect_identical(carry_case(as.numeric(pt[10] + 86400), last, period),
                    "contiguous")
  expect_identical(carry_case(as.numeric(pt[10]), last, period), "overlap")
  expect_identical(carry_case(as.numeric(pt[5]), last, period), "overlap")
  expect_identical(carry_case(as.numeric(pt[10] + 86400 * 10), last, period),
                    "gap")
  expect_identical(carry_case(as.numeric(pt[10] + 86400), NA_real_, period),
                    "unseen")
})

# ---- calendar months: every step contiguous, a 2-month skip is a gap -------

test_that("calendar-monthly steps classify as contiguous; a two-month skip is a gap", {
  months <- seq(as.Date("2020-01-01"), by = "month", length.out = 24)
  period <- infer_period(months)
  # months are 28-31 days; the loose 0.25 relative tolerance absorbs that
  expect_true(period >= 28 && period <= 31)

  for (i in 2:length(months)) {
    case <- carry_case(as.numeric(months[i]), as.numeric(months[i - 1]), period)
    expect_identical(case, "contiguous",
                      info = sprintf("%s -> %s", months[i - 1], months[i]))
  }

  # a genuine two-month skip
  skip_case <- carry_case(as.numeric(as.Date("2020-04-01")),
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

# ---- .mm_infer_period and .mm_gap_size, directly ----------------------------

test_that(".mm_infer_period returns NA for fewer than two points and the median step otherwise", {
  expect_true(is.na(infer_period(5)))
  expect_true(is.na(infer_period(numeric(0))))
  expect_identical(infer_period(c(1, 3, 5, 100)), 2)  # median of {2,2,95}
})

test_that(".mm_gap_size reports how many periods are missing", {
  expect_equal(gap_size(new_index = 20, last_index = 10, period = 1), 9)
  expect_equal(gap_size(new_index = 13, last_index = 10, period = 1), 2)
  # exactly one period ahead -> zero periods missing
  expect_equal(gap_size(new_index = 11, last_index = 10, period = 1), 0)
})

# ---- duplicated index check (also lives in contiguity.R) -------------------

test_that(".mm_check_duplicated_index errors only when the index has repeats", {
  check_dup <- mediamix:::.mm_check_duplicated_index
  expect_null(check_dup(1:5, NULL))
  expect_error(check_dup(c(1, 2, 2, 3), NULL), "repeated values")
  expect_error(check_dup(c(1, 2, 2, 3), "north"), "north")
})
