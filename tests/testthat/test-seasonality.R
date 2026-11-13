# Tests for fourier_terms() and holiday_flags() (R/seasonality.R).

# The terms of one harmonic at day of the year `d`, 1 for 1 January.
by_hand <- function(d, k = 1) {
  t <- (d - 1) / 365.25
  c(sin(2 * pi * k * t), cos(2 * pi * k * t))
}

# ---- fourier_terms(): values -------------------------------------------------

test_that("fourier_terms() matches hand-computed values for dates", {
  # 2 April 2023 is day 31 + 28 + 31 + 2 = 92 of the year.
  f <- fourier_terms(as.Date(c("2023-01-01", "2023-04-02")), harmonics = 2)
  expect_equal(unlist(f[1, ], use.names = FALSE), c(0, 1, 0, 1))
  expect_equal(unlist(f[2, c("season_sin1", "season_cos1")], use.names = FALSE),
               by_hand(92), tolerance = 1e-12)
  expect_equal(unlist(f[2, c("season_sin2", "season_cos2")], use.names = FALSE),
               by_hand(92, k = 2), tolerance = 1e-12)
})

test_that("fourier_terms() matches hand-computed values for a numeric index", {
  # Week 13 of a 52-week cycle is a quarter of the way round.
  f <- fourier_terms(c(0, 13, 26), harmonics = 2, cycle = 52)
  expect_equal(f$season_sin1, c(0, 1, 0), tolerance = 1e-12)
  expect_equal(f$season_cos1, c(1, 0, -1), tolerance = 1e-12)
  expect_equal(f$season_sin2, c(0, 0, 0), tolerance = 1e-12)
  expect_equal(f$season_cos2, c(1, -1, 1), tolerance = 1e-12)
  # A cycle need not be a whole number.
  g <- fourier_terms(1:3, harmonics = 1, cycle = 365.25 / 7)
  expect_equal(g$season_sin1, sin(2 * pi * (1:3) * 7 / 365.25),
               tolerance = 1e-12)
})

test_that("every year starts again at phase zero", {
  f <- fourier_terms(as.Date(c("2023-01-01", "2024-01-01", "2031-01-01")),
                     harmonics = 3)
  expect_equal(f[2, ], f[1, ], ignore_attr = TRUE)
  expect_equal(f[3, ], f[1, ], ignore_attr = TRUE)
})

test_that("leap years run one day ahead from 29 February", {
  d <- as.Date(c("2023-02-10", "2024-02-10",
                 "2023-03-01", "2024-02-29", "2024-03-01",
                 "2023-12-31", "2024-12-31"))
  f <- fourier_terms(d, harmonics = 1)
  # Dates before 29 February match across years.
  expect_equal(f[2, ], f[1, ], ignore_attr = TRUE)
  # 1 March 2023 and 29 February 2024 are both day 60.
  expect_equal(f[4, ], f[3, ], ignore_attr = TRUE)
  expect_equal(unlist(f[5, ], use.names = FALSE), by_hand(61),
               tolerance = 1e-12)
  # 31 December is day 365, or 366 in a leap year.
  expect_equal(unlist(f[6, ], use.names = FALSE), by_hand(365),
               tolerance = 1e-12)
  expect_equal(unlist(f[7, ], use.names = FALSE), by_hand(366),
               tolerance = 1e-12)
})

test_that("daily dates step by 1 / 365.25 and restart at the new year", {
  d <- seq(as.Date("2023-12-30"), as.Date("2024-01-02"), by = "day")
  f <- fourier_terms(d, harmonics = 1)
  expect_equal(f$season_sin1, sin(2 * pi * c(363, 364, 0, 1) / 365.25),
               tolerance = 1e-12)
  expect_equal(f$season_cos1, cos(2 * pi * c(363, 364, 0, 1) / 365.25),
               tolerance = 1e-12)
})

test_that("weekly panel data gives the same terms for the same date", {
  data(mm_weekly, envir = environment())
  f <- fourier_terms(mm_weekly$date, harmonics = 2)
  expect_identical(dim(f), c(nrow(mm_weekly), 4L))
  north <- mm_weekly$geo == "north"
  south <- mm_weekly$geo == "south"
  expect_equal(f[north, ], f[south, ], ignore_attr = TRUE)
  # 2 January 2023 is day 2.
  expect_equal(unlist(f[1, 1:2], use.names = FALSE), by_hand(2),
               tolerance = 1e-12)
})

# ---- fourier_terms(): shape of the result ------------------------------------

test_that("fourier_terms() names its columns by harmonic, sine first", {
  d <- as.Date("2024-06-01") + 0:3
  expect_named(fourier_terms(d, harmonics = 3),
               c("season_sin1", "season_cos1", "season_sin2", "season_cos2",
                 "season_sin3", "season_cos3"))
  expect_named(fourier_terms(d, harmonics = 1, prefix = "yearly"),
               c("yearly_sin1", "yearly_cos1"))
  expect_named(fourier_terms(1:4, harmonics = 1, cycle = 7, prefix = "dow"),
               c("dow_sin1", "dow_cos1"))
})

test_that("fourier_terms() returns one row per input row, ready to bind", {
  d <- as.Date("2024-01-01") + 7 * 0:9
  f <- fourier_terms(d)
  expect_s3_class(f, "data.frame")
  expect_identical(nrow(f), 10L)
  expect_identical(rownames(f), as.character(1:10))
  frame <- data.frame(date = d, sales = 1:10)
  bound <- cbind(frame, f)
  expect_named(bound, c("date", "sales", names(f)))
  expect_identical(nrow(bound), 10L)
})

test_that("fourier_terms() keeps the input order of unsorted dates", {
  d <- as.Date("2023-01-02") + 7 * 0:51
  set.seed(1)
  shuffled <- sample(d)
  expect_equal(fourier_terms(shuffled), fourier_terms(d)[match(shuffled, d), ],
               ignore_attr = TRUE)
})

test_that("a missing date gives a row of NA and leaves the others alone", {
  d <- as.Date(c("2024-01-01", NA, "2024-04-02"))
  f <- fourier_terms(d, harmonics = 2)
  expect_true(all(is.na(f[2, ])))
  expect_equal(f[c(1, 3), ],
               fourier_terms(d[c(1, 3)], harmonics = 2), ignore_attr = TRUE)
  g <- fourier_terms(c(1, NA, 3), harmonics = 1, cycle = 52)
  expect_true(all(is.na(g[2, ])))
})

# ---- fourier_terms(): invalid input ------------------------------------------

test_that("fourier_terms() checks harmonics and prefix", {
  d <- as.Date("2024-01-01") + 0:3
  expect_error(fourier_terms(d, harmonics = 0), "at least 1")
  expect_error(fourier_terms(d, harmonics = 1.5), "whole number")
  expect_error(fourier_terms(d, harmonics = NA), "harmonics")
  expect_error(fourier_terms(d, harmonics = c(1, 2)), "harmonics")
  expect_error(fourier_terms(d, harmonics = "2"), "harmonics")
  expect_error(fourier_terms(d, prefix = ""), "prefix")
  expect_error(fourier_terms(d, prefix = NA_character_), "prefix")
  expect_error(fourier_terms(d, prefix = c("a", "b")), "prefix")
  expect_error(fourier_terms(d, prefix = 1), "prefix")
})

test_that("fourier_terms() needs a cycle for a number and none for a date", {
  expect_error(fourier_terms(1:10), "needs `cycle`")
  expect_error(fourier_terms(as.Date("2024-01-01"), cycle = 52),
               "calendar year")
  expect_error(fourier_terms(1:10, cycle = 0), "`cycle`")
  expect_error(fourier_terms(1:10, cycle = -52), "`cycle`")
  expect_error(fourier_terms(1:10, cycle = Inf), "`cycle`")
  expect_error(fourier_terms(1:10, cycle = c(52, 12)), "`cycle`")
})

test_that("fourier_terms() refuses other kinds of index", {
  expect_error(fourier_terms(as.POSIXct("2024-01-01", tz = "UTC")),
               "as.Date")
  expect_error(fourier_terms("2024-01-01"), "numeric vector")
  expect_error(fourier_terms(as.Date(character(0))), "at least one element")
  expect_error(fourier_terms(numeric(0), cycle = 52), "at least one element")
  expect_error(fourier_terms(c(1, Inf), cycle = 52), "infinite")
  expect_error(fourier_terms(matrix(1:4, 2), cycle = 52), "numeric vector")
})

# ---- holiday_flags(): period boundaries --------------------------------------

test_that("week-commencing dates flag the week a holiday falls in", {
  mondays <- as.Date(c("2024-12-16", "2024-12-23", "2024-12-30",
                       "2025-01-06"))
  flag <- function(h) holiday_flags(mondays, as.Date(h))$holiday
  # Christmas Day and New Year's Day 2025 are both Wednesdays.
  expect_identical(flag(c("2024-12-25", "2025-01-01")), c(0L, 1L, 1L, 0L))
  # The first and last days of a week are inside it.
  expect_identical(flag("2024-12-23"), c(0L, 1L, 0L, 0L))
  expect_identical(flag("2024-12-29"), c(0L, 1L, 0L, 0L))
  expect_identical(flag("2024-12-30"), c(0L, 0L, 1L, 0L))
  # The last week runs to 12 January.
  expect_identical(flag("2025-01-12"), c(0L, 0L, 0L, 1L))
  expect_identical(flag("2025-01-13"), c(0L, 0L, 0L, 0L))
})

test_that("week-ending dates flag the week that ends on or after a holiday", {
  sundays <- as.Date(c("2024-12-22", "2024-12-29", "2025-01-05"))
  flag <- function(h) {
    holiday_flags(sundays, as.Date(h), align = "end")$holiday
  }
  expect_identical(flag("2024-12-25"), c(0L, 1L, 0L))
  expect_identical(flag("2024-12-22"), c(1L, 0L, 0L))
  expect_identical(flag("2024-12-23"), c(0L, 1L, 0L))
  expect_identical(flag("2024-12-29"), c(0L, 1L, 0L))
  # The first week runs from 16 December.
  expect_identical(flag("2024-12-16"), c(1L, 0L, 0L))
  expect_identical(flag("2024-12-15"), c(0L, 0L, 0L))
  # The same weeks labelled by their Mondays give the same flags.
  expect_identical(
    holiday_flags(sundays - 6, as.Date(c("2024-12-25", "2024-12-16"))),
    holiday_flags(sundays, as.Date(c("2024-12-25", "2024-12-16")),
                  align = "end")
  )
})

test_that("daily dates are flagged on the holiday itself", {
  days <- seq(as.Date("2024-12-20"), as.Date("2024-12-31"), by = "day")
  h <- as.Date(c("2024-12-25", "2024-12-26"))
  start <- holiday_flags(days, h)$holiday
  # From R 4.5, seq() can return Dates stored as integers, so compare values.
  expect_equal(days[start == 1L], h)
  expect_identical(holiday_flags(days, h, align = "end")$holiday, start)
})

test_that("Dates stored as integers give the same terms and flags", {
  dbl <- as.Date("2024-12-20") + 0:11
  int <- structure(as.integer(unclass(dbl)), class = "Date")
  h <- as.Date(c("2024-12-25", "2024-12-26"))
  expect_identical(typeof(unclass(int)), "integer")
  expect_equal(fourier_terms(int), fourier_terms(dbl))
  expect_identical(holiday_flags(int, h), holiday_flags(dbl, h))
  expect_identical(holiday_flags(dbl, structure(as.integer(unclass(h)),
                                                class = "Date")),
                   holiday_flags(dbl, h))
})

test_that("29 February is flagged in daily and weekly leap-year data", {
  days <- seq(as.Date("2024-02-27"), as.Date("2024-03-02"), by = "day")
  leap_day <- as.Date("2024-02-29")
  expect_identical(holiday_flags(days, leap_day)$holiday,
                   c(0L, 0L, 1L, 0L, 0L))
  # The week of Monday 26 February 2024 runs to Sunday 3 March.
  mondays <- as.Date(c("2024-02-19", "2024-02-26", "2024-03-04"))
  expect_identical(holiday_flags(mondays, leap_day)$holiday, c(0L, 1L, 0L))
})

test_that("period is inferred as the usual spacing, or given", {
  mondays <- as.Date("2024-01-01") + 7 * c(0, 1, 2, 4, 5)
  # Week four is missing: its holiday is flagged nowhere, and the gap does
  # not change the inferred period of seven days.
  missing_week <- as.Date("2024-01-24")
  expect_identical(holiday_flags(mondays, missing_week)$holiday,
                   rep(0L, 5))
  expect_identical(holiday_flags(mondays, missing_week, period = 7),
                   holiday_flags(mondays, missing_week))
  # A difftime works as well as a number of days.
  h <- as.Date("2024-01-10")
  expect_identical(
    holiday_flags(mondays, h, period = as.difftime(1, units = "weeks")),
    holiday_flags(mondays, h, period = 7)
  )
  # A shorter period flags fewer rows.
  expect_identical(holiday_flags(mondays, h, period = 2)$holiday,
                   rep(0L, 5))
  # A single date needs period.
  expect_error(holiday_flags(as.Date("2024-01-01"), h), "cannot be inferred")
  expect_identical(holiday_flags(as.Date("2024-01-08"), h, period = 7)$holiday,
                   1L)
})

# ---- holiday_flags(): shape of the result ------------------------------------

test_that("a named list gives one integer flag column per holiday", {
  mondays <- as.Date("2024-11-18") + 7 * 0:6
  holidays <- list(
    christmas = as.Date(c("2023-12-25", "2024-12-25")),
    black_friday = as.Date("2024-11-29")
  )
  flags <- holiday_flags(mondays, holidays)
  expect_s3_class(flags, "data.frame")
  expect_named(flags, c("holiday_christmas", "holiday_black_friday"))
  expect_type(flags$holiday_christmas, "integer")
  expect_identical(flags$holiday_christmas, c(0L, 0L, 0L, 0L, 0L, 1L, 0L))
  expect_identical(flags$holiday_black_friday, c(0L, 1L, 0L, 0L, 0L, 0L, 0L))
  expect_named(holiday_flags(mondays, holidays, prefix = "h"),
               c("h_christmas", "h_black_friday"))
  expect_named(holiday_flags(mondays, holidays$christmas), "holiday")
  expect_named(holiday_flags(mondays, holidays$christmas, prefix = "xmas"),
               "xmas")
})

test_that("holiday_flags() returns one row per input row, ready to bind", {
  data(mm_weekly, envir = environment())
  xmas <- as.Date(c("2023-12-25", "2024-12-25", "2025-12-25"))
  flags <- holiday_flags(mm_weekly$date, list(christmas = xmas))
  expect_identical(nrow(flags), nrow(mm_weekly))
  expect_identical(rownames(flags), as.character(seq_len(nrow(mm_weekly))))
  # Three geographies share the dates, so each Christmas week appears three
  # times.
  expect_identical(sum(flags$holiday_christmas), 9L)
  bound <- cbind(mm_weekly, flags)
  expect_identical(bound$holiday_christmas, flags$holiday_christmas)
})

test_that("holiday_flags() keeps the order of unsorted and repeated dates", {
  mondays <- as.Date("2024-11-04") + 7 * 0:9
  h <- as.Date(c("2024-12-25", "2024-11-11"))
  set.seed(2)
  shuffled <- sample(rep(mondays, 2))
  expect_identical(holiday_flags(shuffled, h)$holiday,
                   holiday_flags(mondays, h)$holiday[match(shuffled, mondays)])
})

test_that("holiday dates may repeat, be unsorted, or fall outside the series", {
  mondays <- as.Date("2024-12-02") + 7 * 0:4
  tidy <- holiday_flags(mondays, as.Date("2024-12-25"))
  messy <- holiday_flags(mondays, as.Date(c("2024-12-25", "2019-01-01",
                                            "2024-12-25", "2030-06-01")))
  expect_identical(messy, tidy)
  expect_identical(holiday_flags(mondays, as.Date(character(0)))$holiday,
                   rep(0L, 5))
})

test_that("a missing date gives a missing flag and does not set the period", {
  d <- as.Date(c("2024-12-16", NA, "2024-12-23", "2024-12-30"))
  flags <- holiday_flags(d, as.Date("2024-12-25"))
  expect_identical(flags$holiday, c(0L, NA, 1L, 0L))
  ending <- holiday_flags(d, as.Date("2024-12-25"), align = "end")
  expect_identical(ending$holiday, c(0L, NA, 0L, 1L))
})

test_that("overlapping inferred periods warn, and a given period does not", {
  # Months are 28 to 31 days long, so a 31-day period overlaps the next month.
  months <- seq(as.Date("2024-01-01"), by = "month", length.out = 12)
  expect_warning(flags <- holiday_flags(months, as.Date("2024-03-01")),
                 "closer together than the inferred period")
  expect_identical(sum(flags$holiday), 2L)
  # A panel whose series start their weeks on different days needs period.
  mondays <- as.Date("2024-01-01") + 7 * 0:3
  sundays <- mondays + 6
  panel <- c(mondays, sundays)
  expect_warning(holiday_flags(panel, as.Date("2024-01-10")), "overlap")
  expect_no_warning(
    flags <- holiday_flags(panel, as.Date("2024-01-10"), period = 7)
  )
  expect_identical(flags$holiday, c(0L, 1L, 0L, 0L, 1L, 0L, 0L, 0L))
  # Evenly spaced weekly and daily dates do not warn.
  expect_no_warning(holiday_flags(mondays, as.Date("2024-01-10")))
  expect_no_warning(holiday_flags(as.Date("2024-01-01") + 0:9,
                                  as.Date("2024-01-05")))
})

# ---- holiday_flags(): invalid input ------------------------------------------

test_that("holiday_flags() needs a Date index", {
  h <- as.Date("2024-12-25")
  expect_error(holiday_flags("2024-12-23", h), "<Date>")
  expect_error(holiday_flags(as.POSIXct("2024-12-23", tz = "UTC"), h),
               "as.Date")
  expect_error(holiday_flags(1:10, h), "<Date>")
  expect_error(holiday_flags(as.Date(character(0)), h), "at least one")
})

test_that("holiday_flags() checks the holidays", {
  d <- as.Date("2024-12-16") + 7 * 0:3
  expect_error(holiday_flags(d, "2024-12-25"), "as.Date")
  expect_error(holiday_flags(d, as.Date(c("2024-12-25", NA))),
               "1 missing date")
  expect_error(holiday_flags(d, list(xmas = as.Date(c("2024-12-25", NA)))),
               "in element \"xmas\"")
  expect_error(holiday_flags(d, list(as.Date("2024-12-25"))), "named")
  expect_error(holiday_flags(d, list(a = as.Date("2024-12-25"),
                                     as.Date("2024-12-26"))),
               "named")
  expect_error(holiday_flags(d, list(a = as.Date("2024-12-25"),
                                     a = as.Date("2024-12-26"))),
               "repeats")
  expect_error(holiday_flags(d, list(a = as.Date("2024-12-25"),
                                     b = "2024-12-26")),
               "Element \"b\"")
  expect_error(holiday_flags(d, list()), "at least one")
  table <- data.frame(date = as.Date("2024-12-25"), holiday = "christmas")
  expect_error(holiday_flags(d, table), "split")
})

test_that("holiday_flags() checks period, align and prefix", {
  d <- as.Date("2024-12-16") + 7 * 0:3
  h <- as.Date("2024-12-25")
  expect_error(holiday_flags(d, h, period = 0), "`period`")
  expect_error(holiday_flags(d, h, period = -7), "`period`")
  expect_error(holiday_flags(d, h, period = "7"), "`period`")
  expect_error(holiday_flags(d, h, period = c(7, 7)), "`period`")
  expect_error(holiday_flags(d, h, period = NA_real_), "`period`")
  expect_error(holiday_flags(d, h, align = "middle"), "should be one of")
  expect_error(holiday_flags(d, h, prefix = ""), "prefix")
})

test_that("a Date with a fractional part is flagged on the day it prints as", {
  d <- seq(as.Date("2023-12-04"), by = 7, length.out = 5)
  h <- as.Date("2023-12-25")
  # A Date class on a fractional day number; as.Date() of a number needs an
  # origin before R 4.3.
  frac <- function(x, by) structure(as.numeric(x) + by, class = "Date")
  expect_identical(holiday_flags(frac(d, 0.5), h), holiday_flags(d, h))
  expect_identical(holiday_flags(d, frac(h, 0.75)), holiday_flags(d, h))
})
