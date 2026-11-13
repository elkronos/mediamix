# Seasonality controls for a model frame

Calendar regressors for a marketing mix model: Fourier terms for a
smooth yearly cycle, and flags for the periods that contain a holiday.
Both return a data frame with one row per element of `index`, in the
same order, ready to [`cbind()`](https://rdrr.io/r/base/cbind.html) to a
model frame or to pass as `controls` to
[`tune_carryover()`](https://elkronos.github.io/mediamix/reference/tune_carryover.md).

## Usage

``` r
fourier_terms(index, harmonics = 2, cycle = NULL, prefix = "season")

holiday_flags(
  index,
  holidays,
  period = NULL,
  align = c("start", "end"),
  prefix = "holiday"
)
```

## Arguments

- index:

  The time of each row. For `fourier_terms()`, a `Date` vector, or a
  numeric vector with `cycle` given. For `holiday_flags()`, a `Date`
  vector. It need not be sorted and may repeat dates, as a panel does. A
  missing value gives a row of `NA`.

- harmonics:

  Number of sine and cosine pairs, a whole number of at least 1.
  Harmonic `k` repeats `k` times per cycle, so `k` times a year for a
  `Date` index.

- cycle:

  Length of one seasonal cycle in the units of a numeric `index`, such
  as `52` for a week number or `12` for a month number. Required for a
  numeric `index`. A `Date` index always uses the calendar year, so
  `cycle` must then be left out.

- prefix:

  Start of every column name, a single string.

- holidays:

  The holiday dates: a `Date` vector for a single flag, or a named list
  of `Date` vectors for one flag per element. Dates may repeat and need
  not be sorted, but must not be missing.

- period:

  Length of each row's period: a single positive number of days, or a
  `difftime`. `NULL` (the default) takes the median step between
  consecutive distinct dates in `index`, which is 7 for weekly data and
  1 for daily data.

- align:

  Where each date sits in its period: `"start"` (the default) when dates
  mark the first day of the period, as week-commencing dates do, or
  `"end"` when they mark the last day, as week-ending dates do.

## Value

A data frame with one row per element of `index`, in the same order, and
default row names.

`fourier_terms()` returns `2 * harmonics` numeric columns, a sine and a
cosine for each harmonic in turn: `<prefix>_sin1`, `<prefix>_cos1`,
`<prefix>_sin2`, `<prefix>_cos2` and so on.

`holiday_flags()` returns integer columns holding 1 for a period that
contains a holiday and 0 otherwise. A `Date` vector of holidays gives
one column named `prefix`; a named list gives one column per element,
named `<prefix>_<element name>`, in the order of the list.

## Details

**Fourier terms.** For a `Date`, the time of year is
`t = (d - 1) / 365.25`, where `d` is the day of the year, 1 for 1
January. For a numeric index, `t = index / cycle`. Harmonic `k`
contributes `sin(2 * pi * k * t)` and `cos(2 * pi * k * t)`. Every year
starts at `t = 0`, so a calendar date has the same terms every year,
except that from 29 February a leap year is one day further along: its
31 December reaches `t = 365 / 365.25`. Each harmonic adds two columns,
and so two coefficients to the model; the first few describe a smooth
seasonal shape, and later ones sharper peaks.

**Holiday flags.** Each date in `index` marks one end of a period of
`period` days. With `align = "start"` the period runs from the date up
to, but not including, the date plus `period`; with `align = "end"` it
runs from the day after the date minus `period` up to and including the
date. A row is flagged when at least one holiday falls inside its
period, so with daily data a row is flagged on the holiday itself. A
holiday that falls in no row's period, such as one in a missing week, is
not flagged anywhere.

Every period has the same length, which suits daily and weekly data.
When `period` is inferred and some dates are closer together than it,
the periods overlap, a holiday can be flagged in two rows, and a warning
says so. That happens with monthly data, whose months run from 28 to 31
days, and with a panel whose series start their weeks on different days.
Give `period` for the panel, and flag monthly data by each holiday's
month.

## See also

[`tune_carryover()`](https://elkronos.github.io/mediamix/reference/tune_carryover.md)
and
[`tune_carryover_joint()`](https://elkronos.github.io/mediamix/reference/tune_carryover_joint.md),
which take control columns as `controls`;
[mm_weekly](https://elkronos.github.io/mediamix/reference/mm_weekly.md),
whose `seasonality` and `holiday` columns are simulated stand-ins for
these controls.

## Examples

``` r
data(mm_weekly)
north <- mm_weekly[mm_weekly$geo == "north", ]

# Two harmonics of the yearly cycle, from week-commencing dates
season <- fourier_terms(north$date, harmonics = 2)
head(round(season, 3))
#>   season_sin1 season_cos1 season_sin2 season_cos2
#> 1       0.017       1.000       0.034       0.999
#> 2       0.137       0.991       0.272       0.962
#> 3       0.255       0.967       0.493       0.870
#> 4       0.369       0.929       0.687       0.727
#> 5       0.478       0.878       0.840       0.542
#> 6       0.580       0.814       0.945       0.326

# A numeric index needs the length of its cycle
head(round(fourier_terms(1:104, harmonics = 1, cycle = 52), 3))
#>   season_sin1 season_cos1
#> 1       0.121       0.993
#> 2       0.239       0.971
#> 3       0.355       0.935
#> 4       0.465       0.885
#> 5       0.568       0.823
#> 6       0.663       0.749

# Flag the weeks that contain Christmas Day or Black Friday
holidays <- list(
  christmas = as.Date(c("2023-12-25", "2024-12-25", "2025-12-25")),
  black_friday = as.Date(c("2023-11-24", "2024-11-29", "2025-11-28"))
)
flags <- holiday_flags(north$date, holidays)
north$date[flags$holiday_christmas == 1]
#> [1] "2023-12-25" "2024-12-23" "2025-12-22"

# Week-ending dates flag the same weeks with align = "end"
week_ending <- north$date + 6
identical(holiday_flags(week_ending, holidays, align = "end"), flags)
#> [1] TRUE

# A table with one row per holiday becomes the list through split()
calendar <- data.frame(date = as.Date(c("2024-12-25", "2024-11-29")),
                       name = c("christmas", "black_friday"))
flags_2024 <- holiday_flags(north$date, split(calendar$date, calendar$name))
colSums(flags_2024)
#> holiday_black_friday    holiday_christmas 
#>                    1                    1 

# Ready to bind to a model frame
model_data <- cbind(north[c("date", "revenue")], season, flags)
head(model_data)
#>         date revenue season_sin1 season_cos1 season_sin2 season_cos2
#> 1 2023-01-02   27130  0.01720158   0.9998520  0.03439806   0.9994082
#> 2 2023-01-09   20433  0.13718540   0.9905454  0.27177674   0.9623603
#> 3 2023-01-16   20481  0.25518241   0.9668929  0.49346814   0.8697639
#> 4 2023-01-23   20565  0.36948368   0.9292372  0.68667598   0.7269636
#> 5 2023-01-30   17574  0.47843382   0.8781236  0.84024807   0.5422022
#> 6 2023-02-06   28722  0.58045493   0.8142924  0.94532005   0.3261442
#>   holiday_christmas holiday_black_friday
#> 1                 0                    0
#> 2                 0                    0
#> 3                 0                    0
#> 4                 0                    0
#> 5                 0                    0
#> 6                 0                    0
```
