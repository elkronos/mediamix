# Adstock transformation as a recipe step

Applies adstock to one or more columns inside a recipes pipeline,
carrying the filter's state across the train/test boundary instead of
emitting missing values there. The kernel is geometric by default; the
Weibull and delayed kernels can put the peak effect after the period of
spend.

## Usage

``` r
step_adstock(
  recipe,
  ...,
  index = NULL,
  by = NULL,
  decay = 0.5,
  max_lag = Inf,
  normalise = TRUE,
  carry_over = c("auto", "warm", "cold"),
  na_action = c("zero", "error"),
  tol = 0.25,
  period = NULL,
  kernel = c("geometric", "weibull", "delayed"),
  shape = NULL,
  scale = NULL,
  type = c("cdf", "pdf"),
  peak = 0,
  start = c("zero", "steady"),
  keep_original_cols = FALSE,
  prefix = "adstock_",
  role = "predictor",
  trained = FALSE,
  columns = NULL,
  states = NULL,
  last_index = NULL,
  history = NULL,
  seeds = NULL,
  skip = FALSE,
  id = .mm_rand_id("adstock")
)
```

## Arguments

- recipe:

  A recipe object.

- ...:

  One or more selector functions choosing the media columns. See
  "Selecting media columns" below.

- index:

  Name of the time column, as a string. Required: adstock is a
  time-ordered filter, so the step needs to know the order and the
  spacing. The column is used for ordering and validation and is not
  itself transformed. It must be numeric, `Date` or `POSIXct`, with no
  missing values.

- by:

  Optional character vector naming grouping columns. Adstock is applied
  independently within each group, which is what geo-level and panel
  models need. This is an explicit argument because recipes has no group
  awareness and a grouped data frame does not reliably survive into
  [`bake()`](https://recipes.tidymodels.org/reference/bake.html).
  Grouping columns must not contain missing values.

- decay:

  Geometric decay coefficient in `[0, 1]`, or for `kernel = "delayed"`
  the retention rate, in `(0, 1)`. Ignored by the Weibull kernel.
  Tunable.

- max_lag:

  Kernel length; `Inf` (the default) uses the infinite recursive form of
  the geometric kernel. The Weibull and delayed kernels need a finite
  value. Tunable.

- normalise:

  Should the kernel sum to 1? See
  [`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md).

- carry_over:

  How to treat state at the start of new data. `"auto"` (the default)
  continues the filter when the new data continues or repeats the
  training series, and otherwise cold-starts with a warning. `"warm"`
  errors instead of cold-starting: on a gap, on a start that does not
  line up with the training periods, on an unseen group and when the
  period is unknown. `"cold"` always restarts from zero, which
  reproduces the behaviour of a lag-based step; under `start = "steady"`
  it restarts from the steady state of the new data's first periods.

- na_action:

  What to do about missing media values. `"zero"` (the default) treats
  missing media as no media, which is usually right for spend and is
  what lets a recipe run over a real, patchy panel without stopping. It
  is still a substantive assumption (an `NA` becomes a real number in
  the output), so `"error"` is available when you would rather find out.
  This differs from
  [`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md),
  whose default is `"error"`; a step has to survive resampling, and a
  bare function call does not. Infinite values are an error under either
  setting.

- tol:

  Relative tolerance on the step between periods when deciding whether
  two blocks are contiguous, in `(0, 0.5)`. The default of `0.25` is
  loose so that calendar months, which vary from 28 to 31 days, are not
  mistaken for gaps. It stays below one half so that a block one period
  later, two periods later and part-way through a period remain
  distinct.

- period:

  The spacing between consecutive periods: a single positive number in
  the units of `index` (days for a `Date`, seconds for a `POSIXct`), or
  a `difftime`. `NULL` (the default) infers it within each group as the
  median step of that group's training index, so geographies that report
  on different weekdays each keep their own grid. A group with a single
  training time point takes the median of the other groups' periods.

- kernel:

  The carryover kernel: `"geometric"` (the default), `"weibull"` or
  `"delayed"`, applied by
  [`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md),
  [`adstock_weibull()`](https://elkronos.github.io/mediamix/reference/adstock_weibull.md)
  or
  [`adstock_delayed()`](https://elkronos.github.io/mediamix/reference/adstock_delayed.md).
  See "Kernels" below. An argument the kernel ignores must still be a
  valid value of its kind, such as a `decay` in `[0, 1]` or a
  non-negative `peak`, so that a typo is caught rather than kept in the
  step.

- shape, scale:

  Weibull shape and scale, both positive; `scale` is in periods.
  Required when `kernel = "weibull"` and ignored otherwise. Tunable.

- type:

  The form of the Weibull kernel, `"cdf"` (the default) or `"pdf"`. See
  [`adstock_weights_weibull()`](https://elkronos.github.io/mediamix/reference/adstock_weights_weibull.md).
  Ignored by the other kernels.

- peak:

  Lag of the peak effect when `kernel = "delayed"`, in periods, in
  `[0, max_lag - 1]`. Ignored by the other kernels. Tunable.

- start:

  How each series starts. `"zero"` (the default) assumes no media ran
  before its first row. `"steady"` starts it at the steady state of its
  own first periods, as
  [`adstock_steady_state()`](https://elkronos.github.io/mediamix/reference/adstock_steady_state.md)
  computes it, so a long carryover has no burn-in. See "Steady-state
  starts" below.

- keep_original_cols:

  Should the selected columns be kept unchanged? `FALSE` (the default)
  replaces each one with its adstocked values. `TRUE` writes the
  adstocked values to a new column named with `prefix` and keeps the raw
  spend, which ROI calculations need after the model.

- prefix:

  The start of each new column's name when `keep_original_cols = TRUE`:
  the default `"adstock_"` turns `tv` into `adstock_tv`. Unused
  otherwise.

- role:

  The role of the new columns made with `keep_original_cols = TRUE`,
  `"predictor"` by default. Unused when the columns are transformed in
  place.

- trained:

  Has the step been prepared?

- columns, states, last_index, history, seeds:

  Populated by
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html); not
  set directly.

- skip:

  Should the step be skipped when baking? Must remain `FALSE`: adstock
  is a predictor transform and has to run at prediction time.

- id:

  A unique step identifier.

## Value

An updated recipe with the new step added.

## The problem this solves

Adstock is a filter with memory. Prepare a recipe on January to June and
bake it on July to December, and the first rows of July have no history
to draw on. A lag built with
[`recipes::step_lag()`](https://recipes.tidymodels.org/reference/step_lag.html)
leaves `NA`, or the filler given as its `default`, in the first rows of
new data at every prep/bake boundary, and dropping those rows with
`step_naomit(skip = FALSE)` removes the boundary weeks from training and
evaluation (with its default, `skip = TRUE`,
[`step_naomit()`](https://recipes.tidymodels.org/reference/step_naomit.html)
drops them from the training data only). `step_adstock()` carries the
filter's state across the boundary instead, so no row is dropped or
filled in.

Geometric adstock is an infinite impulse response filter whose memory is
one number per series, \$\$a_t = x_t + \theta a\_{t-1},\$\$ so the
filter can be continued from where the training data left off.
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) stores,
for each group, the training time points and the training values of each
selected column.
[`bake()`](https://recipes.tidymodels.org/reference/bake.html) runs the
filter over the training rows that come before the new data and then
over the new rows, so the new rows get exactly the values a filter over
the whole series gives them. That holds whether the new data follows the
training data, repeats its last few periods or repeats all of it.

## Storage

The stored values take as much space as the selected columns of the
training data, plus one time index per group: about 25 KB for the three
geographies, 156 weeks and five channels of `mm_weekly`. They are media
values only; no outcome is stored. A recipe prepared with
`retain = TRUE`, the default of
[`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html),
already keeps the whole training set. Selecting only the media columns
keeps the stored part small.

## What bake() does at the boundary

[`prep()`](https://recipes.tidymodels.org/reference/prep.html) records,
per group, the training time points and values, and the period: the
median step of that group's index unless `period` is given.
[`bake()`](https://recipes.tidymodels.org/reference/bake.html) then
takes, for each group, the training time points strictly before the
first new row and compares the first new row with the last of them:

- About one period later, or on a training time point:

  The new data continues or repeats the series. The filter runs over the
  earlier training rows and then the new rows, and the result is
  identical to filtering the whole series at once. A block that starts
  at or before the first training time point has nothing before it, so
  it starts as the training series did (from zero, or from its steady
  state under `start = "steady"`), which is why
  `bake(rec, new_data = training)` reproduces the prepared output. A
  block that repeats the last periods of training, as
  `rsample::rolling_origin(lag = )` builds, continues from the rows
  before it.

- More than one period later:

  A gap. Cold-start, and warn, naming the gap size and the group.

- Less than one period later:

  A misaligned start, as when daily rows are baked into a weekly step or
  the reporting day moves. Cold-start, and warn.

- An unseen group:

  Cold-start, and warn.

Under `carry_over = "warm"` each of these warnings is an error instead.
A cold start begins the block from zero, or under `start = "steady"`
from the steady state of the block's own first periods. Row count and
row order are preserved in every case. The step sorts by `index`
internally and restores the original order before returning, so nothing
downstream sees a reordered table.

## Kernels

The geometric kernel is the only one with a recursive form, and the only
one that accepts `max_lag = Inf`. The Weibull and delayed kernels are
finite, and either can put the peak effect one or more periods after the
spend; see
[`adstock_weibull()`](https://elkronos.github.io/mediamix/reference/adstock_weibull.md)
and
[`adstock_delayed()`](https://elkronos.github.io/mediamix/reference/adstock_delayed.md).
A finite kernel reaches back `max_lag - 1` periods, so
[`bake()`](https://recipes.tidymodels.org/reference/bake.html) puts at
most that many training rows in front of the new data, and the boundary
cases above work the same way for every kernel.

Each kernel uses its own arguments and ignores the others, and only its
own can be marked with `tune()`:

- `"geometric"`:

  `decay` and `max_lag`, tuned over
  [`carryover_decay()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md)
  and
  [`carryover_max_lag()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md).

- `"weibull"`:

  `shape`, `scale` and `max_lag`, tuned over
  [`carryover_shape()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md),
  [`carryover_scale()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md)
  and
  [`carryover_max_lag()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md).

- `"delayed"`:

  `decay`, `peak` and `max_lag`. Its `decay` is tuned over
  `[0.05, 0.95]`, since the kernel needs a positive retention rate.
  [`carryover_peak()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md)
  stops at `max_lag - 1` when `max_lag` is fixed, and a tuned `max_lag`
  starts above the largest peak the search can propose, so every
  candidate is a valid kernel.

[`step_saturation()`](https://elkronos.github.io/mediamix/reference/step_saturation.md)
also has a `shape` argument. When both steps tune one, give them
distinct ids, for example `shape = tune("adstock_shape")`.

## Steady-state starts

A filter started from zero understates the first periods of a
long-carryover channel. With `start = "steady"`,
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) stores
for each group and column the steady state of the training series: the
state the filter would hold had the mean spend of the series' first
periods been running forever, from
[`adstock_steady_state()`](https://elkronos.github.io/mediamix/reference/adstock_steady_state.md).
The mean covers the geometric kernel's 90% effective window, capped at
`max_lag - 1` periods for a finite `max_lag`; the Weibull and delayed
kernels average the first `max_lag - 1` periods. It uses training media
only, never the outcome.

[`bake()`](https://recipes.tidymodels.org/reference/bake.html) starts
the filter from that state wherever the filter starts at the beginning
of a training series: when it re-bakes the training data, and when a
block repeats or continues the series. The new rows then get exactly the
values that a filter over the whole series, started from the same state,
gives them. A cold-started block (after a gap, misaligned, from an
unseen group, with an unknown period, or under `carry_over = "cold"`) is
treated as a series of its own and starts at the steady state of its own
first periods. That state is computed from the block's media, so a block
baked in pieces starts differently from the same block baked whole.

## Keeping the raw columns

With `keep_original_cols = TRUE` each selected column is left as it is
and its adstocked values go to a new column, `adstock_tv` for `tv` with
the default `prefix`. The new columns get the role given by `role`. The
raw columns keep theirs, so a model fitted on the baked data uses both
unless the raw ones get another role before this step, for example with
`update_role(tv, new_role = "spend")`. A new name that is already in the
data is an error.

## Selecting media columns

Select the media columns only.
[`all_predictors()`](https://recipes.tidymodels.org/reference/has_role.html)
also picks up controls such as price, seasonality and holiday flags, and
adstocking those is rarely what a model needs. Name the columns, or keep
their names in a character vector and use
[`all_of()`](https://tidyselect.r-lib.org/reference/all_of.html):

    channels <- c("tv", "video", "search", "social", "display")
    recipe(revenue ~ ., data = train) |>
      step_adstock(all_of(channels), index = "date", by = "geo")

The step drops `index` and `by` from the selection before checking
column types. To give each channel its own step in a loop, write
`all_of(!!ch)`: selectors are evaluated when the recipe is prepared,
after the loop has finished, so a bare `all_of(ch)` selects the last
channel in every step.
[`step_adstock_channels()`](https://elkronos.github.io/mediamix/reference/step_adstock_channels.md)
adds one step per channel without a loop, with a decay or a tuning
parameter for each.

## Tidying

[`tidy()`](https://generics.r-lib.org/reference/tidy.html) returns a
tibble with one row per selected column per group and the columns
`terms`, `group` (`NA` without `by`), `kernel`, `decay`, `max_lag`,
`shape`, `scale`, `type`, `peak`, `start`, `state`, `state_length` and
`id`. The kernel's parameters are `NA` where the kernel ignores them,
and before
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) when they
are marked with `tune()`. `state` is the filter's state at the end of
the training data. For `max_lag = Inf` it is the raw recursive
accumulator \\a_t\\, before the `normalise` scaling by \\1 - \theta\\,
and under `start = "steady"` it includes what remains of the starting
state. For a finite `max_lag` it is the most recent of the `max_lag - 1`
training values the kernel still reaches, and `state_length` gives their
number. Before
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) the state
columns are `NA`.

## Resampling and leakage

Stored state does not leak across a resampling boundary. Under any
rsample scheme,
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) re-runs
on each split's analysis set, so the state is re-learned from that
split's training rows alone and describes only media that precedes the
assessment set. The information that crosses the boundary is past *media
spend*, which is genuinely known at prediction time; no outcome
information crosses at all.

This step is strictly causal, unlike
[`recipes::step_window()`](https://recipes.tidymodels.org/reference/step_window.html)
and
[`recipes::step_impute_roll()`](https://recipes.tidymodels.org/reference/step_impute_roll.html),
which are centred and fill the leading edge of a series using future
values. Applied to a media filter, that would leak future spend into
past adstock and inflate in-sample fit. The exception is
`start = "steady"`, whose starting state averages the media of a series'
first periods.

## See also

[`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md),
[`adstock_weibull()`](https://elkronos.github.io/mediamix/reference/adstock_weibull.md),
[`adstock_delayed()`](https://elkronos.github.io/mediamix/reference/adstock_delayed.md),
[`adstock_steady_state()`](https://elkronos.github.io/mediamix/reference/adstock_steady_state.md),
[`step_adstock_channels()`](https://elkronos.github.io/mediamix/reference/step_adstock_channels.md),
[`step_saturation()`](https://elkronos.github.io/mediamix/reference/step_saturation.md),
[`carryover_decay()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md)

## Examples

``` r
library(recipes)
#> Loading required package: dplyr
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
#> 
#> Attaching package: ‘recipes’
#> The following object is masked from ‘package:stats’:
#> 
#>     step
data(mm_weekly)

train <- mm_weekly[mm_weekly$date < as.Date("2025-01-01"), ]
test <- mm_weekly[mm_weekly$date >= as.Date("2025-01-01"), ]

rec <- recipe(revenue ~ ., data = train) |>
  step_adstock(tv, video, search, index = "date", by = "geo", decay = 0.7) |>
  prep()

# Row count and order are preserved; no NA at the boundary
baked <- bake(rec, new_data = test)
nrow(baked) == nrow(test)
#> [1] TRUE
sum(is.na(baked$tv))
#> [1] 0

# The filter state at the end of training, per column and geo
tidy(rec, number = 1)
#> # A tibble: 9 × 13
#>   terms  group   kernel    decay max_lag shape scale type   peak start state
#>   <chr>  <chr>   <chr>     <dbl>   <dbl> <dbl> <dbl> <chr> <dbl> <chr> <dbl>
#> 1 tv     north   geometric   0.7     Inf    NA    NA NA       NA zero  1709.
#> 2 video  north   geometric   0.7     Inf    NA    NA NA       NA zero  1174.
#> 3 search north   geometric   0.7     Inf    NA    NA NA       NA zero  1504.
#> 4 tv     central geometric   0.7     Inf    NA    NA NA       NA zero  1732.
#> 5 video  central geometric   0.7     Inf    NA    NA NA       NA zero   758.
#> 6 search central geometric   0.7     Inf    NA    NA NA       NA zero  1094.
#> 7 tv     south   geometric   0.7     Inf    NA    NA NA       NA zero  5923.
#> 8 video  south   geometric   0.7     Inf    NA    NA NA       NA zero  1331.
#> 9 search south   geometric   0.7     Inf    NA    NA NA       NA zero  2186.
#> # ℹ 2 more variables: state_length <int>, id <chr>

# A delayed peak, started at steady state, with the raw spend kept
rec_delayed <- recipe(revenue ~ ., data = train) |>
  step_adstock(tv, index = "date", by = "geo", kernel = "delayed",
               decay = 0.6, peak = 1, max_lag = 6, start = "steady",
               keep_original_cols = TRUE) |>
  prep()
baked_delayed <- bake(rec_delayed, new_data = test)
head(baked_delayed[c("date", "geo", "tv", "adstock_tv")])
#> # A tibble: 6 × 4
#>   date       geo      tv adstock_tv
#>   <date>     <fct> <dbl>      <dbl>
#> 1 2025-01-06 north  2166       556.
#> 2 2025-01-13 north  1920      1418.
#> 3 2025-01-20 north  1454      1749.
#> 4 2025-01-27 north  1198      1541.
#> 5 2025-02-03 north  2631      1675.
#> 6 2025-02-10 north  2106      2061.
```
