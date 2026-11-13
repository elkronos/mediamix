# Tune every channel's carryover jointly, inside one model

The multi-channel counterpart of
[`tune_carryover()`](https://elkronos.github.io/mediamix/reference/tune_carryover.md).
Each channel's `(max_lag, decay)` is chosen by forward-only
cross-validation of a model containing *all* the channels and any
control variables, so a channel's carryover is judged with the others'
effects accounted for rather than absorbed.

## Usage

``` r
tune_carryover_joint(
  media,
  y,
  controls = NULL,
  fit_fn = NULL,
  predict_fn = NULL,
  max_lags = Inf,
  decays = seq(0.05, 0.95, by = 0.05),
  normalise = TRUE,
  scheme = c("rolling_origin", "k_fold_forward"),
  metric_fn = rmse,
  initial = NULL,
  assess = 1L,
  skip = 0L,
  k = 5L,
  aggregate = c("pooled", "mean"),
  start = NULL,
  max_iter = 10L,
  warm_start = FALSE,
  cores = 1L,
  by = NULL,
  index = NULL,
  se_block = 1L,
  save_pred = FALSE
)
```

## Arguments

- media:

  A data frame (or matrix with column names) of raw media, one column
  per channel, rows in time order unless `index` is given. Every column
  must be finite and must vary.

- y:

  Numeric vector of the KPI, one value per row of `media`.

- controls:

  Optional data frame of control variables, such as trend, price,
  seasonality and holidays, entered into the model untransformed.
  Character, factor and logical columns are expanded to treatment
  dummies by
  [`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html)
  and every name is made syntactic. A column with a single value is
  dropped with a message, and missing or non-finite values are an error
  that names the column and rows. No control may share a name with a
  channel.

- fit_fn, predict_fn:

  Optional model. `fit_fn(X, y)` receives a data frame of the adstocked
  media followed by the controls; `predict_fn(model, X)` returns
  predictions for new rows of the same shape. Leaving both `NULL` (the
  default) fits ordinary least squares with an intercept; giving only
  one is an error.

- max_lags, decays:

  Candidate kernel lengths and decays, shared by every channel. See
  [`tune_carryover()`](https://elkronos.github.io/mediamix/reference/tune_carryover.md).

- normalise:

  Should the adstock kernel be normalised to sum to 1? See
  [`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md).

- scheme:

  Resampling scheme. `"rolling_origin"` (the default) grows the training
  window one step at a time and assesses on the periods immediately
  following. `"k_fold_forward"` splits the periods after the first
  training window into `k` contiguous blocks and assesses each with a
  model trained on every period before it. Both are forward-only;
  neither ever trains on data that follows what it assesses.

- metric_fn:

  A function of `(actual, predicted)` returning a single number to be
  minimised. Defaults to
  [`rmse()`](https://elkronos.github.io/mediamix/reference/metrics.md).

- initial:

  Size of the first training window, in periods. Defaults to half the
  series for `scheme = "rolling_origin"`, and to `floor(n / (k + 1))`
  for `scheme = "k_fold_forward"`, in both cases rounded down.

- assess:

  Number of periods assessed per split. Used by
  `scheme = "rolling_origin"` only; the forward-block scheme's
  assessment sizes are determined by `k` and the series length.

- skip:

  Number of splits to skip between evaluations, to make a long series
  cheaper. `0` evaluates every split. Used by
  `scheme = "rolling_origin"` only.

- k:

  Number of forward blocks when `scheme = "k_fold_forward"`. Ignored
  otherwise.

- aggregate:

  How to turn the assessment sets into one score. `"pooled"` (the
  default) collects every out-of-sample prediction across splits and
  evaluates `metric_fn` once on the lot, so `rmse` really is the root
  mean squared forecast error. `"mean"` evaluates `metric_fn` on each
  split and averages, which is the tune convention. With `assess = 1` a
  per-split RMSE is an absolute error, so the mean of them is a mean
  absolute error whatever `metric_fn` is called.

- start:

  Optional numeric vector of starting decays in `[0, 1]`, named by
  channel. Each channel it names starts at the grid point with the
  nearest decay; the others start, as every channel does by default, at
  the middle of `decays`.

- max_iter:

  Maximum number of full sweeps over the channels.

- warm_start:

  Seed each adstocked series with
  [`adstock_steady_state()`](https://elkronos.github.io/mediamix/reference/adstock_steady_state.md)
  instead of starting from zero, removing the start-up bias of
  long-carryover channels. As in
  [`tune_carryover()`](https://elkronos.github.io/mediamix/reference/tune_carryover.md),
  the seed uses no media from beyond the first training window.

- cores:

  Number of cores over which each channel's grid points are scored.
  Values above 1 use
  [`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html) on
  Unix-alikes and a socket cluster elsewhere. The workers of a socket
  cluster are new R sessions that load mediamix and nothing else, so
  there `fit_fn` and `predict_fn` must call other packages with
  `pkg::fun()` and must not rely on functions or data defined in your
  session. `metric_fn` runs in your session.

- by, index:

  Panel data, as in
  [`tune_carryover()`](https://elkronos.github.io/mediamix/reference/tune_carryover.md).
  `by` assigns each row to a series, as a vector or a data frame of key
  columns, and `index` gives each row's period, as a number,
  [Date](https://rdrr.io/r/base/Dates.html) or
  [POSIXct](https://rdrr.io/r/base/DateTimeClasses.html). With `index`
  the rows may come in any order. `by` needs `index`. Every channel is
  adstocked within each series; see *Panel data* in
  [`tune_carryover()`](https://elkronos.github.io/mediamix/reference/tune_carryover.md).

- se_block:

  Number of consecutive splits the jackknife leaves out together when it
  computes `std_err` and the one-standard-error rule. `1`, the default,
  treats the splits as independent. Successive forecasts share most of
  their training data, so their errors are usually correlated and
  independence understates the standard error; a block of a few splits
  allows for that. `best` does not depend on it.

- save_pred:

  Keep the out-of-sample predictions of the selected values? `FALSE` by
  default, as in tune's `control_resamples()`. With `TRUE` the result
  gains `predictions`, for `best` and for the `best_1se` set. The other
  grid points' predictions are not kept: each channel's final profile
  alone holds one set per grid point, so keeping them would need a row
  for every channel, grid point and assessed row.

## Value

An object of class `mm_carryover_joint`: a list with

- `best`:

  One row per channel: `channel`, `max_lag`, `decay`, `half_life`.

- `best_1se`:

  The same, with each channel's value chosen by the paired
  one-standard-error rule on its own final profile, anchored on its
  value in `best`. See *The one-standard-error set*.

- `best_1se_metric`:

  The cross-validated metric of the `best_1se` values taken together.

- `profiles`:

  Each channel's cross-validation profile from the final sweep, with the
  other channels held at their selected values: `channel`, `max_lag`,
  `decay`, `half_life`, `metric`, `std_err`.

- `metric`:

  The cross-validated metric at the selected values, scored on the
  `n_ok` splits that the `best` and `best_1se` sets both completed.

- `metric_name`:

  Name of the metric function used.

- `iterations`, `converged`:

  Sweeps run, and whether the last sweep changed nothing.

- `predictions`:

  Only with `save_pred = TRUE`: the forecasts of `best` and of the
  `best_1se` set on the splits both completed, one row per set per
  assessed row. Columns: `selection` (`"best"` or `"best_1se"`), `split`
  (the split's position among the `n_splits` formed), `row` (the row of
  `media` and `y`), `index` and `group` (with `index` and `by`),
  `max_lag_<channel>` and `decay_<channel>` for each channel, `observed`
  and `predicted`. When the two sets are the same, the `best_1se` rows
  repeat the `best` rows.

plus `scheme`, `aggregate`, `n_splits`, `n_ok`, `n_series` and
`normalise`, as in
[`tune_carryover()`](https://elkronos.github.io/mediamix/reference/tune_carryover.md),
except that `n_ok` counts the splits the two selected sets completed
rather than every grid point. Each channel's profile is compared on the
splits its own grid completed, which can be fewer.
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) and
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) return
`profiles` with logical columns `best` and `best_1se` marking each
channel's selections.

## Details

The search is coordinate descent: each sweep visits the channels in turn
and sets each one to its best grid point with the others held fixed,
repeating until a sweep changes nothing. Each step can only lower the
cross-validated error, so the search terminates, but like any coordinate
method it finds a local optimum of the grid, not necessarily the global
one. A different `start` is a cheap check. The cost is roughly
`channels x grid size x sweeps` cross-validated fits, far fewer than the
full grid's `grid size ^ channels`; `cores` spreads each channel's grid
points over several processes.

A grid point whose fit fails on some splits is handled as in
[`tune_carryover()`](https://elkronos.github.io/mediamix/reference/tune_carryover.md):
within each step every grid point is scored on the splits all of them
completed. A warning reports the most splits any step left out, and a
selection on the edge of the grid is reported for each channel.

Saturation is not tuned here. For carryover, saturation and a model
penalty tuned jointly, use
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
and
[`step_saturation()`](https://elkronos.github.io/mediamix/reference/step_saturation.md)
with tune; see
[`vignette("tidymodels")`](https://elkronos.github.io/mediamix/articles/tidymodels.md).

## The one-standard-error set

Each channel's one-standard-error choice is made on its own profile,
with the other channels held at `best`, so the rule is applied channel
by channel. Shortening several channels at once can cost more than any
one of them does alone, and the combination was not part of the search.
The set is therefore scored as a whole after the search, and
`best_1se_metric` reports that score next to `metric`. When the gap
between the two is large against the profiles' `std_err`, the channels'
near-ties do not add up to a near-tie for the set, and `best_1se` is
better read one channel at a time.

## See also

[`tune_carryover()`](https://elkronos.github.io/mediamix/reference/tune_carryover.md)
for one channel,
[`plot_carryover()`](https://elkronos.github.io/mediamix/reference/plot_carryover.md)
for the profiles,
[`adstock_steady_state()`](https://elkronos.github.io/mediamix/reference/adstock_steady_state.md)

## Examples

``` r
data(mm_weekly)
north <- mm_weekly[mm_weekly$geo == "north", ]
channels <- c("tv", "video", "search", "social", "display")
controls <- data.frame(week = seq_len(nrow(north)), price = north$price,
                       seasonality = north$seasonality,
                       holiday = north$holiday)

joint <- tune_carryover_joint(north[channels], north$revenue,
                              controls = controls,
                              decays = seq(0.05, 0.95, by = 0.1),
                              skip = 3, save_pred = TRUE)
#> Warning: The chosen parameters sit on the edge of the grid.
#> • display: the selected decay (0.95) is the largest value searched.
#> ℹ Widen the grid and re-run. A parameter pinned to a boundary usually means the
#>   optimum is outside it, or that the series is too short to identify carryover
#>   this long.
joint
#> 
#> ── Joint carryover tuning 
#> 5 channels, rolling_origin resampling, 20 splits; 3 sweeps, converged
#> rmse (pooled) = 788.545
#> One-SE set, chosen channel by channel: rmse = 867.986
#>  channel max_lag decay half_life decay_1se
#>       tv     Inf  0.85     4.265      0.85
#>    video     Inf  0.45     0.868      0.05
#>   search     Inf  0.75     2.409      0.05
#>   social     Inf  0.65     1.609      0.25
#>  display     Inf  0.95    13.513      0.45
#> 40 out-of-sample predictions kept in `$predictions`.
rbind(truth = attr(mm_weekly, "truth")$decay[channels],
      joint = joint$best$decay)
#>         tv video search social display
#> truth 0.85  0.70   0.15   0.45    0.55
#> joint 0.85  0.45   0.75   0.65    0.95

# The selected values' forecasts of the weeks each split held out
kept <- joint$predictions[joint$predictions$selection == "best", ]
head(kept[c("split", "row", "observed", "predicted")])
#>   split row observed predicted
#> 1     1  79    29396  29986.39
#> 2     2  83    24098  22656.69
#> 3     3  87    17509  17744.49
#> 4     4  91    23845  23695.48
#> 5     5  95    20278  20235.17
#> 6     6  99    21398  22520.96
```
