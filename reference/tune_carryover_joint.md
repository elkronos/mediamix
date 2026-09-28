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
  warm_start = FALSE
)
```

## Arguments

- media:

  A data frame (or matrix with column names) of raw media, one column
  per channel, rows in time order.

- y:

  Numeric vector of the KPI, one value per row of `media`.

- controls:

  Optional data frame of control variables – trend, price, seasonality,
  holidays – entered into the model untransformed. Character and factor
  columns are expanded to dummies.

- fit_fn, predict_fn:

  Optional model. `fit_fn(X, y)` receives a data frame of the adstocked
  media followed by the controls; `predict_fn(model, X)` returns
  predictions for new rows of the same shape. Both `NULL` (the default)
  fits ordinary least squares with an intercept.

- max_lags, decays:

  Candidate kernel lengths and decays, shared by every channel. See
  [`tune_carryover()`](https://elkronos.github.io/mediamix/reference/tune_carryover.md).

- normalise:

  Should the adstock kernel be normalised to sum to 1? See
  [`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md).

- scheme:

  Resampling scheme. `"rolling_origin"` (the default) grows the training
  window one step at a time and assesses on the periods immediately
  following. `"k_fold_forward"` splits the series into `k` contiguous
  forward blocks. Both are forward-only; neither ever trains on data
  that follows what it assesses.

- metric_fn:

  A function of `(actual, predicted)` returning a single number to be
  minimised. Defaults to
  [`rmse()`](https://elkronos.github.io/mediamix/reference/metrics.md).

- initial:

  Size of the first training window. Defaults to half the series for
  `scheme = "rolling_origin"`, and to `floor(n / (k + 1))` for
  `scheme = "k_fold_forward"`, in both cases rounded down.

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
  split and averages, which is the tune convention; note that with
  `assess = 1` a per-split RMSE is an absolute error, so the mean of
  them is a mean absolute error whatever `metric_fn` is called.

- start:

  Optional named numeric vector of starting decays, one per channel.
  Defaults to the middle of `decays`.

- max_iter:

  Maximum number of full sweeps over the channels.

- warm_start:

  Seed each adstocked series with
  [`adstock_steady_state()`](https://elkronos.github.io/mediamix/reference/adstock_steady_state.md)
  instead of starting from zero, removing the start-up bias of
  long-carryover channels.

## Value

An object of class `mm_carryover_joint`: a list with

- `best`:

  One row per channel: `channel`, `max_lag`, `decay`, `half_life`.

- `best_1se`:

  The same, with each channel's decay chosen by the paired
  one-standard-error rule on its final profile.

- `profiles`:

  Each channel's cross-validation profile from the final sweep, with the
  other channels held at their selected values: `channel`, `max_lag`,
  `decay`, `half_life`, `metric`, `std_err`.

- `metric`:

  The cross-validated metric at the selected values.

- `iterations`, `converged`:

  Sweeps run, and whether the last sweep changed nothing.

plus `metric_name`, `scheme`, `aggregate`, `n_splits` and `normalise`.

## Details

The search is coordinate descent: each sweep visits the channels in turn
and sets each one to its best grid point with the others held fixed,
repeating until a sweep changes nothing. Each step can only lower the
cross-validated error, so the search terminates, but like any coordinate
method it finds a local optimum of the grid, not necessarily the global
one. A different `start` is a cheap check. The cost is roughly
`channels x grid size x sweeps` cross-validated fits, far fewer than the
full grid's `grid size ^ channels`.

Saturation is not tuned here. For carryover, saturation and a model
penalty tuned jointly, use
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
and
[`step_saturation()`](https://elkronos.github.io/mediamix/reference/step_saturation.md)
with tune; see
[`vignette("tidymodels")`](https://elkronos.github.io/mediamix/articles/tidymodels.md).

## See also

[`tune_carryover()`](https://elkronos.github.io/mediamix/reference/tune_carryover.md)
for one channel,
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
                              skip = 3)
joint
#> 
#> ── Joint carryover tuning 
#> 5 channels, rolling_origin resampling, 20 splits; 3 sweeps, converged
#> rmse (pooled) = 788.545
#>  channel max_lag decay half_life decay_1se
#>       tv     Inf  0.85     4.265      0.65
#>    video     Inf  0.45     0.868      0.25
#>   search     Inf  0.75     2.409      0.05
#>   social     Inf  0.65     1.609      0.05
#>  display     Inf  0.95    13.513      0.05
rbind(truth = attr(mm_weekly, "truth")$decay[channels],
      joint = joint$best$decay)
#>         tv video search social display
#> truth 0.85  0.70   0.15   0.45    0.55
#> joint 0.85  0.45   0.75   0.65    0.95
```
