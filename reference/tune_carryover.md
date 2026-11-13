# Select carryover parameters by cross-validation against a KPI

Searches a grid of geometric adstock parameters and returns the pair
that best predicts a held-out key performance indicator, using
forward-only resampling and least squares or a model you supply.

## Usage

``` r
tune_carryover(
  x,
  y,
  fit_fn = fit_ols,
  predict_fn = stats::predict,
  max_lags = Inf,
  decays = seq(0.1, 0.9, by = 0.05),
  normalise = TRUE,
  scheme = c("rolling_origin", "k_fold_forward"),
  metric_fn = rmse,
  initial = NULL,
  assess = 1L,
  skip = 0L,
  k = 5L,
  cores = 1L,
  aggregate = c("pooled", "mean"),
  controls = NULL,
  warm_start = FALSE,
  by = NULL,
  index = NULL,
  se_block = 1L,
  save_pred = FALSE
)
```

## Arguments

- x:

  Numeric vector of media spend, in time order unless `index` is given.
  It must be finite and must vary: a constant series carries no
  information about carryover.

- y:

  Numeric vector of the KPI, the same length as `x`. It may have missing
  values: the default model leaves those rows out of its fits, and a
  split with no KPI to assess is left out.

- fit_fn:

  A function of `(x_adstocked, y)` returning a fitted model. Defaults to
  [`fit_ols()`](https://elkronos.github.io/mediamix/reference/fit_ols.md).
  When `controls` are supplied it receives a data frame instead, with
  the adstocked media as column `media` followed by the controls, and
  the default becomes least squares on all of them. For several channels
  at once, see
  [`tune_carryover_joint()`](https://elkronos.github.io/mediamix/reference/tune_carryover_joint.md).

- predict_fn:

  A function of `(model, newdata)` returning predictions, where
  `newdata` is the adstocked media for the assessment rows, or with
  `controls` a data frame of the same shape `fit_fn` receives. Defaults
  to [`stats::predict()`](https://rdrr.io/r/stats/predict.html).

- max_lags:

  Integer vector of candidate kernel lengths. `Inf` is allowed and
  denotes the infinite recursive kernel.

- decays:

  Numeric vector of candidate decay coefficients in `[0, 1]`.

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

- cores:

  Number of cores. Values above 1 use
  [`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html) on
  Unix-alikes and a socket cluster elsewhere. The workers of a socket
  cluster are new R sessions that load mediamix and nothing else, so
  there `fit_fn` and `predict_fn` must call other packages with
  `pkg::fun()` and must not rely on functions or data defined in your
  session. `metric_fn` runs in your session.

- aggregate:

  How to turn the assessment sets into one score. `"pooled"` (the
  default) collects every out-of-sample prediction across splits and
  evaluates `metric_fn` once on the lot, so `rmse` really is the root
  mean squared forecast error. `"mean"` evaluates `metric_fn` on each
  split and averages, which is the tune convention. With `assess = 1` a
  per-split RMSE is an absolute error, so the mean of them is a mean
  absolute error whatever `metric_fn` is called.

- controls:

  Optional data frame of control variables (trend, price, seasonality,
  ...) entered into the model untransformed, so carryover is judged with
  them accounted for. The rows are kept aligned with every split
  automatically. Factor, character and logical columns are expanded to
  treatment dummies by
  [`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html)
  and every name is made syntactic, so `fit_fn` sees columns such as
  `seasonQ2` rather than `season`. A column with a single value adds
  nothing beside an intercept and is dropped with a message. Missing or
  non-finite values are an error that names the column and rows: impute
  them, or drop those periods from every input.

- warm_start:

  Seed the adstock with
  [`adstock_steady_state()`](https://elkronos.github.io/mediamix/reference/adstock_steady_state.md)
  rather than zero, removing the start-up bias of a long carryover. The
  seed averages spend over no more than the first training window, so it
  uses only media that every split trains on.

- by, index:

  Panel data. `by` assigns each row to a series, as a vector or a data
  frame of key columns, and `index` gives each row's period, as a
  number, [Date](https://rdrr.io/r/base/Dates.html) or
  [POSIXct](https://rdrr.io/r/base/DateTimeClasses.html). With `index`
  the rows may come in any order. `by` needs `index`. See *Panel data*.

- se_block:

  Number of consecutive splits the jackknife leaves out together when it
  computes `std_err` and the one-standard-error rule. `1`, the default,
  treats the splits as independent. Successive forecasts share most of
  their training data, so their errors are usually correlated and
  independence understates the standard error; a block of a few splits
  allows for that. `best` does not depend on it.

- save_pred:

  Keep the out-of-sample predictions? `FALSE` by default, as in tune's
  `control_resamples()`. With `TRUE` the result gains `predictions`:
  every candidate's forecasts for the assessed rows of the splits it was
  scored on. The fits produce them in any case, so keeping them costs
  only the table, one row per candidate per assessed row.

## Value

An object of class `mm_carryover`: a list with elements

- `best`:

  One-row data frame with the selected `max_lag` and `decay`, plus the
  corresponding `half_life`, `metric`, `std_err` and `n_splits`.

- `best_1se`:

  The same, for the candidate chosen by the one-standard-error rule. See
  *Choosing among near-ties*.

- `results`:

  Data frame of the full grid: `max_lag`, `decay`, `half_life`,
  `metric`, `std_err` and `n_splits`. `half_life` is `NA` where the
  kernel carries nothing over (`decay = 0` or `max_lag = 1`) or never
  decays (`decay = 1`). `std_err` is the jackknife standard error of
  `metric` over splits; for `aggregate = "mean"` with `se_block = 1` it
  is `sd / sqrt(n_splits)` of the per-split values. `n_splits` is the
  number of splits every candidate was scored on.

- `metric`:

  The cross-validated metric of `best`.

- `metric_name`:

  Name of the metric function used.

- `aggregate`:

  How the splits were combined.

- `scheme`:

  The resampling scheme used.

- `n_splits`:

  Number of resampling splits formed.

- `n_ok`:

  Number of splits every candidate was scored on. See *Failed fits*.

- `n_series`:

  Number of series, more than 1 only with `by`.

- `normalise`:

  Whether the adstock kernel was normalised, carried through so the
  selected decay can be reapplied on the same footing.

- `predictions`:

  Only with `save_pred = TRUE`: one row per candidate per assessed row
  of the `n_ok` splits `metric` was computed on, with columns `split`
  (the split's position among the `n_splits` formed), `row` (the row of
  `x` and `y`), `index` and `group` (with `index` and `by`), `max_lag`,
  `decay`, `observed` and `predicted`. Candidates come in the order of
  `results`, and a candidate that completed no split has no rows. With
  `aggregate = "pooled"`, `metric_fn(observed, predicted)` over one
  candidate's rows gives its `metric`.

[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) and
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) return
`results` with two logical columns, `best` and `best_1se`, marking the
two selections.

## What this fixes

It is tempting to choose carryover parameters by asking which
transformed series is best-behaved: smoothest, most trend-like, easiest
to extrapolate. That criterion has no dependent variable in it, so it
gives the same answer whatever the media did to the KPI. It ranks the
candidates by how hard the filter smooths the media, and its choice
usually lands at one end of the decay grid, the lowest decay for some
spend series and the highest for others. On the series simulated in
[`vignette("carryover")`](https://elkronos.github.io/mediamix/articles/carryover.md),
whose true decay is 0.7, its error rises with decay and it picks the
lowest. The chosen parameters then say more about the grid's edges than
about the media.

The fix is to put the KPI in the loop. Here every candidate
`(max_lag, decay)` is scored by how well a model fitted on adstocked
media predicts *held-out KPI*, so the winner is the carryover structure
that actually improves forecasts of the thing being modelled. It also
means the answer depends on the model you intend to fit, which is a
feature: carryover and the rest of the specification are not separable.
In the last example below, leaving the trend and seasonality out of the
model selects a longer carryover than the one the data were simulated
with.

## Why full-series adstock is safe here

The media series is adstocked once per grid point and then sliced into
folds, rather than being re-adstocked inside each fold. That is valid
because the filter is causal: the adstock value at time \\t\\ depends
only on media at times \\\le t\\. Assessment-period regressors therefore
draw on training-period *media*, which is information genuinely
available at prediction time, and never on future media. No KPI
information crosses the boundary at any point. The
`adstock(x)[1:m] == adstock(x[1:m])` invariant is enforced by the test
suite precisely because this shortcut depends on it.

## Choosing among near-ties

Carryover is weakly identified in most media data: the cross-validation
curve is often nearly flat across a wide band of decay, and the minimum
moves around from sample to sample. `best` reports the minimum;
`best_1se` applies the one-standard-error rule (Breiman et al., 1984;
Hastie, Tibshirani and Friedman, 2009, section 7.10), choosing the
*shortest* carryover (lowest half-life, then shortest kernel) that the
resampling cannot distinguish from the best candidate. When the two
disagree, the shorter carryover is the more conservative claim.

The comparison is *paired* and is made on the quantity `best` minimises.
Every candidate is scored on the same splits, and a candidate qualifies
when its `metric` exceeds that of `best` by no more than one standard
error of the *difference*. The standard error comes from a jackknife
over splits: both metrics are recomputed with each block of `se_block`
consecutive splits left out. For `aggregate = "mean"` with
`se_block = 1` it is the standard error of the mean per-split
difference; for the pooled RMSE with one-period assessment sets it
agrees, to first order, with a paired comparison of the per-forecast
squared errors. The textbook unpaired version compares against the
standard error of the best candidate's own metric, which on time-series
splits is dominated by how hard each period is to forecast, a cost
common to every candidate, and so admits nearly the whole grid. The
`std_err` column reports that unpaired standard error, as a measure of
forecast noise.

## Failed fits

When `fit_fn`, `predict_fn` or `metric_fn` fails on a split for some
candidates, or returns missing predictions, that split is left out for
every candidate, so all of them are compared on the same forecasts. A
warning quotes the first failure and `n_ok` counts the splits that
remain. Splits whose assessment periods have no KPI are left out without
a warning. When every candidate fails, the error quotes the first
failure and names the function that raised it.

## Start-up bias

Adstock is cold-started at zero, so the first few periods of a
long-carryover channel are understated: with `decay = 0.85` a constant
spend reaches only 15% of its steady-state level in week one and 90%
after `effective_window(0.85)` = 15 weeks. With `initial` at half the
series this rarely decides the answer, but it biases the fit towards
short carryover on short series. `warm_start = TRUE` seeds every
candidate with
[`adstock_steady_state()`](https://elkronos.github.io/mediamix/reference/adstock_steady_state.md),
averaging spend over `effective_window(decay)` periods or the first
training window, whichever is shorter, so the seed uses only media that
every split trains on. The tuner takes no starting state: it has no
`state` argument, and `fit_fn` receives media that is already adstocked.
To use real pre-period spend, put it at the front of `x` with `NA` for
those periods in `y`; the default model leaves rows without a KPI out of
every fit. Or tune with `warm_start = TRUE`, and when you apply the
selected decay, start
[`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md)
from the state
[`adstock_state()`](https://elkronos.github.io/mediamix/reference/adstock_state.md)
computes from the pre-period spend.

## Panel data

With `by` and `index`, `x`, `y` and `controls` hold several series
stacked, such as the geographies of
[mm_weekly](https://elkronos.github.io/mediamix/reference/mm_weekly.md).
Each series is adstocked on its own in `index` order, so carryover never
runs from one series into the next. Splits are formed on the distinct
values of `index`: `initial`, `assess`, `skip` and `k` count periods
rather than rows, every split trains on the same periods of every
series, and it assesses the periods that follow. One model is fitted to
all the series at once and the metric pools their forecasts; pass the
series as a control to give each its own intercept. A series has at most
one row per period, and its consecutive rows are taken to be consecutive
periods. Series may start and end in different periods; with
`warm_start`, a series that starts after the first training window is
cold-started.

## References

Breiman, L., Friedman, J., Olshen, R. and Stone, C. (1984).
*Classification and Regression Trees*. Wadsworth.
[doi:10.1201/9781315139470](https://doi.org/10.1201/9781315139470)

Hastie, T., Tibshirani, R. and Friedman, J. (2009). *The Elements of
Statistical Learning* (2nd ed.). Springer.
[doi:10.1007/978-0-387-84858-7](https://doi.org/10.1007/978-0-387-84858-7)

Bergmeir, C. and Benítez, J. M. (2012). On the use of cross-validation
for time series predictor evaluation. *Information Sciences*, 191,
192–213.
[doi:10.1016/j.ins.2011.12.028](https://doi.org/10.1016/j.ins.2011.12.028)

## See also

[`plot_carryover()`](https://elkronos.github.io/mediamix/reference/plot_carryover.md)
for the error across the grid,
[`tune_carryover_joint()`](https://elkronos.github.io/mediamix/reference/tune_carryover_joint.md)
for several channels at once,
[`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md),
[`fit_ols()`](https://elkronos.github.io/mediamix/reference/fit_ols.md),
[`effective_window()`](https://elkronos.github.io/mediamix/reference/decay_vocabulary.md)

## Examples

``` r
# Recover a known decay from data simulated with it
set.seed(42)
n <- 120
spend <- pmax(0, rnorm(n, 500, 250))
truth <- 0.7
kpi <- 200 + 0.5 * adstock_geometric(spend, decay = truth) + rnorm(n, sd = 8)

tuned <- tune_carryover(
  spend, kpi,
  max_lags = Inf,
  decays = seq(0.1, 0.9, by = 0.1)
)
tuned
#> 
#> ── Carryover tuning 
#> rolling_origin resampling, 60 splits, 9 grid points
#> Best: decay = 0.7 (half-life 1.94 periods), max_lag = infinite
#> rmse (pooled) = 7.3134, std. error 0.659
tuned$best$decay
#> [1] 0.7

# The shortest carryover the data cannot distinguish from the best
tuned$best_1se$decay
#> [1] 0.7

# Keep the out-of-sample forecasts, for example to look at the best one
kept <- tune_carryover(spend, kpi, max_lags = Inf,
                       decays = seq(0.1, 0.9, by = 0.1), save_pred = TRUE)
best <- kept$predictions[kept$predictions$decay == kept$best$decay, ]
head(best)
#>   split row max_lag decay observed predicted
#> 1     1  61     Inf   0.7 422.5377  419.9361
#> 2     2  62     Inf   0.7 432.2015  435.7249
#> 3     3  63     Inf   0.7 465.5809  461.7442
#> 4     4  64     Inf   0.7 513.6887  511.1374
#> 5     5  65     Inf   0.7 463.4245  464.9368
#> 6     6  66     Inf   0.7 499.1164  509.6529
c(rmse(best$observed, best$predicted), kept$best$metric)
#> [1] 7.313401 7.313401

# Tune against a model with controls rather than the bare default. Passed
# as `controls`, the trend and seasonality are fitted inside every training
# window, so no split learns from KPI values that come after it.
season <- sin(2 * pi * seq_len(n) / 52)
trend <- seq_len(n)
kpi2 <- kpi + 40 * trend + 300 * season

bare <- suppressWarnings(tune_carryover(
  spend, kpi2, max_lags = Inf, decays = seq(0.1, 0.9, by = 0.1)
))
with_controls <- tune_carryover(
  spend, kpi2, max_lags = Inf, decays = seq(0.1, 0.9, by = 0.1),
  controls = data.frame(trend, season)
)

# The truth is 0.7. Ignoring the trend and seasonality moves the answer.
c(truth = 0.7, bare = bare$best$decay, controls = with_controls$best$decay)
#>    truth     bare controls 
#>      0.7      0.9      0.7 
```
