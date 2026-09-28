# Select carryover parameters by cross-validation against a KPI

Searches a grid of geometric adstock parameters and returns the pair
that best predicts a held-out key performance indicator, using
forward-only resampling and a model you supply.

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
  warm_start = FALSE
)
```

## Arguments

- x:

  Numeric vector of media spend in time order.

- y:

  Numeric vector of the KPI, the same length as `x`.

- fit_fn:

  A function of `(x_adstocked, y)` returning a fitted model. Defaults to
  [`fit_ols()`](https://elkronos.github.io/mediamix/reference/fit_ols.md).
  When `controls` are supplied it receives a data frame instead – the
  adstocked media as column `media`, then the controls – and the default
  becomes least squares on all of them. For several channels at once,
  see
  [`tune_carryover_joint()`](https://elkronos.github.io/mediamix/reference/tune_carryover_joint.md).

- predict_fn:

  A function of `(model, newdata)` returning predictions, where
  `newdata` is the adstocked media for the assessment rows. Defaults to
  [`stats::predict()`](https://rdrr.io/r/stats/predict.html).

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

- cores:

  Number of cores. Values above 1 use
  [`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html) on
  Unix-alikes and a socket cluster elsewhere.

- aggregate:

  How to turn the assessment sets into one score. `"pooled"` (the
  default) collects every out-of-sample prediction across splits and
  evaluates `metric_fn` once on the lot, so `rmse` really is the root
  mean squared forecast error. `"mean"` evaluates `metric_fn` on each
  split and averages, which is the tune convention; note that with
  `assess = 1` a per-split RMSE is an absolute error, so the mean of
  them is a mean absolute error whatever `metric_fn` is called.

- controls:

  Optional data frame of control variables (trend, price, seasonality,
  ...) entered into the model untransformed, so carryover is judged with
  them accounted for. The rows are kept aligned with every split
  automatically.

- warm_start:

  Seed the adstock with
  [`adstock_steady_state()`](https://elkronos.github.io/mediamix/reference/adstock_steady_state.md)
  rather than zero, removing the start-up bias of a long carryover.

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
  `metric`, `std_err` and `n_splits`. `std_err` is the standard error of
  the per-split metric, `sd / sqrt(n_splits)`.

- `metric`:

  Name of the metric function used.

- `aggregate`:

  How the splits were combined.

- `scheme`:

  The resampling scheme used.

- `n_splits`:

  Number of resampling splits evaluated.

- `normalise`:

  Whether the adstock kernel was normalised, carried through so the
  selected decay can be reapplied on the same footing.

## What this fixes

It is tempting to choose carryover parameters by asking which
transformed series is best-behaved – smoothest, most trend-like, easiest
to extrapolate. That criterion has no dependent variable in it, and it
has a defect that is easy to miss: the objective is monotone in how hard
the filter smooths, so it drifts toward the longest lag and highest
decay in whatever grid it is given. The chosen parameters then say more
about the grid's upper corner than about the media.

The fix is to put the KPI in the loop. Here every candidate
`(max_lag, decay)` is scored by how well a model fitted on adstocked
media predicts *held-out KPI*, so the winner is the carryover structure
that actually improves forecasts of the thing being modelled. It also
means the answer depends on the model you intend to fit, which is a
feature: carryover and the rest of the specification are not separable,
and pretending otherwise is how a three-week half-life turns into a
nine-week one once seasonality is added.

## Why full-series adstock is safe here

The media series is adstocked once per grid point and then sliced into
folds, rather than being re-adstocked inside each fold. That is valid
because the filter is causal: the adstock value at time \\t\\ depends
only on media at times \\\le t\\. Assessment-period regressors therefore
draw on training -period *media*, which is information genuinely
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
*shortest* carryover – lowest half-life, then shortest kernel – that the
resampling cannot distinguish from the best candidate. When the two
disagree, the shorter carryover is the more conservative claim.

The comparison is *paired*: every candidate is scored on the same
splits, so a candidate qualifies when its mean per-split error exceeds
the best candidate's by no more than one standard error of the per-split
*difference*. The textbook unpaired version compares against the
standard error of the best candidate's own mean error, which on
time-series splits is dominated by how hard each period is to forecast –
common to every candidate – and so admits nearly the whole grid. The
`std_err` column reports that unpaired standard error, as a measure of
forecast noise. The rule is computed on per-split errors whatever
`aggregate` is, because that is where a standard error comes from.

## Start-up bias

Adstock is cold-started at zero, so the first few periods of a
long-carryover channel are understated: with `decay = 0.85` a constant
spend reaches only 15% of its steady-state level in week one and 90%
after `effective_window(0.85)` = 15 weeks. With `initial` at half the
series this rarely decides the answer, but it biases the fit towards
short carryover on short series. `warm_start = TRUE` seeds every
candidate with
[`adstock_steady_state()`](https://elkronos.github.io/mediamix/reference/adstock_steady_state.md);
better still, pass real pre-period spend through
[`adstock_state()`](https://elkronos.github.io/mediamix/reference/adstock_state.md)
and your own `fit_fn` when you have it.

## References

Breiman, L., Friedman, J., Olshen, R. and Stone, C. (1984).
*Classification and Regression Trees*. Wadsworth.

Hastie, T., Tibshirani, R. and Friedman, J. (2009). *The Elements of
Statistical Learning* (2nd ed.). Springer.

Bergmeir, C. and Benitez, J. M. (2012). On the use of cross-validation
for time series predictor evaluation. *Information Sciences*, 191,
192–213.

## See also

[`tune_carryover_joint()`](https://elkronos.github.io/mediamix/reference/tune_carryover_joint.md)
for several channels at once,
[`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md),
[`fit_ols()`](https://elkronos.github.io/mediamix/reference/fit_ols.md),
[`effective_window()`](https://elkronos.github.io/mediamix/reference/decay_vocabulary.md)

## Examples

``` r
# Recover a known decay from data generated with it
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
#> rmse (pooled) = 7.3134, std. error 0.586
tuned$best$decay
#> [1] 0.7

# The shortest carryover the data cannot distinguish from the best
tuned$best_1se$decay
#> [1] 0.7

# Tune against a model with controls rather than the bare default. The
# cleanest way is to residualise the KPI first, which needs no row
# bookkeeping: `tune_carryover()` then sees a response the controls have
# already explained away.
season <- sin(2 * pi * seq_len(n) / 52)
trend <- seq_len(n)
kpi2 <- kpi + 40 * trend + 300 * season

bare <- suppressWarnings(tune_carryover(
  spend, kpi2, max_lags = Inf, decays = seq(0.1, 0.9, by = 0.1)
))

controls <- stats::lm(kpi2 ~ trend + season)
residualised <- suppressWarnings(tune_carryover(
  spend, stats::residuals(controls),
  max_lags = Inf, decays = seq(0.1, 0.9, by = 0.1)
))

# The truth is 0.7. Ignoring the trend and seasonality moves the answer.
c(truth = 0.7, bare = bare$best$decay, residualised = residualised$best$decay)
#>        truth         bare residualised 
#>          0.7          0.9          0.7 
```
