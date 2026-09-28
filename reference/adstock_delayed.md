# Delayed adstock

The delayed-peak geometric kernel of Jin et al. (2017), \\w_l =
\theta^{(l - \delta)^2}\\ for lags \\l = 0, \ldots, L-1\\, where
\\\theta\\ is the retention rate and \\\delta\\ the lag at which the
effect peaks. With `peak = 0` the weights fall off as \\\theta^{l^2}\\,
faster than geometric; with `peak > 0` the response builds for `peak`
periods before it decays, the shape television and out-of-home often
show. It is the delayed form used in Google's Bayesian MMM work, and a
one-parameter-per-idea alternative to the Weibull kernel: `peak` is read
directly in periods.

## Usage

``` r
adstock_weights_delayed(max_lag, decay, peak = 0, normalise = TRUE)

adstock_delayed(
  x,
  decay,
  peak = 0,
  max_lag,
  normalise = TRUE,
  state = 0,
  by = NULL,
  na_action = c("error", "zero", "keep")
)
```

## Arguments

- max_lag:

  Number of periods the kernel spans, including the current period.
  Required and finite: the kernel has no recursive form. Jin et al. use
  13 weeks for weekly data.

- decay:

  Retention rate \\\theta\\ in `(0, 1)`.

- peak:

  Lag of the peak effect, in periods, in `[0, max_lag - 1]`. Need not be
  a whole number.

- normalise:

  Should the weights sum to 1? When `FALSE` the weights are left as
  \\\theta^{(l - \delta)^2}\\, whose largest value is 1 when the peak
  falls on a whole period.

- x:

  Numeric vector of media spend (or impressions, or GRPs) in time order.
  `x` must already be sorted by time and evenly spaced; the function has
  no index argument and cannot check this for you. Use
  [`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
  if you want the time index validated.

- state:

  The preceding `max_lag - 1` values of `x`, oldest first, or `0` for a
  cold start; a named list of them when `by` is supplied.

- by:

  Optional grouping vector, or data frame of grouping vectors, the same
  length as `x`. Adstock is applied independently within each group,
  which is what geo-level and panel models need. Never rely on
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  for this: grouping metadata does not reliably survive into every
  context where this function is called.

- na_action:

  What to do about missing values in `x`. `"error"` (the default)
  refuses to guess. `"zero"` treats missing media as no media, which is
  usually right for spend but is a substantive assumption. `"keep"` lets
  `NA` propagate through the filter, which for the recursive form
  poisons every subsequent value.

## Value

`adstock_weights_delayed()` returns the kernel, ordered from the current
period outward. `adstock_delayed()` returns a numeric vector the same
length as `x`, in the same order.

## References

Jin, Y., Wang, Y., Sun, Y., Chan, D. and Koehler, J. (2017). Bayesian
methods for media mix modeling with carryover and shape effects. Google
Inc. <https://research.google/pubs/pub46001/>

## See also

[`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md),
[`adstock_weibull()`](https://elkronos.github.io/mediamix/reference/adstock_weibull.md),
[`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md)

## Examples

``` r
# Peak two weeks after the spend
round(adstock_weights_delayed(8, decay = 0.6, peak = 2), 3)
#> [1] 0.052 0.243 0.405 0.243 0.052 0.004 0.000 0.000

spend <- c(100, 0, 0, 0, 0, 0, 0, 0)
round(adstock_delayed(spend, decay = 0.6, peak = 2, max_lag = 8), 2)
#> [1]  5.25 24.30 40.49 24.30  5.25  0.41  0.01  0.00

# Through media_transform()
media_transform(spend, adstock = list(kernel = "delayed", decay = 0.6,
                                      peak = 2, max_lag = 8))
#> [1] 5.247893e+00 2.429580e+01 4.049300e+01 2.429580e+01 5.247893e+00
#> [6] 4.080761e-01 1.142352e-02 1.151228e-04
```
