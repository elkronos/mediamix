# Weibull adstock

Adstock with a Weibull kernel, which can place its peak after the period
of spend. See
[`adstock_weights_weibull()`](https://elkronos.github.io/mediamix/reference/adstock_weights_weibull.md)
for the two parameterisations.

## Usage

``` r
adstock_weibull(
  x,
  shape,
  scale,
  max_lag,
  type = c("cdf", "pdf"),
  normalise = TRUE,
  state = 0,
  by = NULL,
  na_action = c("error", "zero", "keep")
)
```

## Arguments

- x:

  Numeric vector of media spend (or impressions, or GRPs) in time order.
  `x` must already be sorted by time and evenly spaced; the function has
  no index argument and cannot check this for you. Use
  [`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
  if you want the time index validated.

- shape, scale:

  Weibull shape and scale, both positive. `scale` is in periods.

- max_lag:

  Number of periods the kernel spans. Required and finite: the Weibull
  kernel has no recursive form.

- type:

  Either `"cdf"` (monotone decay) or `"pdf"` (permits a delayed peak).

- normalise:

  Should the kernel sum to 1? Defaults to `TRUE`. See the
  *Normalisation* section.

- state:

  The preceding `max_lag - 1` values of `x`, oldest first, or `0` for a
  cold start. When `by` is supplied, a named list with one entry per
  group.
  [`adstock_state()`](https://elkronos.github.io/mediamix/reference/adstock_state.md)
  produces one of the right shape when called with the same `max_lag`;
  for a finite kernel the state is simply the tail of `x`, so
  `utils::tail(x, max_lag - 1)` works too.

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

A numeric vector the same length as `x`, in the same order.

## See also

[`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md),
[`adstock_weights_weibull()`](https://elkronos.github.io/mediamix/reference/adstock_weights_weibull.md)

## Examples

``` r
spend <- c(100, 0, 0, 0, 0, 0, 0, 0)

# Delayed peak: the response builds before it decays
round(adstock_weibull(spend, shape = 2, scale = 3, max_lag = 8,
                      type = "pdf"), 2)
#> [1] 20.27 29.05 25.00 15.31  7.04  2.49  0.69  0.15

# Monotone form
round(adstock_weibull(spend, shape = 2, scale = 3, max_lag = 8,
                      type = "cdf"), 2)
#> [1] 36.80 32.93 21.11  7.77  1.31  0.08  0.00  0.00
```
