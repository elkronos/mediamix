# Steady-state starting value for an adstock filter

A filter started from zero assumes no media ran before the series began,
so the first periods of a long-carryover channel are understated: at
`decay = 0.85` a constant spend reaches only 15% of its steady-state
adstock in the first period. This function returns the state the filter
would hold had the average spend of the first `periods` been running
forever, which is the usual remedy when real pre-period spend is not
available.

## Usage

``` r
adstock_steady_state(x, decay, max_lag = Inf, periods = NULL, by = NULL)
```

## Arguments

- x:

  Numeric vector of media spend (or impressions, or GRPs) in time order.
  `x` must already be sorted by time and evenly spaced; the function has
  no index argument and cannot check this for you. Use
  [`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
  if you want the time index validated.

- decay:

  Geometric decay coefficient in `[0, 1]`. See
  [`decay_from_half_life()`](https://elkronos.github.io/mediamix/reference/decay_vocabulary.md)
  for the half-life vocabulary.

- max_lag:

  Number of periods the kernel spans. The default `Inf` uses the
  infinite (recursive) form; a finite value truncates the kernel. See
  *Details*.

- periods:

  Number of leading periods to average. Defaults to the geometric
  kernel's 90% effective window, capped at the series length, since that
  is how far back the start of the series "remembers".

- by:

  Optional grouping vector, or data frame of grouping vectors, the same
  length as `x`. Adstock is applied independently within each group,
  which is what geo-level and panel models need. Never rely on
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  for this: grouping metadata does not reliably survive into every
  context where this function is called.

## Value

A state in the form
[`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md),
[`adstock_filter()`](https://elkronos.github.io/mediamix/reference/adstock_filter.md)
and friends accept: a single raw accumulator for `max_lag = Inf`, the
`max_lag - 1` preceding values for a finite kernel, or a named list of
these when `by` is supplied.

## Details

For the recursive kernel the raw accumulator under constant spend \\m\\
is \\m / (1 - \theta)\\; for a finite kernel the preceding values are
all \\m\\. Either way a series that really was constant at \\m\\
adstocks to \\m\\ (normalised) from its first period.

The seed uses the series' own early *media*, never the KPI, so it does
not leak outcome information into a cross-validation. It does assume the
pre-period looked like the first few observed periods; when you have the
real pre-period spend, pass
[`adstock_state()`](https://elkronos.github.io/mediamix/reference/adstock_state.md)
of it instead.

## See also

[`adstock_state()`](https://elkronos.github.io/mediamix/reference/adstock_state.md),
[`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md),
[`tune_carryover()`](https://elkronos.github.io/mediamix/reference/tune_carryover.md),
which accepts `warm_start = TRUE`.

## Examples

``` r
x <- rep(100, 10)
round(adstock_geometric(x, decay = 0.85), 1)             # cold start
#>  [1] 15.0 27.8 38.6 47.8 55.6 62.3 67.9 72.8 76.8 80.3
s <- adstock_steady_state(x, decay = 0.85)
round(adstock_geometric(x, decay = 0.85, state = s), 1)  # no burn-in
#>  [1] 100 100 100 100 100 100 100 100 100 100

# Finite kernels and grouped series work the same way
adstock_steady_state(c(10, 20, 30, 40), decay = 0.5, max_lag = 3)
#> [1] 25 25
adstock_steady_state(c(10, 20, 100, 200), decay = 0.5,
                     by = c("a", "a", "b", "b"), periods = 2)
#> $a
#> [1] 30
#> 
#> $b
#> [1] 300
#> 
```
