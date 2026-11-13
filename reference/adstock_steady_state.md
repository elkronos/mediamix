# Steady-state starting value for an adstock filter

A filter started from zero assumes no media ran before the series began,
so the first periods of a long-carryover channel are understated: at
`decay = 0.85` a constant spend reaches only 15% of its steady-state
adstock in the first period. This function returns the state the filter
would hold had the average spend of the first `periods` been running
forever. Use it when real pre-period spend is not available.

## Usage

``` r
adstock_steady_state(
  x,
  decay,
  max_lag = Inf,
  periods = NULL,
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
  if you want the time index validated. Infinite values are an error,
  since the filter would carry them into later periods; missing values
  are handled by `na_action`.

- decay:

  Geometric decay coefficient in `[0, 1]`. Required for the infinite
  kernel, and below 1 there, since an undecaying kernel has no steady
  state. A finite kernel's steady state does not depend on it, so there
  it is used only to choose the default `periods` and can be left out.

- max_lag:

  Number of periods the kernel spans. `Inf`, the default, gives the
  recursive form, whose state is a single number.

- periods:

  Number of leading periods to average, capped at the length of the
  series (of each group, with `by`). Defaults to the geometric kernel's
  90% effective window, `effective_window(decay, 0.9)`, since that is
  how far back the start of the series "remembers". For a finite
  `max_lag` the default is at most the `max_lag - 1` periods the
  kernel's state covers, and exactly that when `decay` is left out.

- by:

  Optional grouping vector, or data frame of grouping vectors, the same
  length as `x`. Adstock is applied independently within each group,
  which is what geo-level and panel models need. Never rely on
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  for this: grouping metadata does not reliably survive into every
  context where this function is called. Missing values are an error, in
  a vector or in any column of a data frame.

- na_action:

  What to do about missing values in `x`. `"error"` (the default)
  refuses to guess. `"zero"` treats missing media as no media, which is
  usually right for spend but is a substantive assumption. `"keep"` lets
  `NA` propagate through the filter, which for the recursive form
  poisons every subsequent value.

## Value

A state in the form
[`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md),
[`adstock_filter()`](https://elkronos.github.io/mediamix/reference/adstock_filter.md)
and friends accept: a single raw accumulator for `max_lag = Inf`, the
`max_lag - 1` preceding values for a finite kernel, or a named list of
these when `by` is supplied, in order of first appearance. These are
plain numeric vectors, so they carry no record of the kernel and are
checked only for their length and for missing or infinite values when
used.

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

Missing values follow `na_action`, as in
[`adstock_state()`](https://elkronos.github.io/mediamix/reference/adstock_state.md):
an error by default, zeros with `"zero"`, and with `"keep"` a missing
state when one falls in the averaged periods.

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
#> [1] 15 15
adstock_steady_state(c(10, 20, 100, 200), decay = 0.5,
                     by = c("a", "a", "b", "b"), periods = 2)
#> $a
#> [1] 30
#> 
#> $b
#> [1] 300
#> 
```
