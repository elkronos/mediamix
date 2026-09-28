# Terminal adstock state

Returns the carryover left in flight at the end of a series, in a form
[`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md)
and friends accept as their `state` argument. This is how you continue a
filter across contiguous chunks of a series without re-running it from
the beginning, and it is what
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
stores at `prep()` time.

## Usage

``` r
adstock_state(
  x,
  decay,
  max_lag = Inf,
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

- decay:

  Geometric decay coefficient in `[0, 1]`. See
  [`decay_from_half_life()`](https://elkronos.github.io/mediamix/reference/decay_vocabulary.md)
  for the half-life vocabulary.

- max_lag:

  Number of periods the kernel spans. `Inf` gives the recursive form,
  whose state is a single number.

- state:

  Carryover already in flight at the start of `x`, for chaining calls
  across contiguous chunks of a series. The default `0` is a cold start,
  meaning no media ran before `x` began. For the infinite kernel this is
  a single number; for a finite `max_lag` it is the preceding
  `max_lag - 1` values of `x`. Obtain it from `adstock_state()`.

  When `by` is supplied, pass either the scalar `0` (or `NULL`) to
  cold-start every group, or a named list with one entry per group –
  which is the shape `adstock_state()` returns when it is given `by`. A
  list with a `NULL` entry is an error rather than a silent cold start
  for that group.

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

For the infinite kernel, a single number: the raw (unnormalised)
accumulator. For a finite `max_lag`, the last `max_lag - 1` values of
`x`, oldest first. When `by` is supplied, a named list of such objects,
one per group.

## Details

The stored state is always in *raw* units, independent of `normalise`,
so a state captured under one normalisation setting stays valid under
the other.

The size of this object is the reason
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
can survive a train/test boundary without the leakage and row-count
problems that
[`recipes::step_lag()`](https://recipes.tidymodels.org/reference/step_lag.html)
runs into. For the recursive kernel it is one double per series: no
training observations are retained, so there is nothing to leak and
nothing to inflate the size of a fitted workflow.

## See also

[`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md),
[`adstock_weibull()`](https://elkronos.github.io/mediamix/reference/adstock_weibull.md),
[`adstock_filter()`](https://elkronos.github.io/mediamix/reference/adstock_filter.md),
and
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md),
which stores this state at `prep()` time.

## Examples

``` r
first_half <- c(100, 80, 60, 40)
second_half <- c(20, 10, 5, 0)

s <- adstock_state(first_half, decay = 0.5)
s
#> [1] 102.5

# Filtering in two chunks with the state carried across gives exactly the
# same answer as filtering the whole series at once
chunked <- c(
  adstock_geometric(first_half, decay = 0.5),
  adstock_geometric(second_half, decay = 0.5, state = s)
)
whole <- adstock_geometric(c(first_half, second_half), decay = 0.5)
all.equal(chunked, whole)
#> [1] TRUE
```
