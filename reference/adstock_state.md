# Terminal adstock state

Returns the carryover left in flight at the end of a series, in a form
[`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md)
and friends accept as their `state` argument. This is how you continue a
filter across contiguous chunks of a series without re-running it from
the beginning.
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
computes it at `prep()` time and reports it through `tidy()`.

## Usage

``` r
adstock_state(
  x,
  decay,
  max_lag = Inf,
  state = 0,
  by = NULL,
  na_action = c("error", "zero", "keep"),
  missing_state = c("error", "cold")
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
  kernel. A finite kernel's state is the tail of `x` and does not depend
  on the decay, so there `decay` can be left out.

- max_lag:

  Number of periods the kernel spans. `Inf`, the default, gives the
  recursive form, whose state is a single number.

- state:

  Carryover already in flight at the start of `x`, for chaining calls
  across contiguous chunks of a series. The default `0` is a cold start,
  meaning no media ran before `x` began. For the infinite kernel this is
  a single number, the raw (unnormalised) accumulator; for a finite
  `max_lag` it is the `max_lag - 1` values that came before `x`, oldest
  first. Obtain it from `adstock_state()`, which records the `max_lag`
  (and, for the infinite kernel, the `decay`) it was computed for, so
  that a state from another kernel is refused rather than used. A plain
  numeric vector of the right length is accepted too.

  When `by` is supplied, pass either the scalar `0` (or `NULL`) to
  cold-start every group, or a named list with one entry per group,
  which is the shape `adstock_state()` returns when it is given `by`. A
  list with a `NULL` entry is an error rather than a silent cold start
  for that group.

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

- missing_state:

  What to do when `state` is a per-group list with no entry for a group
  in `by`. `"error"` (the default) refuses; `"cold"` cold-starts those
  groups, as
  [`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
  does by default for groups it did not see at `prep()` time.

## Value

An object of class `mm_adstock_state`, a numeric vector that records
what it was computed for. For the infinite kernel it holds a single
number, the raw (unnormalised) accumulator, and records the kernel,
`max_lag` and `decay`. For a finite `max_lag` it holds the last
`max_lag - 1` values of the series, oldest first, and records `max_lag`;
when `x` is shorter than that, the earlier values come from `state`, or
are zeros from a cold start. When `by` is supplied, a named list of such
objects, one per group, in order of first appearance.

## Details

The stored state is always in *raw* units, independent of `normalise`,
so a state captured under one normalisation setting stays valid under
the other.

The transforms check a state against their own arguments and refuse one
computed for another kernel, instead of starting from the wrong
carryover. The recursive accumulator is valid only for the same `decay`
and `max_lag = Inf`. A finite state is the raw tail of the series, so it
serves every kernel with the same `max_lag`:
[`adstock_weibull()`](https://elkronos.github.io/mediamix/reference/adstock_weibull.md),
[`adstock_delayed()`](https://elkronos.github.io/mediamix/reference/adstock_delayed.md)
and
[`adstock_filter()`](https://elkronos.github.io/mediamix/reference/adstock_filter.md)
accept it as readily as
[`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md).
A plain numeric vector is still accepted and is checked only for its
length and for missing or infinite values.

For the recursive kernel this object is one double per series, so
continuing the filter needs none of the earlier observations.
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
stores the training media as well, so that `bake()` can also take new
data that repeats training periods; see its *Storage* section.

## See also

[`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md),
[`adstock_weibull()`](https://elkronos.github.io/mediamix/reference/adstock_weibull.md),
[`adstock_filter()`](https://elkronos.github.io/mediamix/reference/adstock_filter.md),
and
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md),
which reports this state through `tidy()`.

## Examples

``` r
first_half <- c(100, 80, 60, 40)
second_half <- c(20, 10, 5, 0)

s <- adstock_state(first_half, decay = 0.5)
s
#> <adstock state: geometric accumulator at decay 0.5>
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

# A state from another kernel is refused
try(adstock_geometric(second_half, decay = 0.5, max_lag = 2, state = s))
#> Error in adstock_geometric(second_half, decay = 0.5, max_lag = 2, state = s) : 
#>   `state` was computed for a different kernel length.
#> ✖ It is the state of a kernel with `max_lag = Inf`; this call uses `max_lag =
#>   2`.
#> ℹ Recompute it with `adstock_state()` and the same `max_lag`.
```
