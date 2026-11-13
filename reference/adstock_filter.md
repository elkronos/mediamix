# Adstock with an arbitrary kernel

Applies a user-supplied weight vector as a causal filter. Use this when
you have a kernel from somewhere else (an econometric study, a vendor's
published curve, a shape you fitted yourself) and want the same state
handling, grouping and length guarantees as the built-in transforms.

## Usage

``` r
adstock_filter(
  x,
  weights,
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

- weights:

  Numeric vector of kernel weights, ordered from the current period to
  the most distant lag. Not rescaled: whatever you supply is what is
  applied. Missing or infinite weights are an error.

- state:

  The `length(weights) - 1` values that came before `x`, oldest first,
  as `utils::tail(x_previous, length(weights) - 1)` gives them, or `0`
  for a cold start. When `by` is supplied, a named list with one entry
  per group.

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

A numeric vector the same length as `x`, in the same order, with the
names and time-series attributes of `x`.

## See also

[`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md),
[`adstock_weibull()`](https://elkronos.github.io/mediamix/reference/adstock_weibull.md)
and
[`adstock_delayed()`](https://elkronos.github.io/mediamix/reference/adstock_delayed.md)
for the built-in kernels;
[`adstock_weights()`](https://elkronos.github.io/mediamix/reference/adstock_weights.md),
[`adstock_weights_weibull()`](https://elkronos.github.io/mediamix/reference/adstock_weights_weibull.md)
and
[`adstock_weights_delayed()`](https://elkronos.github.io/mediamix/reference/adstock_delayed.md)
to build a kernel to pass here;
[`adstock_state()`](https://elkronos.github.io/mediamix/reference/adstock_state.md)
for chaining.

## Examples

``` r
# An explicitly humped kernel: the peak lands one period after the spend
adstock_filter(c(100, 0, 0, 0, 0), weights = c(0.2, 0.5, 0.3))
#> [1] 20 50 30  0  0

# Weights are applied as supplied, never rescaled. These sum to 2, and the
# output level doubles accordingly.
adstock_filter(c(100, 100, 100, 100), weights = c(1, 1))
#> [1] 100 200 200 200

# Chaining across chunks, and grouping, work as they do for the built-in
# kernels. The state is the tail of the earlier chunk, as here, or
# `adstock_state()` with a matching `max_lag`.
w <- c(0.5, 0.3, 0.2)
whole <- adstock_filter(c(100, 80, 60, 40, 20), weights = w)
s <- utils::tail(c(100, 80, 60), length(w) - 1L)
chunked <- c(adstock_filter(c(100, 80, 60), weights = w),
             adstock_filter(c(40, 20), weights = w, state = s))
all.equal(chunked, whole)
#> [1] TRUE
```
