# Geometric adstock

Spreads each period's media forward in time with geometric decay, the
standard representation of advertising carryover. This is the
forward-facing half of the package's spine;
[`credit_time_decay()`](https://elkronos.github.io/mediamix/reference/credit.md)
is the same kernel run backward.

## Usage

``` r
adstock_geometric(
  x,
  decay,
  max_lag = Inf,
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

- decay:

  Geometric decay coefficient in `[0, 1]`. See
  [`decay_from_half_life()`](https://elkronos.github.io/mediamix/reference/decay_vocabulary.md)
  for the half-life vocabulary.

- max_lag:

  Number of periods the kernel spans. The default `Inf` uses the
  infinite (recursive) form; a finite value truncates the kernel. See
  *Details*.

- normalise:

  Should the kernel sum to 1? Defaults to `TRUE`. See the
  *Normalisation* section.

- state:

  Carryover already in flight at the start of `x`, for chaining calls
  across contiguous chunks of a series. The default `0` is a cold start,
  meaning no media ran before `x` began. For the infinite kernel this is
  a single number; for a finite `max_lag` it is the preceding
  `max_lag - 1` values of `x`. Obtain it from
  [`adstock_state()`](https://elkronos.github.io/mediamix/reference/adstock_state.md).

  When `by` is supplied, pass either the scalar `0` (or `NULL`) to
  cold-start every group, or a named list with one entry per group –
  which is the shape
  [`adstock_state()`](https://elkronos.github.io/mediamix/reference/adstock_state.md)
  returns when it is given `by`. A list with a `NULL` entry is an error
  rather than a silent cold start for that group.

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

A numeric vector the same length as `x`, in the same order. Row count
and order are preserved unconditionally.

## Normalisation

This is the argument that most often changes an answer without anyone
noticing, so it is explicit rather than implied.

With `normalise = TRUE` the kernel weights sum to 1. A constant spend of
\\c\\ adstocks to \\c\\, the series level is preserved, and a downstream
regression coefficient reads as the effect of *one unit of media*,
directly comparable to the coefficient on untransformed spend.

With `normalise = FALSE` you get raw Koyck accumulation, \\a_t = x_t +
\theta a\_{t-1}\\. A constant spend of \\c\\ accumulates to
\\c/(1-\theta)\\, so the series level rises with the decay rate, and the
coefficient reads as the effect of one unit of *accumulated* media. Both
are defensible; comparing a coefficient fitted one way against a
coefficient fitted the other is not.

Both conventions are in use. Robyn's geometric adstock is the
unnormalised recursion; the Bayesian MMM of Jin et al. (2017) normalises
its kernel weights to sum to 1. This package defaults to normalised
because it keeps the coefficient interpretable and keeps decay and
coefficient magnitude from trading off against each other during
fitting.

## Infinite versus truncated kernels

`max_lag = Inf` runs the recursive filter, an infinite impulse response.
Its entire memory is one number per series, which is what makes
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
able to warm-start across a train/test boundary without storing any
training rows.

A finite `max_lag` truncates to a finite impulse response, which needs
the preceding `max_lag - 1` observations as state. Truncation is a
modelling choice, not an approximation to be minimised: use
[`effective_window()`](https://elkronos.github.io/mediamix/reference/decay_vocabulary.md)
to see how much of the kernel a given `max_lag` retains.

Note that `decay = 1` with `normalise = TRUE` and `max_lag = Inf` is an
error, not an edge case: an undecaying infinite kernel has infinite mass
and cannot be normalised.

## Causality

The filter is strictly causal. `adstock_geometric(x)[1:m]` is identical
to `adstock_geometric(x[1:m])` for every `m`, so no future spend can
leak into a past adstock value. This is what makes it safe to adstock a
full series once and then slice it into cross-validation folds, as
[`tune_carryover()`](https://elkronos.github.io/mediamix/reference/tune_carryover.md)
does.

## References

Broadbent, S. (1979). One way TV advertisements work. *Journal of the
Market Research Society*, 21(3), 139–166.

Jin, Y., Wang, Y., Sun, Y., Chan, D. and Koehler, J. (2017). Bayesian
methods for media mix modeling with carryover and shape effects. Google
Inc.

## See also

[`adstock_weibull()`](https://elkronos.github.io/mediamix/reference/adstock_weibull.md)
for delayed peaks,
[`adstock_state()`](https://elkronos.github.io/mediamix/reference/adstock_state.md)
for chaining,
[`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md)
to compose with saturation,
[`credit_time_decay()`](https://elkronos.github.io/mediamix/reference/credit.md)
for the mirrored backward kernel.

## Examples

``` r
spend <- c(100, 0, 0, 0, 0, 0)

# A single burst decaying forward through time
round(adstock_geometric(spend, decay = 0.5), 2)
#> [1] 50.00 25.00 12.50  6.25  3.12  1.56

# Normalisation preserves the level of a constant series. The equality is
# asymptotic: from a cold start the filter needs a few periods to fill up,
# so take a value well after the burn-in.
constant <- rep(100, 100)
tail(adstock_geometric(constant, decay = 0.8), 1)                     # 100
#> [1] 100
tail(adstock_geometric(constant, decay = 0.8, normalise = FALSE), 1)  # 500
#> [1] 500

# Half-life vocabulary
adstock_geometric(spend, decay = decay_from_half_life(2))
#> [1] 29.28932 20.71068 14.64466 10.35534  7.32233  5.17767

# Dark weeks: carryover decays across a flight gap rather than resetting
flighted <- c(100, 100, 0, 0, 0, 0, 100)
round(adstock_geometric(flighted, decay = 0.6), 2)
#> [1] 40.00 64.00 38.40 23.04 13.82  8.29 44.98

# Geo-level panel data
spend_panel <- c(100, 50, 25, 200, 100, 50)
geo <- c("north", "north", "north", "south", "south", "south")
round(adstock_geometric(spend_panel, decay = 0.5, by = geo), 2)
#> [1]  50.0  50.0  37.5 100.0 100.0  75.0
```
