# Carryover decay vocabulary

Practitioners reason about media carryover in half-lives ("television
keeps working for about three weeks"), while the arithmetic needs a
decay coefficient. These three functions translate between the two and
are shared by both halves of the package: `decay` means the same thing
in
[`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md),
which spreads one impulse of spend *forward* through time, and in
[`credit_time_decay()`](https://elkronos.github.io/mediamix/reference/credit.md),
which spreads one conversion's credit *backward* across prior
touchpoints.

## Usage

``` r
decay_from_half_life(half_life, period = 1)

half_life(decay, period = 1)

effective_window(decay, coverage = 0.9)
```

## Arguments

- half_life:

  Number of periods over which effect falls to half. Must be positive.

- period:

  Spacing between observations, expressed in the same time unit as
  `half_life`. The default `1` means `half_life` is already measured in
  periods, so `decay_from_half_life(3)` reads as "a three-period
  half-life". Supply both in days (say) to mix units:
  `decay_from_half_life(21, period = 7)` is a 21-day half-life observed
  weekly, and gives the same answer.

- decay:

  Geometric decay coefficient. A value of `0` means no carryover; values
  approaching `1` mean effect persists almost indefinitely.
  `effective_window()` accepts `[0, 1)`. `half_life()` requires
  `(0, 1)`, since a decay of exactly `0` has no half-life to report –
  the effect is gone before the next period.

- coverage:

  Proportion of the total carryover effect the window should contain, in
  `(0, 1)`.

## Value

A single number. `decay_from_half_life()` returns a decay coefficient,
`half_life()` returns a number of periods, and `effective_window()`
returns an integer number of periods.

## Details

The geometric kernel places weight \\\theta^i\\ on lag \\i\\, so the
effect halves after \\h\\ periods when \\\theta^h = 0.5\\. Hence
\\\theta = 0.5^{p/h}\\ and \\h = p \log(0.5) / \log(\theta)\\.

`effective_window()` returns the smallest \\n\\ for which the first
\\n\\ lags carry at least `coverage` of the infinite kernel's total
mass, that is the smallest \\n\\ with \\1 - \theta^n \ge\\ `coverage`.
It is the honest way to choose `max_lag` for a truncated kernel, and a
useful sanity check on a fitted decay: a decay implying a 40-week
effective window on 104 weeks of data is not identified by the data.

## See also

[`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md),
which spreads spend forward with this decay, and
[`credit_time_decay()`](https://elkronos.github.io/mediamix/reference/credit.md),
which spreads credit backward with the same one.
[`vignette("spine")`](https://elkronos.github.io/mediamix/articles/spine.md)
shows they are the same kernel.

## Examples

``` r
# A three-week half-life on weekly data
theta <- decay_from_half_life(3)
theta
#> [1] 0.7937005

# Round trip
half_life(theta)
#> [1] 3

# The same half-life stated in days, observed weekly
decay_from_half_life(21, period = 7)
#> [1] 0.7937005

# How many periods to keep before truncating loses 10% of the effect?
effective_window(theta, coverage = 0.90)
#> [1] 10

# The vocabulary is shared by both halves of the package
adstock_geometric(c(100, 0, 0, 0, 0), decay = theta)
#> [1] 20.62995 16.37400 12.99605 10.31497  8.18700
```
