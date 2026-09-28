# Saturation curves

Diminishing returns to media. Adstock says *when* money works;
saturation says *how hard* it works at the margin. Four curves are
provided; they differ in whether the response is bounded, and in whether
they allow an S-shape.

## Usage

``` r
saturate_hill(x, half_max, shape = 1)

saturate_exponential(x, rate)

saturate_michaelis_menten(x, vmax = 1, km)

saturate_power(x, exponent)

saturate(x, type = c("hill", "exponential", "michaelis_menten", "power"), ...)
```

## Arguments

- x:

  Numeric vector of media, normally already adstocked. Values must be
  non-negative: all four curves are defined for spend, not for arbitrary
  reals.

- half_max:

  The value of `x` at which the response reaches half its ceiling.
  Interpretable in the units of `x`, which is why it is used here in
  preference to the equivalent unnamed scale parameter.

- shape:

  Hill exponent, positive. `shape = 1` gives a concave curve with
  diminishing returns everywhere. `shape > 1` gives an S-shape with an
  initial convex region, the usual representation of a threshold effect.

- rate:

  Exponential rate parameter, positive. Larger values saturate sooner.

- vmax:

  Michaelis–Menten asymptote: the response as `x` grows without bound.

- km:

  Michaelis–Menten constant: the value of `x` at which the response
  reaches half of `vmax`.

- exponent:

  Power exponent in `(0, 1]`. `exponent = 1` is the identity (no
  saturation); smaller values bend the curve harder.

- type:

  Which curve `saturate()` should dispatch to.

- ...:

  Passed to the individual curve function.

## Value

A numeric vector the same length as `x`, in the same order.

## Details

`saturate_hill()` and `saturate_exponential()` are bounded on `[0, 1)`,
so the fitted coefficient carries the channel's ceiling.
`saturate_michaelis_menten()` is bounded by `vmax`. All three approach
their ceiling smoothly at arbitrarily large `x`, including `Inf`, rather
than overflowing. `saturate_power()` is unbounded but concave: response
keeps growing, just ever more slowly. Unboundedness is not automatically
wrong, but it does mean the model will happily extrapolate a return on a
spend level never observed.

`saturate_hill(x, half_max, shape = 1)` and
`saturate_michaelis_menten(x, vmax = 1, km = half_max)` are the same
function. Both are provided because the two literatures name it
differently and practitioners arrive expecting one or the other.

## Transform order

Saturation is applied *after* adstock, not before. Saturating first
would cap each period's spend in isolation and then let carryover
accumulate the capped values past the cap, which defeats the point of
having a ceiling. Use
[`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md)
to get the order right without having to remember it.

## References

Hill, A. V. (1910). The possible effects of the aggregation of the
molecules of haemoglobin on its dissociation curves. *The Journal of
Physiology*, 40(Suppl), iv–vii.

## See also

[`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md),
[`response_curve()`](https://elkronos.github.io/mediamix/reference/response_curve.md)

## Examples

``` r
spend <- c(0, 25, 50, 100, 200, 400)

# Concave: diminishing returns from the first pound
round(saturate_hill(spend, half_max = 100), 3)
#> [1] 0.000 0.200 0.333 0.500 0.667 0.800

# S-shaped: a threshold below which media barely registers
round(saturate_hill(spend, half_max = 100, shape = 3), 3)
#> [1] 0.000 0.015 0.111 0.500 0.889 0.985

# The four curves side by side. Hill, exponential and Michaelis-Menten are
# bounded; power keeps growing, just ever more slowly.
round(rbind(
  hill        = saturate_hill(spend, half_max = 100),
  exponential = saturate_exponential(spend, rate = 0.007),
  michaelis   = saturate_michaelis_menten(spend, vmax = 1, km = 100),
  power       = saturate_power(spend, exponent = 0.5)
), 3)
#>             [,1]  [,2]  [,3]   [,4]   [,5]   [,6]
#> hill           0 0.200 0.333  0.500  0.667  0.800
#> exponential    0 0.161 0.295  0.503  0.753  0.939
#> michaelis      0 0.200 0.333  0.500  0.667  0.800
#> power          0 5.000 7.071 10.000 14.142 20.000

# Bounded means bounded: no overflow, even at the extremes
saturate_hill(c(1e300, Inf), half_max = 100, shape = 3)
#> [1] 1 1

# The dispatcher, for programmatic use
round(saturate(spend, type = "hill", half_max = 100), 3)
#> [1] 0.000 0.200 0.333 0.500 0.667 0.800
```
