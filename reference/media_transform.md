# Adstock then saturate, in that order

Composes a carryover transform with a saturation curve to produce a
single model-ready regressor. The point of the function is the *order*:
adstock first, saturation second.

## Usage

``` r
media_transform(
  x,
  adstock = list(kernel = "geometric", decay = 0.5),
  saturation = list(type = "none"),
  by = NULL,
  order = c("adstock_then_saturate", "saturate_then_adstock")
)
```

## Arguments

- x:

  Numeric vector of media spend in time order.

- adstock:

  A named list of arguments for the carryover step, including a `kernel`
  element of `"geometric"` (the default), `"weibull"`, `"delayed"` or
  `"none"`. Remaining elements are passed to
  [`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md),
  [`adstock_weibull()`](https://elkronos.github.io/mediamix/reference/adstock_weibull.md)
  or
  [`adstock_delayed()`](https://elkronos.github.io/mediamix/reference/adstock_delayed.md).

- saturation:

  A named list of arguments for the saturation step, including a `type`
  element of `"hill"`, `"exponential"`, `"michaelis_menten"`, `"power"`
  or `"none"`. Remaining elements are passed to the corresponding
  `saturate_*()` function – for `"hill"` that means `half_max`, which
  has no default because a sensible value depends entirely on the scale
  of your spend.

  Saturation defaults to `"none"`, so calling `media_transform()` with
  only an `adstock` argument applies carryover alone.

- by:

  Optional grouping vector, or data frame of grouping vectors, the same
  length as `x`.

- order:

  Transform order. `"adstock_then_saturate"` is the convention and the
  default. `"saturate_then_adstock"` is permitted but warns, because it
  is nearly always a mistake rather than a choice.

## Value

A numeric vector the same length as `x`, in the same order.

## Details

The order is not arbitrary. Saturation represents a ceiling on what a
given weight of media can achieve in a period. Applying it before
adstock caps each period's spend in isolation and then lets carryover
accumulate those capped values past the cap, so the composed transform
is no longer bounded by the ceiling you specified. Applying it after
adstock caps the *total media pressure* in each period, which is what a
saturation curve is meant to mean.

This function will do it backwards if you insist, but it will not do it
backwards quietly.

## See also

[`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md),
[`saturate()`](https://elkronos.github.io/mediamix/reference/saturation.md)

## Examples

``` r
spend <- c(0, 500, 800, 300, 0, 0, 1200, 400)

# Carryover only -- saturation is off by default
round(media_transform(spend, adstock = list(decay = 0.5)), 4)
#> [1]   0.0000 250.0000 525.0000 412.5000 206.2500 103.1250 651.5625 525.7812

# Carryover and saturation together, in the conventional order
round(media_transform(
  spend,
  adstock = list(decay = decay_from_half_life(2)),
  saturation = list(half_max = 300)
), 4)
#> [1] 0.0000 0.3280 0.5297 0.5214 0.4351 0.3526 0.6089 0.5986

# A delayed-peak kernel with an S-shaped response
round(media_transform(
  spend,
  adstock = list(kernel = "weibull", shape = 2, scale = 2, max_lag = 6,
                 type = "pdf"),
  saturation = list(type = "hill", half_max = 300, shape = 2)
), 4)
#> [1] 0.0000 0.3149 0.7485 0.7445 0.4412 0.0706 0.7383 0.8131

# Grouped: geo-level panels transform within each geography
spend_panel <- c(100, 50, 25, 200, 100, 50)
geo <- rep(c("north", "south"), each = 3)
round(media_transform(spend_panel, adstock = list(decay = 0.5),
                      by = geo), 4)
#> [1]  50.0  50.0  37.5 100.0 100.0  75.0
```
