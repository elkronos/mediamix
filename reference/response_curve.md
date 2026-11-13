# Response curve for a channel

Predicted response across a range of adstocked spend, derived from the
fitted saturation parameters. `spend_for()` is the inverse: the
adstocked spend that achieves a target response.

## Usage

``` r
response_curve(spend, coefficient, type = "hill", ...)

spend_for(target, coefficient, type = "hill", max_spend = NULL, ...)
```

## Arguments

- spend:

  Numeric vector of adstocked spend levels to evaluate, in the units the
  saturation curve was fitted on (see *Details*), finite and
  non-negative. `response_curve()` only; `spend_for()` has no such
  argument.

- coefficient:

  The channel's fitted coefficient.

- type:

  Saturation curve type, as in
  [`saturate()`](https://elkronos.github.io/mediamix/reference/saturation.md),
  or `"none"` for a linear response.

- ...:

  Passed to
  [`saturate()`](https://elkronos.github.io/mediamix/reference/saturation.md),
  for example `half_max` and `shape`.

- target:

  Target response level. `spend_for()` only. It must lie on the side of
  the response at zero spend that spending moves towards: above it for a
  positive coefficient, below it for a negative one.

- max_spend:

  Upper bound of the search interval for `spend_for()`. Defaults to 1000
  times the curve's spend-scaled parameter (`half_max`, `km`, or
  `log(2)/rate`).
  [`saturate_power()`](https://elkronos.github.io/mediamix/reference/saturation.md)
  and `"none"` have no such parameter, so `max_spend` is required there.

## Value

`response_curve()` returns a data frame with columns `spend` (the levels
given), `response` (the coefficient times the curve at each level) and
`marginal` (the slope of `response` there, as
[`mroi()`](https://elkronos.github.io/mediamix/reference/roi.md)
computes it), one row per element of `spend` and in the same order. It
has class `mm_response_curve`, with a
[plot()](https://elkronos.github.io/mediamix/reference/plot.mm_response_curve.md)
method. `spend_for()` returns a single adstocked spend level, or
`NA_real_` with a warning when the target is not reachable within
`max_spend`. A target that no amount of spend can reach, such as a
negative response from a channel with a positive coefficient, is an
error.

## Details

`spend` is the saturation curve's input. When the regressor came from
[`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md),
that is adstocked spend: the media pressure in a period, carryover
included, rather than the spend of that period alone. Under a normalised
kernel, the default, a steady spend per period settles at the same
adstocked level, so a level can be read as the steady spend per period
that holds the channel there.

Both functions describe the *response curve the model fitted*, which is
not the same thing as what would happen if you actually spent that much.
The curve is identified only over the range of spend the data contains;
asking a saturated Hill curve what happens at ten times the observed
maximum returns a number, and that number is extrapolation.
`spend_for()` returns `NA` rather than a fabricated answer when the
target lies beyond `max_spend`, but it cannot tell you that a reachable
target is outside the data's support. Check the range of the adstocked
series yourself;
[plot()](https://elkronos.github.io/mediamix/reference/plot.mm_response_curve.md)
shades the levels outside it when given the series as `observed`.

## See also

[`plot.mm_response_curve()`](https://elkronos.github.io/mediamix/reference/plot.mm_response_curve.md),
[`saturate()`](https://elkronos.github.io/mediamix/reference/saturation.md),
[`mroi()`](https://elkronos.github.io/mediamix/reference/roi.md), and
[`marginal_roi()`](https://elkronos.github.io/mediamix/reference/marginal_roi.md)
for the marginal return on raw spend through the full transform

## Examples

``` r
# The curve mm_weekly's television was simulated with: coefficient 5200,
# half_max 2250 and shape 1.6, on adstocked weekly spend.
data(mm_weekly)
truth <- attr(mm_weekly, "truth")
c(truth$beta[["tv"]], truth$half_max[["tv"]], truth$shape[["tv"]])
#> [1] 5200.0 2250.0    1.6

# A concave curve: marginal return falls from the first pound onward.
concave <- response_curve(
  spend = seq(0, 5000, by = 1000),
  coefficient = 5200, half_max = 2250, shape = 1
)
concave
#>   spend response  marginal
#> 1     0    0.000 2.3111041
#> 2  1000 1600.000 1.1076949
#> 3  2000 2447.059 0.6477545
#> 4  3000 2971.429 0.4244933
#> 5  4000 3328.000 0.2995231
#> 6  5000 3586.207 0.2225948
all(diff(concave$marginal) < 0)
#> [1] TRUE

# An S-curve: marginal return rises up to the curve's steepest point, near
# 900 here, and falls after it. Below that point each extra pound returns
# more than the one before, so the curve is not yet saturated there; whether
# the channel deserves more budget depends on how its marginal return
# compares with other channels' and with break-even.
s_curve <- response_curve(
  spend = seq(0, 5000, by = 500),
  coefficient = 5200, half_max = 2250, shape = 1.6
)
s_curve
#>    spend  response    marginal
#> 1      0    0.0000 0.001128472
#> 2    500  429.9182 1.261993634
#> 3   1000 1115.8577 1.402248359
#> 4   1500 1785.0175 1.250419105
#> 5   2000 2355.7338 1.030821918
#> 6   2500 2818.6324 0.826120871
#> 7   3000 3188.0331 0.657873910
#> 8   3500 3482.5595 0.525816116
#> 9   4000 3718.8362 0.423714294
#> 10  4500 3910.1361 0.344863577
#> 11  5000 4066.6239 0.283637139
s_curve$spend[which.max(s_curve$marginal)]
#> [1] 1000

# What adstocked spend achieves a response of 2000?
spend_for(target = 2000, coefficient = 5200, half_max = 2250, shape = 1.6)
#> [1] 1677.288

# An unbounded curve needs an explicit search range
spend_for(target = 200000, coefficient = 5200, type = "power",
          exponent = 0.5, max_spend = 1e6)
#> [1] 1479.29
```
