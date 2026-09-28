# Response curve for a channel

Predicted response across a range of spend, derived from the fitted
saturation parameters. `spend_for()` is the inverse: the spend that
achieves a target response.

## Usage

``` r
response_curve(spend, coefficient, type = "hill", ...)

spend_for(target, coefficient, type = "hill", max_spend = NULL, ...)
```

## Arguments

- spend:

  Numeric vector of spend levels to evaluate. `response_curve()` only;
  `spend_for()` has no such argument.

- coefficient:

  The channel's fitted coefficient.

- type:

  Saturation curve type, as in
  [`saturate()`](https://elkronos.github.io/mediamix/reference/saturation.md).

- ...:

  Passed to
  [`saturate()`](https://elkronos.github.io/mediamix/reference/saturation.md),
  for example `half_max` and `shape`.

- target:

  Target response level. `spend_for()` only.

- max_spend:

  Upper bound of the search interval for `spend_for()`. Defaults to 1000
  times the curve's spend-scaled parameter (`half_max`, `km`, or
  `log(2)/rate`).
  [`saturate_power()`](https://elkronos.github.io/mediamix/reference/saturation.md)
  has no such parameter, so `max_spend` is required there.

## Value

`response_curve()` returns a data frame with columns `spend`, `response`
and `marginal`, one row per element of `spend` and in the same order.
`spend_for()` returns a single number, or `NA_real_` with a warning when
the target is not reachable within `max_spend`.

## Details

These describe the *response curve the model fitted*, which is not the
same thing as what would happen if you actually spent that much. The
curve is identified only over the range of spend the data contains;
asking a saturated Hill curve what happens at ten times the observed
maximum returns a number, and that number is extrapolation.
`spend_for()` returns `NA` rather than a fabricated answer when the
target lies beyond `max_spend`, but it cannot tell you that a reachable
target is outside the data's support. Check the observed spend range
yourself.

## See also

[`saturate()`](https://elkronos.github.io/mediamix/reference/saturation.md),
[`mroi()`](https://elkronos.github.io/mediamix/reference/roi.md)

## Examples

``` r
# A concave curve: marginal return falls from the first pound onward.
concave <- response_curve(
  spend = seq(0, 100000, by = 20000),
  coefficient = 5200, half_max = 45000, shape = 1
)
concave
#>   spend response   marginal
#> 1 0e+00    0.000 0.11555556
#> 2 2e+04 1600.000 0.05538475
#> 3 4e+04 2447.059 0.03238772
#> 4 6e+04 2971.429 0.02122466
#> 5 8e+04 3328.000 0.01497615
#> 6 1e+05 3586.207 0.01112974
all(diff(concave$marginal) < 0)
#> [1] TRUE

# An S-curve: marginal return RISES to the inflection point and only then
# falls. Below the peak the channel is under-funded, not saturated.
s_curve <- response_curve(
  spend = seq(0, 100000, by = 20000),
  coefficient = 5200, half_max = 45000, shape = 1.6
)
s_curve
#>   spend response     marginal
#> 1 0e+00    0.000 5.523462e-07
#> 2 2e+04 1115.858 7.011242e-02
#> 3 4e+04 2355.734 5.154110e-02
#> 4 6e+04 3188.033 3.289370e-02
#> 5 8e+04 3718.836 2.118571e-02
#> 6 1e+05 4066.624 1.418186e-02
s_curve$spend[which.max(s_curve$marginal)]
#> [1] 20000

# What spend achieves a response of 2000?
spend_for(target = 2000, coefficient = 5200, half_max = 45000, shape = 1.6)
#> [1] 33545.75

# An unbounded curve needs an explicit search range
spend_for(target = 200000, coefficient = 5200, type = "power",
          exponent = 0.5, max_spend = 1e6)
#> [1] 1479.29
```
