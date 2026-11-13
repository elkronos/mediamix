# Convert Robyn hyperparameters to mediamix arguments

Translates one channel's adstock and saturation hyperparameters from a
Robyn model into the `adstock` and `saturation` lists that
[`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md)
takes, so that mediamix reproduces the series Robyn fitted its model on.
Geometric adstock and Robyn's Weibull CDF convert exactly. Hill `alpha`
and `gamma` convert exactly given the media series, because Robyn places
the inflexion point relative to the adstocked series.

## Usage

``` r
from_robyn(
  x = NULL,
  adstock = c("geometric", "weibull_cdf", "weibull_pdf"),
  theta = NULL,
  shape = NULL,
  scale = NULL,
  alpha = NULL,
  gamma = NULL,
  window = NULL,
  inflexion = c("max", "range")
)
```

## Arguments

- x:

  The media series Robyn transformed for this channel: its column of
  `InputCollect$dt_mod`, every row, in date order. Values must be
  finite, non-missing and non-negative. Needed to convert `gamma` and
  Weibull adstock; a geometric `theta` converts without it.

- adstock:

  Robyn's adstock type, as in `InputCollect$adstock`: one of
  `"geometric"`, `"weibull_cdf"` or `"weibull_pdf"`. `"weibull_pdf"` has
  no exact equivalent and is refused; see *Details*.

- theta:

  Robyn's geometric decay rate (the `_thetas` hyperparameter), in
  `[0, 1)`. Required for geometric adstock.

- shape, scale:

  Robyn's Weibull shape and scale (the `_shapes` and `_scales`
  hyperparameters), with `shape >= 0` and `scale` in `[0, 1]`. Required
  for Weibull adstock. These are adstock parameters: the Hill shape is
  `alpha`.

- alpha, gamma:

  Robyn's Hill exponent and inflexion share (the `_alphas` and `_gammas`
  hyperparameters), with `alpha > 0` and `gamma` in `(0, 1]`. Give both
  or neither. Without them the saturation step is `"none"`.

- window:

  Row positions of `x` inside Robyn's modelling window,
  `InputCollect$rollingWindowStartWhich:InputCollect$rollingWindowEndWhich`.
  The default `NULL` uses every row. Used only to convert `gamma`.

- inflexion:

  How the Robyn version that fitted the model placed the Hill inflexion
  point: `"max"` (the default) for Robyn 3.12.0 and later, `"range"` for
  Robyn 3.7.0 to 3.11.1. Robyn 3.0.0 to 3.6.3 also used the range point,
  rounded to four decimal places, so `"range"` matches those versions up
  to that rounding. Used only to convert `gamma`.

## Value

A list with two elements, `adstock` and `saturation`, to pass to the
arguments of the same names in
[`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md):

- `adstock`:

  `list(kernel = "geometric", decay, normalise = FALSE)` for geometric
  adstock;
  `list(kernel = "weibull", shape, scale, max_lag, type = "cdf", normalise = FALSE)`
  for the Weibull CDF, with `scale` in periods and `max_lag` equal to
  `length(x)`; or `list(kernel = "none")` when the Weibull `shape` or
  `scale` is 0.

- `saturation`:

  `list(type = "hill", half_max, shape)`, where `half_max` is Robyn's
  inflexion point in the units of the adstocked media and `shape` is
  `alpha`; or `list(type = "none")` when `alpha` and `gamma` are not
  given.

## Details

Each conversion follows Robyn's transformation code, in the file
`R/R/transformation.R` of its repository.

**Geometric.** Robyn accumulates \\a_t = x_t + \theta a\_{t-1}\\ from
\\a_1 = x_1\\, the unnormalised recursion. That is
[`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md)
with `decay = theta` and `normalise = FALSE`.

**Weibull CDF.** Robyn converts `scale` to periods as
`round(quantile(1:n, scale))`, where `n` is the length of the series it
adstocks, and uses a kernel of `n` lags whose first weight is 1. That is
[`adstock_weibull()`](https://elkronos.github.io/mediamix/reference/adstock_weibull.md)
with `type = "cdf"`, `max_lag = n`, the rounded scale and
`normalise = FALSE`. The number of periods depends on `n`, which is why
the conversion needs `x`: the same `scale` fitted on a longer series
means a longer carryover. From Robyn 3.10.0, a `shape` or `scale` of 0
switches the carryover off, and the conversion returns
`kernel = "none"`.

**Weibull PDF.** Robyn rescales the Weibull density over its `n` lags so
that the smallest weight is 0 and the largest is 1.
[`adstock_weibull()`](https://elkronos.github.io/mediamix/reference/adstock_weibull.md)
divides by the largest weight, or by the sum of the weights, and keeps
the smallest above 0, so no
[`adstock_weibull()`](https://elkronos.github.io/mediamix/reference/adstock_weibull.md)
arguments reproduce Robyn's series, and `from_robyn()` refuses rather
than return an approximation. The gap grows with the ratio of the
smallest density value to the largest: it is negligible when the density
dies out well inside the series, and not when the tail is heavy (`shape`
below 1) or `scale` is long.
[`adstock_filter()`](https://elkronos.github.io/mediamix/reference/adstock_filter.md)
applies any kernel given as weights, Robyn's included.

**Hill.** Robyn's curve is \\a^\alpha / (a^\alpha + g^\alpha)\\, which
is
[`saturate_hill()`](https://elkronos.github.io/mediamix/reference/saturation.md)
with `shape = alpha` and `half_max = g`. Robyn computes the inflexion
point \\g\\ from the adstocked series `a` over the rows of the modelling
window: `gamma * max(a)` from Robyn 3.12.0, and
`(1 - gamma) * min(a) + gamma * max(a)` in Robyn 3.7.0 to 3.11.1. The
two agree when the adstocked series reaches 0 inside the window. Set
`inflexion` to match the version that fitted the model.

Robyn adstocks every row and saturates the rows of the modelling window,
so with the converted arguments `media_transform(x, ...)[window]` equals
Robyn's saturated series. Once converted, `half_max` is a fixed amount
of adstocked media, so the curve does not move when the converted
arguments are applied to other data.

## References

Robyn, Meta's open-source marketing mix modelling package, MIT licence.
<https://github.com/facebookexperimental/Robyn>. The conversions follow
`R/R/transformation.R` on its main branch at commit `5915aa9` (the file
last changed in commit `33baf19`, June 2025), and release 3.11.1 of the
same file for the `"range"` inflexion point.

## See also

[`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md),
which takes the result;
[`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md),
[`adstock_weibull()`](https://elkronos.github.io/mediamix/reference/adstock_weibull.md)
and
[`saturate_hill()`](https://elkronos.github.io/mediamix/reference/saturation.md)
for the individual steps.

## Examples

``` r
spend <- c(0, 120, 300, 80, 0, 0, 450, 200, 60, 0, 0, 0)

# Geometric adstock with Hill saturation
args <- from_robyn(spend, theta = 0.3, alpha = 2, gamma = 0.5)
str(args)
#> List of 2
#>  $ adstock   :List of 3
#>   ..$ kernel   : chr "geometric"
#>   ..$ decay    : num 0.3
#>   ..$ normalise: logi FALSE
#>  $ saturation:List of 3
#>   ..$ type    : chr "hill"
#>   ..$ half_max: num 227
#>   ..$ shape   : num 2
media_transform(spend, adstock = args$adstock,
                saturation = args$saturation)
#>  [1] 0.0000000000 0.2177551583 0.6857754428 0.3872237728 0.0538121073
#>  [6] 0.0050924625 0.8000000000 0.6863704971 0.3336494768 0.0431208493
#> [11] 0.0040393817 0.0003648856

# A geometric decay converts without the series
str(from_robyn(theta = 0.3)$adstock)
#> List of 3
#>  $ kernel   : chr "geometric"
#>  $ decay    : num 0.3
#>  $ normalise: logi FALSE

# Weibull CDF: a scale of 0.1 on a 12-period series is 2 periods
str(from_robyn(spend, adstock = "weibull_cdf", shape = 1.5,
               scale = 0.1)$adstock)
#> List of 6
#>  $ kernel   : chr "weibull"
#>  $ shape    : num 1.5
#>  $ scale    : num 2
#>  $ max_lag  : int 12
#>  $ type     : chr "cdf"
#>  $ normalise: logi FALSE

# A model fitted by Robyn 3.11.1 with a window of rows 3 to 12
str(from_robyn(spend, theta = 0.3, alpha = 2, gamma = 0.5, window = 3:12,
               inflexion = "range")$saturation)
#> List of 3
#>  $ type    : chr "hill"
#>  $ half_max: num 230
#>  $ shape   : num 2
```
