# Default linear fitter for carryover tuning

A bare intercept-and-slope least squares fit of `y` on transformed
media, supplied so that
[`tune_carryover()`](https://elkronos.github.io/mediamix/reference/tune_carryover.md)
has a sensible default and the simple case stays one line. It exists to
be replaced: pass your own `fit_fn` and `predict_fn` to tune carryover
against the model you intend to fit.

## Usage

``` r
fit_ols(x, y)

# S3 method for class 'mm_ols'
predict(object, newdata = NULL, ...)

# S3 method for class 'mm_ols'
coef(object, ...)

# S3 method for class 'mm_ols'
fitted(object, ...)

# S3 method for class 'mm_ols'
print(x, ...)
```

## Arguments

- x:

  For `fit_ols()`, a numeric vector of transformed (adstocked) media.
  For [`print()`](https://rdrr.io/r/base/print.html), an `mm_ols`
  object.

- y:

  Numeric vector of the response, the same length as `x`.

- object:

  An `mm_ols` object.

- newdata:

  Numeric vector of transformed media to predict from. When omitted,
  [`predict()`](https://rdrr.io/r/stats/predict.html) returns the fitted
  values.

- ...:

  Ignored, present for generic consistency.

## Value

`fit_ols()` returns an object of class `mm_ols`: a list with elements
`intercept`, `slope`, `n` and `fitted.values`, the fitted values of the
`n` finite pairs used.
[`predict()`](https://rdrr.io/r/stats/predict.html) returns a numeric
vector the same length as `newdata`.
[`coef()`](https://rdrr.io/r/stats/coef.html) returns the intercept and
slope, named `(Intercept)` and `media`, so a fit on a column named
`media` works with
[`contributions()`](https://elkronos.github.io/mediamix/reference/contributions.md).
[`fitted()`](https://rdrr.io/r/stats/fitted.values.html) returns
`fitted.values`.

## Details

This is deliberately the simplest possible model. It has no trend, no
seasonality, no price term and no other controls, so carryover
parameters chosen against it absorb whatever those omitted terms would
have explained. That is fine for a first pass and wrong for a
deliverable.

To tune against a model with controls, pass them to
[`tune_carryover()`](https://elkronos.github.io/mediamix/reference/tune_carryover.md)'s
`controls` argument, which switches the default model to least squares
on the adstocked media plus the controls and keeps every row aligned
with every split. For several channels at once use
[`tune_carryover_joint()`](https://elkronos.github.io/mediamix/reference/tune_carryover_joint.md),
and for carryover tuned jointly with saturation and a model penalty,
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
in a recipes pipeline. The examples of
[`tune_carryover()`](https://elkronos.github.io/mediamix/reference/tune_carryover.md)
show the first;
[`vignette("carryover")`](https://elkronos.github.io/mediamix/articles/carryover.md)
and
[`vignette("tidymodels")`](https://elkronos.github.io/mediamix/articles/tidymodels.md)
work through the other two.

Pairs where `x` or `y` is missing or non-finite are dropped before
fitting. With fewer than two pairs left the slope is zero and the
intercept is the one remaining `y`, or `NA` when none remains. Otherwise
the slope is set to zero when `x` has no spread relative to its own
scale, so the answer does not depend on the units `x` is measured in.

## See also

[`tune_carryover()`](https://elkronos.github.io/mediamix/reference/tune_carryover.md),
which uses `fit_ols()` and
[`rmse()`](https://elkronos.github.io/mediamix/reference/metrics.md) as
its defaults.

## Examples

``` r
set.seed(1)
spend <- adstock_geometric(c(100, 50, 0, 0, 200, 100, 0, 50), decay = 0.5)
kpi <- 10 + 0.4 * spend + rnorm(8, sd = 0.1)

m <- fit_ols(spend, kpi)
m
#> <mm_ols> fitted on 8 observations
#> response = 10.06 + 0.3991 * media
coef(m)
#> (Intercept)       media 
#>  10.0634323   0.3991045 
predict(m, spend)
#> [1] 30.01866 30.01866 20.04104 15.05224 52.46828 51.22108 30.64226 30.33046
```
