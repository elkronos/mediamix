# Minimal error metrics

Two metrics, provided only so that
[`tune_carryover()`](https://elkronos.github.io/mediamix/reference/tune_carryover.md)
has a working default and a one-line simple case. Any function of
`(actual, predicted)` returning a single number will do, so use
yardstick if you want a real metric library.

## Usage

``` r
rmse(actual, predicted, na_rm = TRUE)

mae(actual, predicted, na_rm = TRUE)
```

## Arguments

- actual:

  Numeric vector of observed values.

- predicted:

  Numeric vector of predicted values, the same length as `actual`.

- na_rm:

  Should pairs where either value is missing or non-finite be dropped?
  Defaults to `TRUE`.

## Value

A single number. `NA_real_` if no complete pairs remain.

## See also

[`tune_carryover()`](https://elkronos.github.io/mediamix/reference/tune_carryover.md),
which takes any such function as `metric_fn`.

## Examples

``` r
rmse(c(1, 2, 3), c(1.1, 1.9, 3.2))
#> [1] 0.1414214
mae(c(1, 2, 3), c(1.1, 1.9, 3.2))
#> [1] 0.1333333
```
