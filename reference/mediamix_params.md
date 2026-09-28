# Tuning parameters for media transforms

dials parameter objects for the quantities
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
and
[`step_saturation()`](https://elkronos.github.io/mediamix/reference/step_saturation.md)
expose to tuning. Having these is the entire reason to be a recipe step
rather than a function you call beforehand: they let carryover decay,
saturation shape and a model's own penalty be tuned *jointly*, in one
[`tune::tune_bayes()`](https://tune.tidymodels.org/reference/tune_bayes.html)
call over proper rolling-origin resampling, rather than fixing the
transform parameters by eye and tuning only the model.

## Usage

``` r
carryover_decay(range = c(0, 0.95), trans = NULL)

carryover_max_lag(range = c(1L, 26L), trans = NULL)

saturation_half_max(range = c(0.05, 1), trans = NULL)

saturation_shape(range = c(0.5, 3), trans = NULL)
```

## Arguments

- range:

  A two-element vector giving the range of the parameter.

- trans:

  A transformation from scales, or `NULL` for none.

## Value

A `dials` parameter object.

## Details

`carryover_decay()` spans `[0, 0.95]` rather than `[0, 1)`. The upper
bound is a practical one: a decay of 0.99 implies a half-life of 69
periods, which no ordinary media dataset can identify, and leaving it in
the range mostly wastes tuning iterations on parameter values that trade
off against the intercept.

`carryover_max_lag()` spans 1 to 26 periods. It is finite by necessity:
`Inf` is a legitimate value for
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
but not a tunable one, since a search cannot propose an infinite
integer. So tuning `max_lag` over a finite grid and leaving it fixed at
`Inf` are genuinely different searches – the first over truncated
kernels, the second over the recursive one. Most of the time, tuning
`decay` with `max_lag = Inf` is the better use of the budget: the
truncation point is usually a modelling decision rather than something
the data speaks to.

`saturation_half_max()` is a fraction of each column's reference level,
so its range is unitless and comparable across channels. See
[`step_saturation()`](https://elkronos.github.io/mediamix/reference/step_saturation.md).

`saturation_shape()` spans `[0.5, 3]`. Values above 1 give an S-shaped
response with a threshold below which media barely registers; below 1
the curve bends harder than a plain hyperbola. Note that
[`step_saturation()`](https://elkronos.github.io/mediamix/reference/step_saturation.md)
narrows this range to `(0.1, 1]` when `type = "power"`, where `shape` is
the exponent itself and a value above 1 would not be saturating at all.

## See also

[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md),
[`step_saturation()`](https://elkronos.github.io/mediamix/reference/step_saturation.md)

## Examples

``` r
carryover_decay()
#> Carryover Decay (quantitative)
#> Range: [0, 0.95]
dials::value_seq(carryover_decay(), 5)
#> [1] 0.0000 0.2375 0.4750 0.7125 0.9500

saturation_shape()
#> Saturation Shape (quantitative)
#> Range: [0.5, 3]
dials::grid_regular(carryover_decay(), saturation_shape(), levels = 3)
#> # A tibble: 9 × 2
#>   decay shape
#>   <dbl> <dbl>
#> 1 0      0.5 
#> 2 0.475  0.5 
#> 3 0.95   0.5 
#> 4 0      1.75
#> 5 0.475  1.75
#> 6 0.95   1.75
#> 7 0      3   
#> 8 0.475  3   
#> 9 0.95   3   
```
