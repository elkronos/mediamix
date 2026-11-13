# Tuning parameters for media transforms

dials parameter objects for the quantities
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
and
[`step_saturation()`](https://elkronos.github.io/mediamix/reference/step_saturation.md)
expose to tuning. They are one reason for the transforms to be recipe
steps rather than functions you call beforehand: they let carryover
decay, saturation shape and a model's own penalty be tuned *jointly*, in
one
[`tune::tune_bayes()`](https://tune.tidymodels.org/reference/tune_bayes.html)
call over proper rolling-origin resampling, rather than fixing the
transform parameters by eye and tuning only the model.

## Usage

``` r
carryover_decay(range = c(0, 0.95), trans = NULL)

carryover_max_lag(range = c(1L, 26L), trans = NULL)

carryover_shape(range = c(0.5, 5), trans = NULL)

carryover_scale(range = c(0.5, 10), trans = NULL)

carryover_peak(range = c(0, 4), trans = NULL)

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
`Inf` are genuinely different searches: the first over truncated
kernels, the second over the recursive one. Most of the time, tuning
`decay` with `max_lag = Inf` is the better use of the budget: the
truncation point is usually a modelling decision rather than something
the data speaks to.

`carryover_shape()` and `carryover_scale()` are the shape and scale of
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)'s
Weibull kernel. The shape spans `[0.5, 5]` and the scale `[0.5, 10]`
periods. Over those ranges the kernel keeps some weight at its first lag
in both forms and at every `max_lag`, so no candidate is rejected as a
kernel that is zero everywhere. `carryover_peak()` is the delayed
kernel's peak lag, from 0 to 4 periods. A peak must not pass
`max_lag - 1`, so
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
stops this range there when `max_lag` is fixed, and starts a tuned
`max_lag` above the largest peak. For the delayed kernel it also narrows
`carryover_decay()` to `[0.05, 0.95]`, since that kernel needs a
positive retention rate.

`saturation_half_max()` is a fraction of each column's reference level,
so its range is unitless and comparable across channels. See
[`step_saturation()`](https://elkronos.github.io/mediamix/reference/step_saturation.md).

`saturation_shape()` spans `[0.5, 3]`. Values above 1 give an S-shaped
response with a threshold below which media barely registers; below 1
the curve bends harder than a plain hyperbola.
[`step_saturation()`](https://elkronos.github.io/mediamix/reference/step_saturation.md)
narrows this range to `[0.1, 1]` when `type = "power"`, where `shape` is
the exponent itself and a value above 1 would not be saturating at all.
It offers `saturation_shape()` only for the Hill and power curves and
`saturation_half_max()` only for the three curves with a half-saturation
point, since the others ignore them.

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

# The Weibull kernel's two parameters
dials::grid_regular(carryover_shape(), carryover_scale(), levels = 3)
#> # A tibble: 9 × 2
#>   shape scale
#>   <dbl> <dbl>
#> 1  0.5   0.5 
#> 2  2.75  0.5 
#> 3  5     0.5 
#> 4  0.5   5.25
#> 5  2.75  5.25
#> 6  5     5.25
#> 7  0.5  10   
#> 8  2.75 10   
#> 9  5    10   
```
