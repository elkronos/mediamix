# Plot a response curve

Two stacked panels on a shared spend axis: the response and its marginal
return. Spend beyond the observed range is shaded, because there the
curve is extrapolation.

## Usage

``` r
# S3 method for class 'mm_response_curve'
plot(x, observed = NULL, ...)
```

## Arguments

- x:

  A data frame from
  [`response_curve()`](https://elkronos.github.io/mediamix/reference/response_curve.md).

- observed:

  Optional numeric vector of the spend levels actually seen (for example
  the adstocked series); its range is left unshaded.

- ...:

  Ignored.

## Value

`x`, invisibly.

## Examples

``` r
rc <- response_curve(seq(0, 6000, by = 100), coefficient = 6000,
                     half_max = 2250, shape = 1.6)
plot(rc, observed = c(0, 3450))
```
