# Conversion lag distribution

How long journeys take, measured from first touch to conversion. This is
how you choose a lookback window instead of reaching for thirty days
because everyone else does: pick the quantile you are willing to
truncate at and read the window off the table.

## Usage

``` r
conversion_lag(paths, probs = c(0.5, 0.75, 0.9, 0.95, 0.99, 1))
```

## Arguments

- paths:

  An `mm_paths` object from
  [`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md).

- probs:

  Quantiles to report.

## Value

A data frame with columns `quantile` and `lag`, in the journey table's
time units, plus an attribute `units` naming them. Only converting
journeys contribute.

## See also

[`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md),
[`path_diagnostics()`](https://elkronos.github.io/mediamix/reference/path_diagnostics.md)

## Examples

``` r
data(mm_events)
paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                     timestamp = "timestamp", conversion = "conversion",
                     split_on = "conversion")

# 90% of journeys complete within this many days
conversion_lag(paths)
#>   quantile        lag
#> 1     0.50   4.634553
#> 2     0.75   8.957101
#> 3     0.90  14.046532
#> 4     0.95  17.427263
#> 5     0.99  29.265186
#> 6     1.00 135.453204
```
