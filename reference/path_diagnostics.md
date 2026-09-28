# Run every journey diagnostic at once

A convenience wrapper returning the diagnostics an analyst normally
presents together. Each element is exactly what the corresponding
function returns.

## Usage

``` r
path_diagnostics(paths, n = 10)
```

## Arguments

- paths:

  An `mm_paths` object from
  [`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md).

- n:

  Number of top paths to include.

## Value

A named list with elements `summary`, `lengths`, `positions`, `assists`,
`top_paths` and `conversion_lag`.

## See also

[`path_summary()`](https://elkronos.github.io/mediamix/reference/path_summary.md),
[`path_lengths()`](https://elkronos.github.io/mediamix/reference/path_lengths.md),
[`channel_positions()`](https://elkronos.github.io/mediamix/reference/channel_positions.md),
[`assisted_conversions()`](https://elkronos.github.io/mediamix/reference/assisted_conversions.md),
[`top_paths()`](https://elkronos.github.io/mediamix/reference/top_paths.md),
[`conversion_lag()`](https://elkronos.github.io/mediamix/reference/conversion_lag.md)

## Examples

``` r
data(mm_events)
paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                     timestamp = "timestamp", conversion = "conversion")
diag <- path_diagnostics(paths)
names(diag)
#> [1] "summary"        "lengths"        "positions"      "assists"       
#> [5] "top_paths"      "conversion_lag"
diag$summary
#>   events_in events_out journeys converting_journeys mean_length median_length
#> 1     20799      17306     6137                2741    2.819945             3
#>   max_length direct_events conversion_events conversions_observed
#> 1          9          1597              2741                 2741
#>   conversions_unattributable
#> 1                          0
```
