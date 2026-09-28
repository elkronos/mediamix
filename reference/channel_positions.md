# Where each channel sits in the journey

Counts how often each channel appears as the only touch, the first
touch, a middle touch or the last touch, and reports the ratio of
first-touch to last-touch appearances. A ratio well above 1 marks a
channel that opens journeys; well below 1 marks one that closes them.

## Usage

``` r
channel_positions(paths, converting_only = TRUE)
```

## Arguments

- paths:

  An `mm_paths` object from
  [`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md).

- converting_only:

  Restrict to converting journeys? Defaults to `TRUE`, which is what you
  want when reading these as a role description. Set `FALSE` to see raw
  exposure.

## Value

A data frame with one row per channel, ordered by descending `touches`:
`channel`, `only`, `first`, `middle`, `last`, `touches`, and
`first_last_ratio` (the count of first-touch appearances divided by the
count of last-touch appearances, `NA` when the channel never appears
last).

This `first_last_ratio` is a ratio of touchpoint counts. The column of
the same name in
[`attribution_spread()`](https://elkronos.github.io/mediamix/reference/attribution_spread.md)
is a ratio of credited shares; the two will not agree numerically.

## See also

[`path_diagnostics()`](https://elkronos.github.io/mediamix/reference/path_diagnostics.md),
[`assisted_conversions()`](https://elkronos.github.io/mediamix/reference/assisted_conversions.md)

## Examples

``` r
data(mm_events)
paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                     timestamp = "timestamp", conversion = "conversion")
channel_positions(paths)
#>           channel only first middle last touches first_last_ratio
#> 9     paid_search   81   171    503  666    1421        0.2567568
#> 6         display   79   720    437  118    1354        6.1016949
#> 10         social   97   504    449  222    1272        2.2702703
#> 8  organic_search   81   270    461  409    1221        0.6601467
#> 7           email   46   109    364  456     975        0.2390351
#> 4       affiliate   40   279    276  181     776        1.5414365
#> 1         (blank)   27    53     80   59     219        0.8983051
#> 3          (none)   19    57     72   56     204        1.0178571
#> 5          direct   13    50     59   48     170        1.0416667
#> 2       (missing)    9    36     32   34     111        1.0588235
```
