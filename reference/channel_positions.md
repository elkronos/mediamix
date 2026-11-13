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
#> 1     paid_search   81   170    503  666    1420        0.2552553
#> 2         display   79   719    437  118    1353        6.0932203
#> 3          social   98   507    450  221    1276        2.2941176
#> 4  organic_search   80   266    467  410    1223        0.6487805
#> 5           email   46   109    363  456     974        0.2390351
#> 6       affiliate   40   281    277  181     779        1.5524862
#> 7         (blank)   27    54     79   59     219        0.9152542
#> 8          (none)   19    57     73   56     205        1.0178571
#> 9          direct   13    51     58   48     170        1.0625000
#> 10      (missing)    9    35     34   34     112        1.0294118
```
