# Most common journeys

Most common journeys

## Usage

``` r
top_paths(paths, n = 10, sep = " > ", converting_only = FALSE)
```

## Arguments

- paths:

  An `mm_paths` object from
  [`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md).

- n:

  Number of distinct channel sequences to return. Each row aggregates
  every journey that followed that sequence, so the `journeys` column
  will normally sum to far more than `n`.

- sep:

  Separator between channels in the rendered path string. A channel name
  that contains `sep` is an error, since two different channel sequences
  could then render as the same string.

- converting_only:

  Restrict to converting journeys? Defaults to `FALSE`, because the
  commonest non-converting journeys are usually the more interesting
  half.

## Value

A data frame with one row per distinct channel sequence: `path`,
`journeys` (how many journeys followed it), `conversions` and
`conversion_rate`. Ordered by descending frequency, then alphabetically
in the C locale, so ties fall the same way on every platform.

## See also

[`as_channel_paths()`](https://elkronos.github.io/mediamix/reference/as_channel_paths.md),
[`path_diagnostics()`](https://elkronos.github.io/mediamix/reference/path_diagnostics.md)

## Examples

``` r
data(mm_events)
paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                     timestamp = "timestamp", conversion = "conversion")
top_paths(paths, n = 8)
#>                       path journeys conversions conversion_rate
#> 1                  display      197          79       0.4010152
#> 2                   social      195          98       0.5025641
#> 3           organic_search      164          80       0.4878049
#> 4    display > paid_search      162          72       0.4444444
#> 5     social > paid_search      158          65       0.4113924
#> 6              paid_search      154          81       0.5259740
#> 7 display > organic_search      102          48       0.4705882
#> 8                    email       99          46       0.4646465
```
