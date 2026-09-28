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

  Separator between channels in the rendered path string.

- converting_only:

  Restrict to converting journeys? Defaults to `FALSE`, because the
  commonest non-converting journeys are usually the more interesting
  half.

## Value

A data frame with one row per distinct channel sequence: `path`,
`journeys` (how many journeys followed it), `conversions` and
`conversion_rate`. Ordered by descending frequency, then alphabetically.

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
#> 1                   social      203          97       0.4778325
#> 2                  display      201          79       0.3930348
#> 3           organic_search      172          81       0.4709302
#> 4    display > paid_search      164          74       0.4512195
#> 5     social > paid_search      158          65       0.4113924
#> 6              paid_search      155          81       0.5225806
#> 7 display > organic_search      102          48       0.4705882
#> 8                    email      102          46       0.4509804
```
