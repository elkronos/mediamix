# Journey length distribution

Journey length distribution

## Usage

``` r
path_lengths(paths)
```

## Arguments

- paths:

  An `mm_paths` object from
  [`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md).

## Value

A data frame with one row per journey length: `length`, `journeys`,
`converting`, and `conversion_rate`. The conversion rate by length is
worth looking at before choosing a credit rule: if it is flat, extra
touchpoints are recording exposure rather than driving it.

## See also

[`path_diagnostics()`](https://elkronos.github.io/mediamix/reference/path_diagnostics.md)

## Examples

``` r
data(mm_events)
paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                     timestamp = "timestamp", conversion = "conversion")
path_lengths(paths)
#>   length journeys converting conversion_rate
#> 1      1     1053        492       0.4672365
#> 2      2     1762        776       0.4404086
#> 3      3     1537        703       0.4573845
#> 4      4     1031        453       0.4393792
#> 5      5      462        190       0.4112554
#> 6      6      188         90       0.4787234
#> 7      7       49         23       0.4693878
#> 8      8       18         11       0.6111111
#> 9      9        5          3       0.6000000
```
