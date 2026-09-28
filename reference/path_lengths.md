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
#> 1      1     1085        492       0.4534562
#> 2      2     1778        792       0.4454443
#> 3      3     1516        682       0.4498681
#> 4      4     1034        456       0.4410058
#> 5      5      464        192       0.4137931
#> 6      6      187         89       0.4759358
#> 7      7       50         24       0.4800000
#> 8      8       18         11       0.6111111
#> 9      9        5          3       0.6000000
```
