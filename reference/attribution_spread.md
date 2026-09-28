# How much does the answer depend on the rule?

Summarises the output of
[`attribute()`](https://elkronos.github.io/mediamix/reference/attribute.md)
into one row per channel, showing the range of credited conversions
across rules. A channel whose share swings from 8% to 31% depending on
the convention has not been measured; it has been assigned a number.

## Usage

``` r
attribution_spread(attribution)
```

## Arguments

- attribution:

  The data frame returned by
  [`attribute()`](https://elkronos.github.io/mediamix/reference/attribute.md).

## Value

A data frame with one row per channel, ordered by descending spread:
`channel`, `min_share`, `max_share`, `mean_share`, `spread` (the
difference between the first two), and – only when both the `"first"`
and `"last"` rules were run – `first_last_ratio`.

Note that `first_last_ratio` here is a ratio of credited *shares*, which
is not the same quantity as the column of the same name in
[`channel_positions()`](https://elkronos.github.io/mediamix/reference/channel_positions.md),
a ratio of touchpoint *counts*. They answer the same question in
different currencies and will not agree numerically.

## See also

[`attribute()`](https://elkronos.github.io/mediamix/reference/attribute.md)

## Examples

``` r
data(mm_events)
paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                     timestamp = "timestamp", conversion = "conversion")
attribution_spread(attribute(paths))
#>           channel  min_share  max_share mean_share       spread
#> 1         display 0.07187158 0.29149945 0.17387971 0.2196278730
#> 2     paid_search 0.09193725 0.27252827 0.18376127 0.1805910252
#> 3           email 0.05654870 0.18314484 0.12365610 0.1265961328
#> 4          social 0.11638088 0.21926304 0.16513028 0.1028821598
#> 5  organic_search 0.12805545 0.17876687 0.15500798 0.0507114192
#> 6       affiliate 0.08062751 0.11638088 0.09828284 0.0357533747
#> 7         (blank) 0.02918643 0.03390476 0.03171303 0.0047183334
#> 8          direct 0.02225465 0.02681421 0.02403805 0.0045595618
#> 9          (none) 0.02736228 0.02982010 0.02848199 0.0024578273
#> 10      (missing) 0.01568771 0.01641737 0.01604876 0.0007296607
#>    first_last_ratio
#> 1         4.0558376
#> 2         0.3373494
#> 3         0.3087649
#> 4         1.8840125
#> 5         0.7163265
#> 6         1.4434389
#> 7         0.9302326
#> 8         1.0327869
#> 9         1.0133333
#> 10        1.0465116
```
