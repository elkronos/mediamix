# Range of each channel's share across attribution rules

Summarises the output of
[`attribute()`](https://elkronos.github.io/mediamix/reference/attribute.md)
into one row per channel, showing the range of its share of credited
conversions across rules. A channel whose share swings from 8% to 31%
depending on the convention has not been measured; it has been assigned
a number.

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
`channel`, `min_share`, `max_share`, `mean_share`, `spread` (`max_share`
minus `min_share`) and, only when both the `"first"` and `"last"` rules
were run, `first_last_ratio` (the first-touch share divided by the
last-touch share, `NA` when the last-touch share is 0). For a result of
`attribute(by = )`, one row per segment and channel instead, with the
segment columns first and rows ordered by segment, then by descending
spread.

`first_last_ratio` here is a ratio of credited *shares*, which is not
the same quantity as the column of the same name in
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
#> 1         display 0.07187158 0.29113462 0.17382396 0.2192630427
#> 2     paid_search 0.09157242 0.27252827 0.18342704 0.1809558555
#> 3           email 0.05654870 0.18314484 0.12349952 0.1265961328
#> 4          social 0.11638088 0.22072236 0.16553747 0.1043414812
#> 5  organic_search 0.12623130 0.17876687 0.15470603 0.0525355710
#> 6       affiliate 0.08062751 0.11711054 0.09861140 0.0364830354
#> 7          direct 0.02225465 0.02682848 0.02410676 0.0045738324
#> 8         (blank) 0.02955126 0.03378661 0.03175589 0.0042353471
#> 9          (none) 0.02736228 0.02982010 0.02849282 0.0024578273
#> 10      (missing) 0.01568771 0.01637007 0.01603912 0.0006823617
#>    first_last_ratio
#> 1         4.0507614
#> 2         0.3360107
#> 3         0.3087649
#> 4         1.8965517
#> 5         0.7061224
#> 6         1.4524887
#> 7         1.0491803
#> 8         0.9418605
#> 9         1.0133333
#> 10        1.0232558
```
