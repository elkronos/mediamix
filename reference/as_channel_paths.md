# Export journeys in ChannelAttribution's format

Collapses an `mm_paths` object into the aggregated `"a > b > c"` table
that ChannelAttribution consumes. This package builds the journeys and
fits the order-1 chain of
[`markov_removal()`](https://elkronos.github.io/mediamix/reference/markov_removal.md);
ChannelAttribution computes Markov removal effects on them in C++, for
higher-order chains too.

## Usage

``` r
as_channel_paths(paths, sep = " > ")
```

## Arguments

- paths:

  An `mm_paths` object from
  [`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md).

- sep:

  Separator between channels, `" > "` by default. A channel name that
  contains `sep` is an error, since its path strings could not be split
  back into channels. ChannelAttribution's `markov_model()` has a `sep`
  argument of its own; give both the same value, as the example does.

## Value

A data frame with columns `path`, `total_conversions`, `total_null` and,
when the journey table carries values, `total_conversion_value`. One row
per distinct channel sequence, ordered by descending conversions, then
descending null journeys, then path in the C locale.

## Details

The `total_null` column is why
[`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md)
keeps non-converting journeys by default. Markov removal effects compare
the probability of reaching conversion with the probability of reaching
the null state. Without null paths the transition matrix has no
absorbing failure state, the baseline conversion probability is 1, and
each channel's removal effect moves up or down depending on how the
missing non-converting journeys differ from the converting ones.

## See also

[`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md),
[`markov_removal()`](https://elkronos.github.io/mediamix/reference/markov_removal.md),
[`attribute()`](https://elkronos.github.io/mediamix/reference/attribute.md)

## Examples

``` r
data(mm_events)
paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                     timestamp = "timestamp", conversion = "conversion",
                     value = "value")

ca <- as_channel_paths(paths)
head(ca)
#>                    path total_conversions total_null total_conversion_value
#> 1                social                98         97               13267.94
#> 2           paid_search                81         73               10658.24
#> 3        organic_search                80         84                8997.63
#> 4               display                79        118                9628.17
#> 5 display > paid_search                72         90                9719.46
#> 6  social > paid_search                65         93                9576.09

# Hand off to ChannelAttribution, if it is installed, with the same
# separator on both sides
if (requireNamespace("ChannelAttribution", quietly = TRUE)) {
  ca_in <- as_channel_paths(paths, sep = ">")
  ChannelAttribution::markov_model(
    ca_in, var_path = "path",
    var_conv = "total_conversions", var_null = "total_null", sep = ">"
  )
}
#> 
#> Number of simulations: 100000 - Convergence reached: 1.90% < 5.00%
#> 
#> Percentage of simulated paths that successfully end before maximum number of steps (10) is reached: 99.11%
#> 
#> [1] "*** Install ChannelAttribution Pro for free running install_pro(). Visit https://channelattribution.io for more info. Set flg_pro=FALSE to hide this message."
#>      channel_name total_conversions
#> 1          social         450.44758
#> 2     paid_search         493.04727
#> 3  organic_search         417.42652
#> 4         display         464.31138
#> 5           email         356.17372
#> 6       affiliate         279.04056
#> 7         (blank)          82.17454
#> 8          (none)          75.87282
#> 9          direct          75.62075
#> 10      (missing)          46.88486
```
