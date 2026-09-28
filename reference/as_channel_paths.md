# Export journeys in ChannelAttribution's format

Collapses an `mm_paths` object into the aggregated `"a > b > c"` table
that ChannelAttribution consumes. This package builds the journeys;
ChannelAttribution computes Markov removal effects on them in C++.
Neither needs to do the other's job.

## Usage

``` r
as_channel_paths(paths, sep = " > ")
```

## Arguments

- paths:

  An `mm_paths` object from
  [`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md).

- sep:

  Separator between channels. `" > "` is ChannelAttribution's default.

## Value

A data frame with columns `path`, `total_conversions`, `total_null` and,
when the journey table carries values, `total_conversion_value`. One row
per distinct channel sequence.

## Details

The `total_null` column is why
[`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md)
keeps non-converting journeys by default. Markov removal effects are
computed by comparing the probability of reaching conversion against the
probability of reaching the null state, so a transition matrix built
without null paths has no absorbing failure state and overstates every
channel's effect.

## See also

[`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md),
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
#> 1                social                97        106               13120.33
#> 2        organic_search                81         91                9013.03
#> 3           paid_search                81         74               10658.24
#> 4               display                79        122                9628.17
#> 5 display > paid_search                74         90                9992.80
#> 6  social > paid_search                65         93                9576.09

# Hand off to ChannelAttribution, if it is installed
if (requireNamespace("ChannelAttribution", quietly = TRUE)) {
  ChannelAttribution::markov_model(
    ca, var_path = "path",
    var_conv = "total_conversions", var_null = "total_null"
  )
}
#> 
#> Number of simulations: 100000 - Convergence reached: 2.69% < 5.00%
#> 
#> Percentage of simulated paths that successfully end before maximum number of steps (10) is reached: 99.14%
#> 
#> [1] "*** Install ChannelAttribution Pro for free running install_pro(). Visit https://channelattribution.io for more info. Set flg_pro=FALSE to hide this message."
#>      channel_name total_conversions
#> 1          social         458.63798
#> 2  organic_search         415.84208
#> 3     paid_search         488.54355
#> 4         display         472.04392
#> 5           email         352.16384
#> 6       affiliate         285.13412
#> 7         (blank)          81.72470
#> 8          (none)          73.99050
#> 9          direct          68.31875
#> 10      (missing)          44.60055
```
