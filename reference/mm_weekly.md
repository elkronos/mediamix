# Synthetic weekly marketing mix panel

Three years of weekly media spend, price and revenue across three
geographies, generated from known adstock and saturation parameters so
that a modelling workflow can be checked against the truth it is trying
to recover.

## Usage

``` r
mm_weekly
```

## Format

A data frame with 468 rows and 11 columns:

- date:

  Week commencing, `Date`.

- geo:

  Geography: `"north"`, `"central"` or `"south"`.

- tv, video, search, social, display:

  Media spend for the week.

- price:

  Average unit price.

- seasonality:

  Underlying seasonal index used to generate the data.

- holiday:

  1 in December weeks, 0 otherwise.

- revenue:

  The KPI.

## Source

Simulated. See `data-raw/make_mm_weekly.R` in the source repository,
<https://github.com/elkronos/mediamix>.

## Details

Revenue was generated as a baseline plus trend, a price term,
seasonality, a December uplift, and for each channel
`beta * saturate_hill(adstock_geometric(spend, decay), half_max, shape)`,
with normally distributed noise. The generating parameters are attached
as the `"truth"` attribute:

    str(attr(mm_weekly, "truth"))

Television is the long-carryover, S-shaped channel (decay 0.85, shape
1.6) and search is the short-carryover, immediately-responding one
(decay 0.15). Channels are flighted, so several contain runs of
zero-spend weeks: dark periods are the normal case in media data, not an
edge case.

The per-channel vectors `decay`, `half_max`, `shape` and `beta` in
`"truth"`, and the single numbers `baseline`, `price_beta`, `trend` and
`noise_sd`, describe north. Each geography has a scale factor, stored as
`geo_scale` (north 1, central 0.72, south 1.35), that multiplies its
spend, half-saturation points, coefficients, baseline, trend, December
uplift (`holiday_uplift` at scale 1) and price effect; the noise
standard deviation is multiplied by the square root of the scale. The
seasonal term is `season_amplitude` times the scaled baseline times
`seasonality - 1`, and price enters centred on its mean within the
geography. `by_geo` lists every channel's decay, half-saturation point,
shape and coefficient for each geography, which is the table to check a
per-geography model against. Decays and shapes are the same everywhere,
so a per-geography model should recover the same carryover in each.

## See also

[mm_events](https://elkronos.github.io/mediamix/reference/mm_events.md)
for the attribution counterpart.

## Examples

``` r
data(mm_weekly)
str(mm_weekly)
#> 'data.frame':    468 obs. of  11 variables:
#>  $ date       : Date, format: "2023-01-02" "2023-01-09" ...
#>  $ geo        : chr  "north" "north" "north" "north" ...
#>  $ tv         : num  0 0 2193 0 0 ...
#>  $ video      : num  399 0 0 0 0 584 482 824 0 251 ...
#>  $ search     : num  277 488 568 576 401 424 614 481 357 423 ...
#>  $ social     : num  296 328 0 303 500 182 126 348 435 428 ...
#>  $ display    : num  0 280 0 251 0 306 0 348 222 0 ...
#>  $ price      : num  22.1 25 25.3 25 26.8 ...
#>  $ seasonality: num  0.899 0.944 0.988 1.03 1.067 ...
#>  $ holiday    : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ revenue    : num  27130 20433 20481 20565 17574 ...
#>  - attr(*, "truth")=List of 12
#>   ..$ decay           : Named num [1:5] 0.85 0.7 0.15 0.45 0.55
#>   .. ..- attr(*, "names")= chr [1:5] "tv" "video" "search" "social" ...
#>   ..$ half_max        : Named num [1:5] 2250 600 450 350 300
#>   .. ..- attr(*, "names")= chr [1:5] "tv" "video" "search" "social" ...
#>   ..$ shape           : Named num [1:5] 1.6 1 1 1 1
#>   .. ..- attr(*, "names")= chr [1:5] "tv" "video" "search" "social" ...
#>   ..$ beta            : Named num [1:5] 5200 2400 4100 1800 900
#>   .. ..- attr(*, "names")= chr [1:5] "tv" "video" "search" "social" ...
#>   ..$ baseline        : num 18000
#>   ..$ price_beta      : num -2600
#>   ..$ trend           : num 22
#>   ..$ noise_sd        : num 900
#>   ..$ geo_scale       : Named num [1:3] 1 0.72 1.35
#>   .. ..- attr(*, "names")= chr [1:3] "north" "central" "south"
#>   ..$ holiday_uplift  : num 2500
#>   ..$ season_amplitude: num 0.35
#>   ..$ by_geo          :'data.frame': 15 obs. of  6 variables:
#>   .. ..$ geo     : chr [1:15] "north" "north" "north" "north" ...
#>   .. ..$ channel : chr [1:15] "tv" "video" "search" "social" ...
#>   .. ..$ decay   : num [1:15] 0.85 0.7 0.15 0.45 0.55 0.85 0.7 0.15 0.45 0.55 ...
#>   .. ..$ half_max: num [1:15] 2250 600 450 350 300 1620 432 324 252 216 ...
#>   .. ..$ shape   : num [1:15] 1.6 1 1 1 1 1.6 1 1 1 1 ...
#>   .. ..$ beta    : num [1:15] 5200 2400 4100 1800 900 ...

# The parameters the data was simulated from
attr(mm_weekly, "truth")$decay
#>      tv   video  search  social display 
#>    0.85    0.70    0.15    0.45    0.55 

# Per-geography parameters, for checking a model fitted to one geography
subset(attr(mm_weekly, "truth")$by_geo, geo == "south")
#>      geo channel decay half_max shape beta
#> 11 south      tv  0.85   3037.5   1.6 7020
#> 12 south   video  0.70    810.0   1.0 3240
#> 13 south  search  0.15    607.5   1.0 5535
#> 14 south  social  0.45    472.5   1.0 2430
#> 15 south display  0.55    405.0   1.0 1215

# Flighting: several channels go dark for weeks at a time
north <- mm_weekly[mm_weekly$geo == "north", ]
mean(north$tv == 0)
#> [1] 0.1346154
```
