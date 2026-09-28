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

- tv, video, search, display, social:

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

Simulated. See `data-raw/make_mm_weekly.R` in the package sources.

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

Geographies differ by a single multiplicative scale factor applied to
spend, half-saturation points, coefficients and baseline, so a correctly
specified pooled model can fit them together and a per-geography model
should recover the same decay parameters in each.

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
#>  - attr(*, "truth")=List of 8
#>   ..$ decay     : Named num [1:5] 0.85 0.7 0.15 0.45 0.55
#>   .. ..- attr(*, "names")= chr [1:5] "tv" "video" "search" "social" ...
#>   ..$ half_max  : Named num [1:5] 2250 600 450 350 300
#>   .. ..- attr(*, "names")= chr [1:5] "tv" "video" "search" "social" ...
#>   ..$ shape     : Named num [1:5] 1.6 1 1 1 1
#>   .. ..- attr(*, "names")= chr [1:5] "tv" "video" "search" "social" ...
#>   ..$ beta      : Named num [1:5] 5200 2400 4100 1800 900
#>   .. ..- attr(*, "names")= chr [1:5] "tv" "video" "search" "social" ...
#>   ..$ baseline  : num 18000
#>   ..$ price_beta: num -2600
#>   ..$ trend     : num 22
#>   ..$ noise_sd  : num 900

# The parameters the data was generated from
attr(mm_weekly, "truth")$decay
#>      tv   video  search  social display 
#>    0.85    0.70    0.15    0.45    0.55 

# Flighting: several channels go dark for weeks at a time
north <- mm_weekly[mm_weekly$geo == "north", ]
mean(north$tv == 0)
#> [1] 0.1346154
```
