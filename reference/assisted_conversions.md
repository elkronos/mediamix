# Assisted conversion matrix

For each ordered pair of channels, how many converting journeys contain
the first channel somewhere before the second. Read a row as "this
channel assisted these channels".

## Usage

``` r
assisted_conversions(paths, normalise = FALSE)
```

## Arguments

- paths:

  An `mm_paths` object from
  [`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md).

- normalise:

  Divide each cell by the number of converting journeys containing the
  assisting channel, giving a rate rather than a count? Defaults to
  `FALSE`.

## Value

A numeric matrix with assisting channels as rows and assisted channels
as columns.

## Details

The diagonal counts journeys where a channel appears before *itself*,
which is repeat exposure. Under
[`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md)'s
default `collapse_repeats = TRUE`, consecutive repeats have already been
merged, so the diagonal only picks up channels that recur after an
intervening different channel. Build the paths with
`collapse_repeats = FALSE` if you want back-to-back repeat exposure to
show up here.

## See also

[`channel_positions()`](https://elkronos.github.io/mediamix/reference/channel_positions.md),
[`path_diagnostics()`](https://elkronos.github.io/mediamix/reference/path_diagnostics.md)

## Examples

``` r
data(mm_events)
paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                     timestamp = "timestamp", conversion = "conversion")
round(assisted_conversions(paths, normalise = TRUE), 3)
#>                 assisted
#> assisting        (blank) (missing) (none) affiliate direct display email
#>   (blank)          0.000     0.009  0.000     0.078  0.000   0.114 0.192
#>   (missing)        0.036     0.000  0.036     0.116  0.009   0.125 0.143
#>   (none)           0.000     0.005  0.000     0.137  0.000   0.141 0.185
#>   affiliate        0.041     0.015  0.043     0.091  0.034   0.160 0.252
#>   direct           0.000     0.006  0.000     0.094  0.000   0.106 0.159
#>   display          0.045     0.023  0.047     0.163  0.038   0.101 0.303
#>   email            0.021     0.013  0.017     0.078  0.021   0.090 0.068
#>   organic_search   0.031     0.019  0.028     0.119  0.020   0.135 0.225
#>   paid_search      0.027     0.013  0.022     0.089  0.025   0.096 0.179
#>   social           0.051     0.023  0.041     0.136  0.036   0.176 0.288
#>                 assisted
#> assisting        organic_search paid_search social
#>   (blank)                 0.205       0.237  0.123
#>   (missing)               0.143       0.259  0.134
#>   (none)                  0.171       0.263  0.137
#>   affiliate               0.281       0.342  0.191
#>   direct                  0.212       0.259  0.153
#>   display                 0.316       0.415  0.259
#>   email                   0.166       0.250  0.128
#>   organic_search          0.125       0.296  0.167
#>   paid_search             0.191       0.128  0.125
#>   social                  0.267       0.391  0.117
```
