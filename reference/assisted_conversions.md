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

The diagonal counts journeys where a channel appears before *itself* –
repeat exposure. Note that under
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
#>   (missing)        0.036     0.000  0.036     0.117  0.009   0.135 0.135
#>   (none)           0.000     0.005  0.000     0.137  0.000   0.142 0.186
#>   affiliate        0.041     0.015  0.044     0.092  0.035   0.162 0.255
#>   direct           0.000     0.006  0.000     0.094  0.000   0.106 0.159
#>   display          0.046     0.022  0.047     0.161  0.038   0.101 0.304
#>   email            0.021     0.013  0.017     0.078  0.021   0.089 0.068
#>   organic_search   0.032     0.019  0.026     0.120  0.021   0.139 0.226
#>   paid_search      0.027     0.013  0.022     0.088  0.025   0.096 0.179
#>   social           0.051     0.022  0.041     0.137  0.036   0.175 0.290
#>                 assisted
#> assisting        organic_search paid_search social
#>   (blank)                 0.196       0.237  0.123
#>   (missing)               0.135       0.270  0.126
#>   (none)                  0.162       0.265  0.137
#>   affiliate               0.279       0.342  0.189
#>   direct                  0.206       0.259  0.153
#>   display                 0.318       0.415  0.258
#>   email                   0.167       0.250  0.128
#>   organic_search          0.128       0.294  0.168
#>   paid_search             0.191       0.128  0.127
#>   social                  0.266       0.389  0.116
```
