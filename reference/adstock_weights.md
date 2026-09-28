# Geometric adstock weights

The finite geometric kernel \\w_i = \theta^i\\ for lags \\i = 0, 1,
\ldots, L-1\\.
[`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md)
calls this internally when `max_lag` is finite; it is exported because
the weights themselves are often what you want to plot or report.

## Usage

``` r
adstock_weights(max_lag, decay, normalise = TRUE)
```

## Arguments

- max_lag:

  Number of periods the kernel spans, including the current period.
  `max_lag = 1` means no carryover.

- decay:

  Geometric decay coefficient in `[0, 1]`. See
  [`decay_from_half_life()`](https://elkronos.github.io/mediamix/reference/decay_vocabulary.md).

- normalise:

  Should the weights sum to 1? See the *Normalisation* section of
  [`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md)
  for what this does to the interpretation of a downstream coefficient.

## Value

A numeric vector of length `max_lag`, ordered from the current period to
the most distant lag.

## See also

[`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md),
[`adstock_weights_weibull()`](https://elkronos.github.io/mediamix/reference/adstock_weights_weibull.md)

## Examples

``` r
adstock_weights(max_lag = 5, decay = 0.5)
#> [1] 0.51612903 0.25806452 0.12903226 0.06451613 0.03225806

# Unnormalised: the current period always carries weight 1
adstock_weights(max_lag = 5, decay = 0.5, normalise = FALSE)
#> [1] 1.0000 0.5000 0.2500 0.1250 0.0625

# Normalised weights sum to 1, so the series level is preserved
sum(adstock_weights(max_lag = 10, decay = 0.7))
#> [1] 1
```
