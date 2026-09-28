# Journey table summary

The construction accounting for an `mm_paths` object: how many events
went in, how many journeys came out, and how many conversions were lost
to filtering along the way.

## Usage

``` r
path_summary(paths)
```

## Arguments

- paths:

  An `mm_paths` object from
  [`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md).

## Value

A one-row data frame with the columns

- `events_in`, `events_out`:

  Rows supplied, and rows retained.

- `journeys`, `converting_journeys`:

  Journeys built, and how many ended in a conversion that can still be
  credited.

- `mean_length`, `median_length`, `max_length`:

  Journey length in touchpoints.

- `direct_events`:

  Events whose channel matched `direct_labels`.

- `conversion_events`:

  Conversion *events* in the input. This exceeds `conversions_observed`
  when several conversions fall inside one journey, which happens under
  `split_on = "none"` or `"gap"`.

- `conversions_observed`:

  Journeys that contained a conversion.

- `conversions_unattributable`:

  Converting journeys that lost every touchpoint to a direct-traffic
  rule, and so cannot be credited to any channel. That number should be
  reported, not absorbed.

On a subset of a journey table the construction counts no longer apply,
and the columns describing the input are `NA` rather than the parent's
values.

## See also

[`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md),
[`path_diagnostics()`](https://elkronos.github.io/mediamix/reference/path_diagnostics.md)

## Examples

``` r
data(mm_events)
paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                     timestamp = "timestamp", conversion = "conversion")
path_summary(paths)
#>   events_in events_out journeys converting_journeys mean_length median_length
#> 1     20799      17306     6137                2741    2.819945             3
#>   max_length direct_events conversion_events conversions_observed
#> 1          9          1597              2741                 2741
#>   conversions_unattributable
#> 1                          0
```
