# Plot attribution shares across rules

One row per channel. The grey bar spans the lowest to the highest share
any rule gives it; first-touch, last-touch and Markov shares are marked,
and the other rules appear as grey ticks. A long bar is a channel whose
value depends on the convention, not the data.

## Usage

``` r
# S3 method for class 'mm_attribution'
plot(x, n = 10L, ...)
```

## Arguments

- x:

  A data frame from
  [`attribute()`](https://elkronos.github.io/mediamix/reference/attribute.md).

- n:

  Show the `n` channels with the largest mean share.

- ...:

  Graphical arguments: `main`, `xlab` and `ylab` replace the defaults,
  and `col` gives the colours of the first-touch, last-touch and Markov
  markers. Others are ignored.

## Value

`x`, invisibly.

## Details

A missing share is left out of the plot with a warning. The mean share
that ranks the channels is taken over the shares that are present, and a
channel with no share at all ranks last.

## Examples

``` r
data(mm_events)
paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                     timestamp = "timestamp", conversion = "conversion")
plot(attribute(paths))
```
