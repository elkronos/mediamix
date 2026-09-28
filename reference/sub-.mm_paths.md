# Subset a journey table

Behaves exactly like subsetting a data frame, with one addition: if the
result no longer carries the columns that make a journey table a journey
table, the `mm_paths` class is dropped and a plain data frame is
returned. This stops a column subset from producing an object that
claims to be an `mm_paths` but cannot answer any question about
journeys.

## Usage

``` r
# S3 method for class 'mm_paths'
x[...]
```

## Arguments

- x:

  An `mm_paths` object.

- ...:

  Passed to the data frame method.

## Value

An `mm_paths` object when the structural columns survive, otherwise a
plain data frame.

## Details

When a row subset removes part of a journey – dropping one channel, say
– `touch_rank` and `touch_n` are recomputed from the touches that
remain, so the result is a valid journey table in its own right and
every credit rule sums to 1 per converting journey. The first remaining
touch becomes rank 1. Convert with
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) first if
you want to inspect the original ranks of a partial selection.

## See also

[`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md),
[`path_summary()`](https://elkronos.github.io/mediamix/reference/path_summary.md)

## Examples

``` r
data(mm_events)
paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                     timestamp = "timestamp", conversion = "conversion")

# Row subset: still a journey table
class(paths[paths$converted, ])
#> [1] "mm_paths"   "data.frame"

# Column subset that drops the structure: plain data frame
class(paths[, c("channel", "touch_rank")])
#> [1] "data.frame"
```
