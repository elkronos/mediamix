# Combine journey tables

Stacks journey tables built by separate
[`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md)
calls, such as monthly batches of one event log. Every call numbers each
customer's journeys from 1, so the same `path_id` names different
journeys in different tables, and stacking them with
[`rbind()`](https://rdrr.io/r/base/cbind.html) would merge those
journeys. `bind_paths()` keeps them apart by prefixing each table's
journey ids with a batch label.

## Usage

``` r
bind_paths(..., rekey = TRUE)

# S3 method for class 'mm_paths'
rbind(..., deparse.level = 1)
```

## Arguments

- ...:

  `mm_paths` objects to combine, or a single list of them. When every
  table is named the names label the batches; otherwise the batches are
  labelled `1`, `2` and so on.

- rekey:

  Prefix journey ids with the batch label? Defaults to `TRUE`, which
  turns `"a#1"` from the batch labelled `jan` into `"jan:a#1"`. With
  `FALSE` ids are kept as they are, and an id found in more than one
  table is an error.

- deparse.level:

  Ignored; present for compatibility with
  [`rbind()`](https://rdrr.io/r/base/cbind.html).

## Value

An `mm_paths` object with every table's rows. Construction counts
describe one
[`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md)
call each, so they are dropped and
[`path_summary()`](https://elkronos.github.io/mediamix/reference/path_summary.md)
reports the input-side columns as `NA`. Settings are kept when every
table was built with the same ones.

## Details

A journey that crosses a batch boundary is two journeys here, because
each batch was split on its own. When that happens often, build the
journeys from the combined event log instead.

The tables must share their columns, their time units and the class of
their timestamps. [`rbind()`](https://rdrr.io/r/base/cbind.html) on
journey tables behaves like `bind_paths(rekey = FALSE)`: ids are kept,
and an id found in more than one table is an error rather than a merged
journey.
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
cannot tell the tables apart, and aborts when the combined rows repeat a
journey's touch ranks.

## See also

[`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md)

## Examples

``` r
jan <- data.frame(id = "a", ch = c("tv", "search"), ts = c(1, 2),
                  conv = c(0, 0))
feb <- data.frame(id = "a", ch = c("email", "display"), ts = c(40, 41),
                  conv = c(0, 1))
p_jan <- build_paths(jan, id = "id", channel = "ch", timestamp = "ts",
                     conversion = "conv")
p_feb <- build_paths(feb, id = "id", channel = "ch", timestamp = "ts",
                     conversion = "conv")
unique(p_jan$path_id)
#> [1] "a#1"
unique(p_feb$path_id)
#> [1] "a#1"

both <- bind_paths(jan = p_jan, feb = p_feb)
unique(both$path_id)
#> [1] "jan:a#1" "feb:a#1"
sum(credit_linear(both)$credit)
#> [1] 1
```
