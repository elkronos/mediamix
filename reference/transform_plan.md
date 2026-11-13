# Transform every channel of a media plan

Applies
[`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md)
to each channel column of a plan, each with its own carryover and
saturation, and returns the plan with those columns replaced. It is the
step between a table of spend and the regressors a model is fitted on,
and the step
[`simulate_plan()`](https://elkronos.github.io/mediamix/reference/simulate_plan.md)
and
[`allocate_budget()`](https://elkronos.github.io/mediamix/reference/allocate_budget.md)
repeat for every plan they evaluate.

## Usage

``` r
transform_plan(plan, spec, by = NULL)
```

## Arguments

- plan:

  A data frame with one row per period and a numeric column of spend for
  each channel. Other columns, such as a date or a geography, may be
  present.

- spec:

  A list with one entry per channel, named after the channel's column in
  `plan`. Each entry is a list with elements `adstock` and `saturation`,
  in the form
  [`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md)
  takes them, and optionally `order`. An element left out takes
  [`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md)'s
  default, so an entry without `adstock` gets geometric carryover at
  `decay = 0.5`; write `adstock = NULL` for none.

- by:

  Optional grouping, so that carryover runs within each group, such as a
  geography, and never from one into the next: column names of `plan`,
  or a vector or data frame of vectors with one element or row per row
  of `plan`, as for
  [`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md).

## Value

`plan`, with each column named in `spec` replaced by its transformed
series. The rows keep their order and every other column is unchanged.

## Details

A spec entry may hold only `adstock`, `saturation` and `order`. Any
other element name is an error, so a misspelt `saturaton` cannot quietly
leave a channel unsaturated. The grouping is set once, by `by`, for
every channel, and a `by` inside an entry's `adstock` list is an error.

A character `by` whose elements all name columns of `plan` is read as
those columns; any other `by` is a grouping vector, or a data frame of
them.
[`simulate_plan()`](https://elkronos.github.io/mediamix/reference/simulate_plan.md),
[`reallocate()`](https://elkronos.github.io/mediamix/reference/reallocate.md)
and
[`allocate_budget()`](https://elkronos.github.io/mediamix/reference/allocate_budget.md)
read `by` the same way.

An error or warning from one channel's transform is reported with the
channel's name, followed by the message
[`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md)
gave.

## See also

[`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md),
[`simulate_plan()`](https://elkronos.github.io/mediamix/reference/simulate_plan.md),
[`allocate_budget()`](https://elkronos.github.io/mediamix/reference/allocate_budget.md),
[`marginal_roi()`](https://elkronos.github.io/mediamix/reference/marginal_roi.md)

## Examples

``` r
data(mm_weekly)
north <- mm_weekly[mm_weekly$geo == "north", ]
truth <- attr(mm_weekly, "truth")
channels <- c("tv", "video", "search", "social", "display")

# One entry per channel, here with the parameters the data was simulated
# from.
spec <- lapply(stats::setNames(channels, channels), function(ch) list(
  adstock = list(decay = truth$decay[[ch]]),
  saturation = list(half_max = truth$half_max[[ch]],
                    shape = truth$shape[[ch]])
))

media <- transform_plan(north, spec)
round(head(media[channels], 4), 4)
#>       tv  video search social display
#> 1 0.0000 0.1663 0.3435 0.3175  0.0000
#> 2 0.0000 0.1225 0.5001 0.4202  0.2958
#> 3 0.0441 0.0890 0.5501 0.2459  0.1877
#> 4 0.0343 0.0640 0.5598 0.3838  0.3349

# A panel: carryover runs within each geography
panel <- transform_plan(mm_weekly, spec, by = "geo")
nrow(panel) == nrow(mm_weekly)
#> [1] TRUE
```
