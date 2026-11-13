# Predicted media response of a plan

Runs each channel of a spend plan through its transform and multiplies
the result by the channel's coefficient, giving the response the model
predicts from each channel. With `baseline`, it also reports the change
from another plan, which is how a proposed reallocation is checked
through the curves rather than by multiplying by a marginal return.

## Usage

``` r
simulate_plan(
  plan,
  spec,
  coefficients,
  by = NULL,
  baseline = NULL,
  extend = 0L
)
```

## Arguments

- plan:

  A data frame with one row per period and a numeric column of spend for
  each channel in `spec`. Spend must be complete, finite and
  non-negative. Other columns may be present.

- spec:

  A list with one entry per channel, named after the channel's column in
  `plan`. Each entry is a list with elements `adstock` and `saturation`,
  in the form
  [`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md)
  takes them, and optionally `order`. An element left out takes
  [`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md)'s
  default, so an entry without `adstock` gets geometric carryover at
  `decay = 0.5`; write `adstock = NULL` for none.

- coefficients:

  A numeric vector of the channels' fitted coefficients, named by
  channel, such as `coef(fit)`. Every channel in `spec` needs one; other
  names, such as the intercept and the controls, are ignored.

- by:

  Optional grouping, so that carryover runs within each group, such as a
  geography, and never from one into the next: column names of `plan`,
  or a vector or data frame of vectors with one element or row per row
  of `plan`, as for
  [`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md).

- baseline:

  Optional plan to compare with, such as the current plan when `plan` is
  a proposed one. It needs the channel columns of `spec`. When `by` is
  given as a grouping vector, which applies to both plans, `baseline`
  needs as many rows as `plan`; column names are looked up in each plan.

- extend:

  Number of zero-spend periods appended to the end of the series (to
  each group, when `by` is supplied) before response is summed, as for
  [`marginal_roi()`](https://elkronos.github.io/mediamix/reference/marginal_roi.md).
  The default `0` counts response only inside the plan's periods.

## Value

A data frame with one row per channel in `spec`, in its order, and
columns `channel`, `spend` (the channel's total spend in `plan`) and
`response` (its coefficient times the sum of its transformed series).
With `baseline`, two more columns give `plan` minus `baseline`:
`spend_change` and `response_change`.

## Details

A channel's response is the quantity
[`marginal_roi()`](https://elkronos.github.io/mediamix/reference/marginal_roi.md)
divides by spend: its coefficient times the sum of its transformed
series over every row of the plan and any `extend` periods after it. It
leaves out the intercept and the controls, which a change of media plan
does not move, so the difference in response between two plans is the
difference in the KPI the model predicts for them. The sum of
`response_change` is the total gain or loss.

Two plans of different lengths can be compared, since each response is a
total; compare plans over the same periods for a like-for-like answer.

## See also

[`reallocate()`](https://elkronos.github.io/mediamix/reference/reallocate.md)
to build a plan that moves spend between channels,
[`allocate_budget()`](https://elkronos.github.io/mediamix/reference/allocate_budget.md),
[`marginal_roi()`](https://elkronos.github.io/mediamix/reference/marginal_roi.md),
[`transform_plan()`](https://elkronos.github.io/mediamix/reference/transform_plan.md)

## Examples

``` r
data(mm_weekly)
north <- mm_weekly[mm_weekly$geo == "north", ]
truth <- attr(mm_weekly, "truth")
channels <- c("tv", "video", "search", "social", "display")
spec <- lapply(stats::setNames(channels, channels), function(ch) list(
  adstock = list(decay = truth$decay[[ch]]),
  saturation = list(half_max = truth$half_max[[ch]],
                    shape = truth$shape[[ch]])
))

simulate_plan(north, spec, truth$beta)
#>   channel  spend  response
#> 1      tv 266978 302361.38
#> 2   video  53248 130431.45
#> 3  search  74560 325735.31
#> 4  social  36716 105769.75
#> 5 display  26937  48394.19

# Move a tenth of display's budget into search, keeping both channels'
# flighting, and compare the two plans
moved <- reallocate(north, from = "display", to = "search",
                    amount = 0.1 * sum(north$display))
change <- simulate_plan(moved, spec, truth$beta, baseline = north)
change
#>   channel    spend  response spend_change response_change
#> 1      tv 266978.0 302361.38          0.0           0.000
#> 2   video  53248.0 130431.45          0.0           0.000
#> 3  search  77253.7 331343.28       2693.7        5607.978
#> 4  social  36716.0 105769.75          0.0           0.000
#> 5 display  24243.3  45293.23      -2693.7       -3100.966
sum(change$response_change)
#> [1] 2507.012
```
