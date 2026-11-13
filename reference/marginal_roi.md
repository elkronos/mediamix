# Average and marginal return by counterfactual simulation

Computes a channel's average return and its marginal return by
re-running the full media transform (carryover *and* saturation) on a
counterfactually increased spend plan, rather than by differentiating
the saturation curve at a single point. This is the definition of
marginal ROI used in the Bayesian MMM literature (Jin et al., 2017) and
in Google's Meridian: the incremental response from a small proportional
increase in spend, divided by the incremental spend.

## Usage

``` r
marginal_roi(
  spend,
  coefficient,
  adstock = list(kernel = "geometric", decay = 0.5),
  saturation = list(type = "none"),
  by = NULL,
  lift = 0.01,
  rows = NULL,
  extend = 0L,
  spec = NULL
)
```

## Arguments

- spend:

  Numeric vector of raw spend for one channel, in time order. Or a data
  frame with one row per period and a column of spend per channel, to
  compute every channel in one call. Spend must be complete, finite and
  non-negative.

- coefficient:

  The channel's fitted coefficient on the transformed regressor. When
  `spend` is a data frame, a numeric vector of coefficients named by
  channel: `coef(fit)` will do with `spec`, but without `spec` every
  name is taken as a channel, so pass only the channels' coefficients.

- adstock, saturation:

  Lists describing the transform, exactly as for
  [`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md).
  They must match the transform the model was fitted on. When `spend` is
  a data frame and `spec` is `NULL`, every channel uses them.

- by:

  Optional grouping vector, as for
  [`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md),
  one element per element of `spend`. When `spend` is a data frame, one
  element or row per row, or column names of `spend`, as for
  [`transform_plan()`](https://elkronos.github.io/mediamix/reference/transform_plan.md).

- lift:

  Proportional increase in spend used for the marginal return. The
  default `0.01` asks what a 1% larger budget would have returned.

- rows:

  Optional logical or integer index of the periods whose spend is
  increased (the planning window). Defaults to every period. Response is
  always summed over the whole series, so carryover from the window into
  later periods is counted.

- extend:

  Number of zero-spend periods appended to the end of the series (to
  each group, when `by` is supplied) before summing response, so that
  carryover from late spend is allowed to play out. At most 1,000,000.
  The default `0` counts response only inside the observed window, as
  [`contributions()`](https://elkronos.github.io/mediamix/reference/contributions.md)
  and [`roi()`](https://elkronos.github.io/mediamix/reference/roi.md)
  do, so the average return matches theirs. It also drops the carryover
  of late spend, which understates the marginal return of slow channels;
  see "Carryover after the window". Set it to
  [`effective_window()`](https://elkronos.github.io/mediamix/reference/decay_vocabulary.md)
  of the decay to measure long-run return.

- spec:

  Optional list with one transform per channel, named by column, as for
  [`transform_plan()`](https://elkronos.github.io/mediamix/reference/transform_plan.md):
  each entry holds `adstock` and `saturation`, and optionally `order`.
  Only when `spend` is a data frame, and in place of `adstock` and
  `saturation`, which must then be left out. The channels are then the
  names of `spec`, and `coefficient` may hold other names, which are
  ignored. Without `spec`, the channels are the names of `coefficient`.

## Value

A one-row data frame with columns `spend` (total spend over the series),
`contribution` (the channel's total response), `roi` (their ratio, the
average return) and `mroi` (incremental response per unit of incremental
spend, when the spend in `rows` is increased by `lift`). A window with
no spend is an error.

When `spend` is a data frame, one row per channel, in the order of
`spec` or of `coefficient`, with a leading `channel` column and then the
same columns. A channel with no spend in the window has an `mroi` of
`NA`, and a message names it; with no spend at all, its `roi` is `NA`
too.

## Details

Differentiating the saturation curve, as
[`mroi()`](https://elkronos.github.io/mediamix/reference/roi.md) does,
answers a narrower question: the slope of the curve at one level of
*adstocked* media. Turning it into a return on *spend* needs two further
steps that are easy to get wrong.

- **Carryover.** A unit spent this week enters this week's adstock with
  the kernel's first weight only (`1 - decay` for a normalised geometric
  kernel), but it also enters every later week's adstock. Under a
  normalised kernel those weights sum to one, so the *total* marginal
  return is close to the curve's slope, not to the slope times
  `1 - decay`. Multiplying by the first weight gives the return inside
  the week of spend and ignores the rest; for a slow channel that
  understates its marginal return several-fold, and a reallocation
  driven by it moves money away from exactly the channels whose effect
  arrives late.

- **Averaging over periods.** The curve's slope at the *mean* adstocked
  level is not the mean of its slope across periods. Flighted media
  spends some weeks near zero and some near saturation, and for an
  S-shaped curve the two can differ by a large factor (Jensen's
  inequality).

Simulation handles both by construction, for any kernel and any curve
[`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md)
supports, and it is what makes average and marginal return directly
comparable: both are totals over the same periods.

## Carryover after the window

With the default `extend = 0`, response that spend would produce after
the last observed period is not counted. The loss falls on spend near
the end of the window and grows with the decay. For `mm_weekly`'s north
television (decay 0.85) at its true parameters, the marginal return with
`extend = 0` is 1.084, about 4% below the 1.129 it reaches with
`extend = effective_window(0.85, 0.99)`; for search (decay 0.15) the
difference is under 0.5%.

The default makes the average return here equal the one
[`roi()`](https://elkronos.github.io/mediamix/reference/roi.md) reports
from
[`contributions()`](https://elkronos.github.io/mediamix/reference/contributions.md).
For a budget decision, where each unit of spend should be credited with
all the response it produces, pass
`extend = effective_window(decay, 0.99)`.

## What this is not

The answer is only as good as the fitted curve, and a curve is
identified only over the spend levels the data contains. A marginal
return is a local quantity: a 1% change is the kind of question the data
can speak to; a 50% change is extrapolation, which is why `lift`
defaults to 0.01.

## References

Jin, Y., Wang, Y., Sun, Y., Chan, D. and Koehler, J. (2017). Bayesian
methods for media mix modeling with carryover and shape effects. Google
Inc. <https://research.google/pubs/pub46001/>

## See also

[`roi()`](https://elkronos.github.io/mediamix/reference/roi.md),
[`mroi()`](https://elkronos.github.io/mediamix/reference/roi.md),
[`response_curve()`](https://elkronos.github.io/mediamix/reference/response_curve.md),
[`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md),
[`simulate_plan()`](https://elkronos.github.io/mediamix/reference/simulate_plan.md),
[`allocate_budget()`](https://elkronos.github.io/mediamix/reference/allocate_budget.md)

## Examples

``` r
data(mm_weekly)
north <- mm_weekly[mm_weekly$geo == "north", ]
truth <- attr(mm_weekly, "truth")

# Television: long carryover, S-shaped response
marginal_roi(
  north$tv, coefficient = truth$beta[["tv"]],
  adstock = list(decay = truth$decay[["tv"]]),
  saturation = list(half_max = truth$half_max[["tv"]],
                    shape = truth$shape[["tv"]])
)
#>    spend contribution      roi     mroi
#> 1 266978     302361.4 1.132533 1.083959

# Letting the carryover from the final weeks play out
marginal_roi(
  north$tv, coefficient = truth$beta[["tv"]],
  adstock = list(decay = truth$decay[["tv"]]),
  saturation = list(half_max = truth$half_max[["tv"]],
                    shape = truth$shape[["tv"]]),
  extend = effective_window(truth$decay[["tv"]], 0.99)
)
#>    spend contribution      roi     mroi
#> 1 266978     311695.6 1.167495 1.128674

# Only the last quarter's budget changes
last_q <- seq_len(nrow(north)) > nrow(north) - 13
marginal_roi(
  north$tv, coefficient = truth$beta[["tv"]],
  adstock = list(decay = truth$decay[["tv"]]),
  saturation = list(half_max = truth$half_max[["tv"]],
                    shape = truth$shape[["tv"]]),
  rows = last_q
)
#>    spend contribution      roi      mroi
#> 1 266978     302361.4 1.132533 0.7083583

# Every channel in one call, each with its own transform
channels <- c("tv", "video", "search", "social", "display")
spec <- lapply(stats::setNames(channels, channels), function(ch) list(
  adstock = list(decay = truth$decay[[ch]]),
  saturation = list(half_max = truth$half_max[[ch]],
                    shape = truth$shape[[ch]])
))
marginal_roi(north, truth$beta, spec = spec)
#>   channel  spend contribution      roi     mroi
#> 1      tv 266978    302361.38 1.132533 1.083959
#> 2   video  53248    130431.45 2.449509 1.522553
#> 3  search  74560    325735.31 4.368768 2.109446
#> 4  social  36716    105769.75 2.880753 1.665098
#> 5 display  26937     48394.19 1.796570 1.104525
```
