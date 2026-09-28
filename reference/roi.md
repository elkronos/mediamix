# Return on investment and marginal return

`roi()` divides each channel's total contribution by its total spend.
`mroi()` asks the different and more useful question of what the *next*
currency unit would return at current spend.

## Usage

``` r
roi(contributions, spend)

mroi(spend_level, coefficient, type = "hill", delta = 0.01, ...)
```

## Arguments

- contributions:

  A data frame from
  [`contributions()`](https://elkronos.github.io/mediamix/reference/contributions.md),
  or a named numeric vector of total contribution per channel.

- spend:

  A data frame of raw (untransformed) spend with one column per channel,
  or a named numeric vector of total spend per channel.

- spend_level:

  Current spend level for the channel, in the same units the saturation
  curve was fitted on.

- coefficient:

  The channel's fitted coefficient.

- type:

  Saturation curve type.

- delta:

  Increment used for the numerical derivative, as a fraction of
  `spend_level`.

- ...:

  Passed to
  [`saturate()`](https://elkronos.github.io/mediamix/reference/saturation.md),
  for example `half_max` and `shape`.

## Value

`roi()` returns a data frame with columns `channel`, `contribution`,
`spend` and `roi`, ordered by descending ROI, with `roi` set to `NA`
where spend is zero. Channels appearing in only one of the two inputs
are omitted, and reported when that happens. `mroi()` returns a single
number.

## Details

Average ROI and marginal ROI answer different questions and are
routinely confused. Average ROI is a scorecard: what did this channel
return over the period as a whole. Marginal ROI is the decision
variable: what would the next unit return. Only the second one should
drive a reallocation, and the two can point in opposite directions.

Where they sit relative to each other depends on the shape of the curve,
and it is worth being precise because the usual summary of this is
wrong.

For a **concave** response –
[`saturate_hill()`](https://elkronos.github.io/mediamix/reference/saturation.md)
with `shape = 1`, Michaelis–Menten, negative exponential,
[`saturate_power()`](https://elkronos.github.io/mediamix/reference/saturation.md)
– marginal ROI is below average ROI everywhere, and the gap widens the
further up the curve a channel sits. Reallocating on average ROI then
systematically over-funds channels that are already saturated, which are
exactly the channels that look best on an average-ROI table.

For an **S-shaped** response –
[`saturate_hill()`](https://elkronos.github.io/mediamix/reference/saturation.md)
with `shape > 1`, the usual way to represent a threshold effect – the
curve is *convex* below its inflection point, and there marginal ROI is
**above** average ROI. A channel in that region is under-funded: each
extra pound works harder than the pounds already spent, because the
channel has not yet reached the pressure at which it starts to pay.
Ruling that out by assuming marginal is always lower is how a threshold
channel stays starved.

`mroi()` differentiates the fitted response curve numerically, so it
works for any curve
[`saturate()`](https://elkronos.github.io/mediamix/reference/saturation.md)
supports, and it is finite and correct at `spend_level = 0`.

## Marginal return when the regressor was adstocked

`mroi()` is the slope of the saturation curve with respect to the
quantity the coefficient multiplies – the *transformed* media – at a
single level. It is a property of the curve, not yet a return on spend,
and two things separate the two.

Carryover spreads a unit of spend over many periods. Under a normalised
kernel the weights sum to one, so the *total* extra response to one more
unit of spend is roughly the curve's slope, arriving over the kernel's
length. Multiplying by the kernel's first weight (`1 - decay`) gives
only the response inside the period of spend; comparing that with an
average ROI that counts every period's carryover compares a part with a
whole, and understates slow channels several-fold.

And the slope at the *mean* adstocked level is not the mean of the slope
across periods, which matters for flighted media and S-shaped curves.

[`marginal_roi()`](https://elkronos.github.io/mediamix/reference/marginal_roi.md)
handles both by re-running the whole transform on a slightly larger
budget. Use it for anything that feeds a budget decision, and use
`mroi()` to read the shape of a curve.

## See also

[`marginal_roi()`](https://elkronos.github.io/mediamix/reference/marginal_roi.md)
for marginal return on spend through the full transform,
[`contributions()`](https://elkronos.github.io/mediamix/reference/contributions.md),
[`response_curve()`](https://elkronos.github.io/mediamix/reference/response_curve.md),
[`spend_for()`](https://elkronos.github.io/mediamix/reference/response_curve.md)

## Examples

``` r
data(mm_weekly)
north <- mm_weekly[mm_weekly$geo == "north", ]
channels <- c("tv", "video", "search", "social", "display")
truth <- attr(mm_weekly, "truth")

transformed <- as.data.frame(Map(
  function(x, d, h, sh) media_transform(
    x, adstock = list(decay = d),
    saturation = list(half_max = h, shape = sh)),
  north[channels], truth$decay[channels], truth$half_max[channels],
  truth$shape[channels]
))

model_data <- transformed
model_data$week <- seq_len(nrow(model_data))
model_data$price <- north$price
model_data$seasonality <- north$seasonality
model_data$holiday <- north$holiday

fit <- stats::lm(north$revenue ~ ., data = model_data)
contrib <- contributions(transformed, fit, index = north$date)

roi(contrib, north[channels])
#>   channel contribution  spend      roi
#> 1  social    132708.89  36716 3.614470
#> 2  search    208306.77  74560 2.793814
#> 3   video    102461.17  53248 1.924226
#> 4 display     40852.88  26937 1.516608
#> 5      tv    349628.86 266978 1.309579

# Average and marginal return are different questions. marginal_roi()
# re-runs the whole transform on a 1% larger budget, so carryover and
# flighting are both accounted for. Display's average pound returns about
# 1.5, but its next pound returns less than it costs.
avg <- roi(contrib, north[channels])
for (ch in channels) {
  m <- marginal_roi(north[[ch]], coefficient = stats::coef(fit)[[ch]],
                    adstock = list(decay = truth$decay[[ch]]),
                    saturation = list(half_max = truth$half_max[[ch]],
                                      shape = truth$shape[[ch]]))
  cat(sprintf("%-8s average %.2f  marginal %.2f\n",
              ch, avg$roi[avg$channel == ch], m$mroi))
}
#> tv       average 1.31  marginal 1.25
#> video    average 1.92  marginal 1.20
#> search   average 2.79  marginal 1.35
#> social   average 3.61  marginal 2.09
#> display  average 1.52  marginal 0.93
```
