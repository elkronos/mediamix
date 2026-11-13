# Return on investment and marginal return

`roi()` divides each channel's total contribution by its total spend.
`mroi()` asks the different and more useful question of what the *next*
currency unit would return at current spend.

## Usage

``` r
roi(contributions, spend, by = NULL, index = NULL, periods = NULL)

mroi(spend_level, coefficient, type = "hill", delta = 0.01, ...)
```

## Arguments

- contributions:

  A data frame from
  [`contributions()`](https://elkronos.github.io/mediamix/reference/contributions.md),
  or a named numeric vector of total contribution per channel.

- spend:

  A data frame of raw (untransformed) spend with one column per channel
  and one row per period, or a named numeric vector of total spend per
  channel. Spend columns are read as numbers: a factor is read by its
  labels, and text that is not a number is an error.

- by:

  Optional group of each row of `spend`, for one row per channel per
  group: the name of a column of `spend`, or a vector (or a data frame
  of key columns) with one value per row of `spend`, as given to
  [`contributions()`](https://elkronos.github.io/mediamix/reference/contributions.md).
  `contributions` must then come from
  [`contributions()`](https://elkronos.github.io/mediamix/reference/contributions.md)
  with `by`, and its `group` column is matched to these groups.

- index:

  Optional period of each row of `spend`: the name of a column of
  `spend`, or a vector with one value per row, as given to
  [`contributions()`](https://elkronos.github.io/mediamix/reference/contributions.md).
  With `index`, spend and contributions are matched period by period,
  within each group when `by` is supplied, and periods that appear in
  only one of them are left out, with a message.

- periods:

  Optional vector of the periods to include, compared with the `period`
  column of `contributions` and with `index`, so that one year or one
  quarter of a longer decomposition can be reported. When `spend` is a
  data frame, `periods` needs `index`. When `spend` gives totals,
  `periods` selects contributions only, and the totals must cover the
  same periods.

- spend_level:

  Current spend level for the channel, in the same units the saturation
  curve was fitted on: the adstocked level, for a curve applied after
  adstock as in
  [`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md).

- coefficient:

  The channel's fitted coefficient.

- type:

  Saturation curve type, as in
  [`saturate()`](https://elkronos.github.io/mediamix/reference/saturation.md),
  or `"none"` for a linear response.

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
are omitted, and reported when that happens. With `by`, a `group` column
comes first and there is one row per channel per group: groups in the
order they first appear in `contributions`, channels by descending ROI
within each. Groups found in only one of the two inputs are omitted,
with a message. `mroi()` returns a single number.

Each channel's contribution and spend must cover the same periods. When
`contributions` comes from
[`contributions()`](https://elkronos.github.io/mediamix/reference/contributions.md)
and `spend` is a data frame without `index`, `roi()` warns if the number
of periods decomposed differs from the number of rows of `spend`, group
by group with `by`. Give `index` to match the two period by period
instead.

## Details

Average ROI and marginal ROI answer different questions and are
routinely confused. Average ROI is a scorecard: what did this channel
return over the period as a whole. Marginal ROI is the decision
variable: what would the next unit return. Only the second one should
drive a reallocation, and the two can point in opposite directions.

Where they sit relative to each other depends on the shape of the curve.

For a *concave* response
([`saturate_hill()`](https://elkronos.github.io/mediamix/reference/saturation.md)
with `shape <= 1`, Michaelis–Menten, negative exponential,
[`saturate_power()`](https://elkronos.github.io/mediamix/reference/saturation.md)),
marginal ROI is below average ROI everywhere, and the gap widens the
further up the curve a channel sits. Reallocating on average ROI then
systematically over-funds channels that are already saturated, which are
exactly the channels that look best on an average-ROI table.

For an *S-shaped* response
([`saturate_hill()`](https://elkronos.github.io/mediamix/reference/saturation.md)
with `shape > 1`, the usual way to represent a threshold effect), the
curve is *convex* below its inflection point, and there marginal ROI is
*above* average ROI. A channel in that region has not saturated: each
extra pound works harder than the pounds already spent, so a cut there
loses more per pound than the average return suggests. Whether the
channel should get more budget still depends on how its marginal return
compares with other channels' and with break-even.

`mroi()` differentiates the fitted response curve numerically, so it
works for any curve
[`saturate()`](https://elkronos.github.io/mediamix/reference/saturation.md)
supports, with a step of `delta` times `spend_level` that never falls
below a small fraction of the curve's own spend parameter (`half_max`,
`km`, or the half-life of `rate`). At `spend_level = 0` it is finite and
correct wherever the curve's slope at zero is finite: Hill curves with
`shape >= 1`, Michaelis–Menten, exponential and `"none"`. A Hill curve
with `shape < 1` and a power curve with `exponent < 1` rise infinitely
steeply at zero, and there `mroi()` returns the average slope over a
short first step, which is large but not a derivative.

## Marginal return when the regressor was adstocked

`mroi()` is the slope of the coefficient times the saturation curve at a
single level of the curve's input, the *adstocked* media. It is a
property of the curve, not yet a return on spend, and two things
separate the two.

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

spec <- lapply(stats::setNames(channels, channels), function(ch) list(
  adstock = list(decay = truth$decay[[ch]]),
  saturation = list(half_max = truth$half_max[[ch]],
                    shape = truth$shape[[ch]])
))
transformed <- transform_plan(north[channels], spec)

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

# One row per channel per year: give contributions() and roi() the same `by`
year <- format(north$date, "%Y")
by_year <- contributions(transformed, fit, index = north$date, by = year)
roi(by_year, north[channels], by = year)
#>    group channel contribution spend      roi
#> 1   2023  social     41851.81 11341 3.690310
#> 2   2023  search     69308.42 24754 2.799888
#> 3   2023   video     34499.16 19144 1.802087
#> 4   2023 display     12225.40  8055 1.517741
#> 5   2023      tv    113145.78 93732 1.207120
#> 6   2024  social     48680.52 14037 3.468014
#> 7   2024  search     71022.56 25540 2.780836
#> 8   2024   video     34585.76 17499 1.976442
#> 9   2024 display     14455.69  9626 1.501734
#> 10  2024      tv    112665.80 77978 1.444841
#> 11  2025  social     42176.56 11338 3.719929
#> 12  2025  search     67975.80 24266 2.801277
#> 13  2025   video     33376.25 16605 2.010012
#> 14  2025 display     14171.79  9256 1.531092
#> 15  2025      tv    123817.28 95268 1.299673

# The last year only, with spend matched to contributions week by week
roi(contrib, north[c("date", channels)], index = "date",
    periods = north$date[year == "2025"])
#>   channel contribution spend      roi
#> 1  social     42176.56 11338 3.719929
#> 2  search     67975.80 24266 2.801277
#> 3   video     33376.25 16605 2.010012
#> 4 display     14171.79  9256 1.531092
#> 5      tv    123817.28 95268 1.299673

# Average and marginal return are different questions. marginal_roi()
# re-runs the whole transform on a 1% larger budget, so carryover and
# flighting are both accounted for; its `roi` column matches
# roi(contrib, north[channels]). Display's average pound returns about 1.5,
# but its next pound returns less than it costs.
marginal_roi(north, stats::coef(fit), spec = spec)
#>   channel  spend contribution      roi      mroi
#> 1      tv 266978    349628.86 1.309579 1.2534121
#> 2   video  53248    102461.17 1.924226 1.1960500
#> 3  search  74560    208306.77 2.793814 1.3489844
#> 4  social  36716    132708.89 3.614470 2.0891914
#> 5 display  26937     40852.88 1.516608 0.9324061
```
