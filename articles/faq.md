# Frequently asked questions

Short answers to questions that come up when building a media mix model
or attributing conversions with mediamix. Each one names the functions
and arguments involved, and their help pages have the detail. The
examples use the package’s simulated data, `mm_weekly` and `mm_events`.

``` r

library(mediamix)
data(mm_weekly)
data(mm_events)
```

## Carryover

### Should the adstock kernel be normalised?

By default (`normalise = TRUE`) the kernel’s weights sum to 1, so a
steady spend of 100 a week adstocks to 100 once the filter has filled,
and a coefficient on the adstocked column reads per unit of spend. With
`normalise = FALSE`, the convention Robyn uses, the same spend
accumulates to 100 / (1 - decay), and the coefficient reads per unit of
accumulated media. The two series differ only by a constant factor, so
in a least-squares fit only the coefficient’s scale changes. A
saturation `half_max` given as an amount belongs to one convention and
is wrong for the other, while one set relative to the series’ own
reference level, as
[`step_saturation()`](https://elkronos.github.io/mediamix/reference/step_saturation.md)
sets it, gives the same curve under both. Keep one convention for a
project, and compare coefficients only between models fitted the same
way.

``` r

steady <- rep(100, 100)
c(normalised = tail(adstock_geometric(steady, decay = 0.8), 1),
  raw = tail(adstock_geometric(steady, decay = 0.8, normalise = FALSE), 1))
#> normalised        raw 
#>        100        500
```

### Why are the first weeks of an adstocked series low?

Every filter starts from zero unless told otherwise, as if nothing had
been spent before the first row. A slow channel takes weeks to fill: at
a decay of 0.85 a steady spend reaches 15% of its level in the first
week, and 90% only after `effective_window(0.85)`, 15 weeks. With spend
from before the series, pass
[`adstock_state()`](https://elkronos.github.io/mediamix/reference/adstock_state.md)
of it as `state`; without it,
[`adstock_steady_state()`](https://elkronos.github.io/mediamix/reference/adstock_steady_state.md)
gives the state that steady spend at the series’ early average would
have left. `warm_start = TRUE` in the tuners and `start = "steady"` in
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
apply the same correction. Robyn’s remedy, adstocking every row and
fitting the model on the later rows only, works here too.

``` r

x <- rep(100, 6)
rbind(cold = adstock_geometric(x, decay = 0.85),
      steady = adstock_geometric(x, decay = 0.85,
                                 state = adstock_steady_state(x, decay = 0.85)))
#>        [,1]   [,2]   [,3]  [,4]   [,5]   [,6]
#> cold     15  27.75  38.59  47.8  55.63  62.29
#> steady  100 100.00 100.00 100.0 100.00 100.00
```

### How long should `max_lag` be?

The default, `max_lag = Inf`, runs the geometric kernel as a recursive
filter and never truncates it, so there is nothing to choose. A finite
kernel should keep most of the effect:
`effective_window(decay, coverage)` is the smallest number of periods
that holds `coverage` of a geometric kernel’s total weight. The same
number checks a fitted decay, since an effective window that approaches
the length of the series is not identified by the data. The Weibull and
delayed kernels have no recursive form and always need a finite
`max_lag`;
[`effective_window()`](https://elkronos.github.io/mediamix/reference/decay_vocabulary.md)
of a comparable geometric decay is a starting point.

``` r

effective_window(c(tv = 0.85, search = 0.15), coverage = 0.9)
#>     tv search 
#>     15      2
effective_window(0.85, coverage = 0.99)
#> [1] 29
```

### How do I convert a half-life between daily and weekly data?

[`decay_from_half_life()`](https://elkronos.github.io/mediamix/reference/decay_vocabulary.md)
and
[`half_life()`](https://elkronos.github.io/mediamix/reference/decay_vocabulary.md)
take `period`, the spacing between rows in the unit the half-life is
stated in. A 21-day half-life on weekly data is
`decay_from_half_life(21, period = 7)`, the same decay as a three-week
half-life, and `half_life(decay, period = 7)` gives a weekly decay’s
half-life in days. To move a decay fitted on daily data to weekly data,
go through its half-life:

``` r

daily <- 0.9
days <- half_life(daily)
c(days = days, weekly = decay_from_half_life(days, period = 7),
  daily_to_the_7th = daily^7)
#>             days           weekly daily_to_the_7th 
#>           6.5788           0.4783           0.4783
```

The half-life is kept and the decay per row changes. Adstocking weekly
totals is still not the same as adstocking daily spend and summing it to
weeks, because the timing of spend within each week is lost.

### How do I choose the decay?

Choose it against the KPI, by cross-validation.
[`tune_carryover()`](https://elkronos.github.io/mediamix/reference/tune_carryover.md)
scores each candidate `(max_lag, decay)` for one channel by how well a
model fitted on the adstocked media predicts held-out KPI, on
forward-only splits, with trend, price and seasonality passed as
`controls`.
[`tune_carryover_joint()`](https://elkronos.github.io/mediamix/reference/tune_carryover_joint.md)
does the same for every channel inside one model, so no channel is
credited with another’s effect, and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) on either
result shows how sharply the data identifies each decay. To tune
carryover together with saturation and a model penalty, use
[`step_adstock_channels()`](https://elkronos.github.io/mediamix/reference/step_adstock_channels.md)
or
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
with
[`step_saturation()`](https://elkronos.github.io/mediamix/reference/step_saturation.md)
and `tune()` in a tidymodels workflow, as
[`vignette("tidymodels")`](https://elkronos.github.io/mediamix/articles/tidymodels.md)
shows; that search needs the tune, parsnip, workflows and yardstick
packages.

### What is the difference between `best` and `best_1se`?

`best` is the candidate with the lowest cross-validated error.
`best_1se` is the shortest carryover (lowest half-life, then shortest
kernel) whose error is within one standard error of `best`’s, the
one-standard-error rule of Breiman et al. (1984) and Hastie et
al. (2009), with the standard error taken from the paired difference
between the two over the same splits. Carryover is often weakly
identified, so the two can differ widely, and when they do the data
cannot tell them apart. The shorter one is the more cautious claim about
how long a channel keeps working, though not for every decision, since
it also lowers the channel’s marginal return. In
[`tune_carryover_joint()`](https://elkronos.github.io/mediamix/reference/tune_carryover_joint.md)
the rule is applied channel by channel, and `best_1se_metric` scores the
resulting set as a whole.

``` r

set.seed(42)
spend <- pmax(0, rnorm(120, 500, 250))
kpi <- 200 + 0.5 * adstock_geometric(spend, decay = 0.7) + rnorm(120, sd = 60)
tuned <- tune_carryover(spend, kpi, decays = seq(0.1, 0.9, by = 0.1))
rbind(best = tuned$best, one_se = tuned$best_1se)
#>        max_lag decay half_life metric std_err n_splits
#> best       Inf   0.8     3.106  54.75   4.548       60
#> one_se     Inf   0.6     1.357  56.31   5.219       60
```

The true decay here is 0.7, and the noise leaves the data unable to
separate 0.6 from 0.8.

### How do I adstock several geographies?

Pass the geography as `by`. Each group is filtered on its own, so
carryover never runs from the last week of one geography into the first
week of the next, and each group’s rows must be in time order, since the
transforms have no time index.
[`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md),
[`transform_plan()`](https://elkronos.github.io/mediamix/reference/transform_plan.md),
[`marginal_roi()`](https://elkronos.github.io/mediamix/reference/marginal_roi.md),
[`simulate_plan()`](https://elkronos.github.io/mediamix/reference/simulate_plan.md)
and
[`allocate_budget()`](https://elkronos.github.io/mediamix/reference/allocate_budget.md)
take `by` too, the tuners take `by` with `index`, and
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
takes `by` and sorts by its own `index`. Do not rely on
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
instead: the grouping does not survive into every place the transforms
run, such as a recipe’s
[`bake()`](https://recipes.tidymodels.org/reference/bake.html), and
[`diagnose_media()`](https://elkronos.github.io/mediamix/reference/diagnose_media.md)
ignores it with a message naming the `by` to pass.

``` r

spend <- c(100, 0, 0, 200, 0, 0)
geo <- rep(c("north", "south"), each = 3)
rbind(pooled = adstock_geometric(spend, decay = 0.5),
      by_geo = adstock_geometric(spend, decay = 0.5, by = geo))
#>        [,1] [,2] [,3]  [,4]  [,5]  [,6]
#> pooled   50   25 12.5 106.2 53.12 26.56
#> by_geo   50   25 12.5 100.0 50.00 25.00
```

### What happens when weeks are missing?

The transforms treat adjacent rows as adjacent periods. A missing week
lets the decay act once across what were two weeks, a repeated week is
carried over twice, and a row out of order mixes later spend into
earlier weeks, and none of these raises an error. Given the time column
as `date`,
[`diagnose_media()`](https://elkronos.github.io/mediamix/reference/diagnose_media.md)
counts each of them per series in its `dates` table and lists them in
`flags`, and
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
warns at [`prep()`](https://recipes.tidymodels.org/reference/prep.html)
when its index is unevenly spaced and stops on a repeated time point.
Add a row for every missing period before transforming, with zero spend
where the channel did not run. A missing value in a row that exists is
an error in the adstock functions by default; `na_action = "zero"` reads
it as no spend, and
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
does that by default.

``` r

gappy <- mm_weekly[-10, ]   # drops one week of north
diagnose_media(gappy, media = "tv", by = "geo", date = "date")$dates
#>     group rows      start        end period out_of_order duplicated missing
#> 1   north  155 2023-01-02 2025-12-22      7            0          0       1
#> 2 central  156 2023-01-02 2025-12-22      7            0          0       0
#> 3   south  156 2023-01-02 2025-12-22      7            0          0       0
#>   irregular
#> 1         1
#> 2         0
#> 3         0
```

## Saturation

### Does saturation go before or after adstock?

Saturation goes after adstock. A saturation curve caps what the media
pressure in a period can achieve, and that pressure includes carryover
from earlier weeks. Saturating first puts each week’s spend through the
curve on its own, so carryover never counts towards saturation, and with
`normalise = FALSE` the carried-over values can add up past the ceiling.
[`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md)
adstocks first by default and warns when asked for
`order = "saturate_then_adstock"`, and in a recipe
[`step_saturation()`](https://elkronos.github.io/mediamix/reference/step_saturation.md)
goes after
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md).
Robyn uses the same order.

### Why is `half_max` a fraction in `step_saturation()`?

In
[`saturate_hill()`](https://elkronos.github.io/mediamix/reference/saturation.md)
and
[`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md),
`half_max` is an amount of adstocked media, the level at which the
response reaches half its ceiling, so it depends on each channel’s
scale.
[`step_saturation()`](https://elkronos.github.io/mediamix/reference/step_saturation.md)
takes it as a fraction of each column’s reference level in the training
data, its largest value by default (`ref = "max"`), so one tunable value
means the same thing for every channel. A `half_max` of 0.4 there puts
the half-way point at 40% of the largest adstocked value seen in
training, and with `by` each geography gets a reference of its own.

## Reporting

### What does “(baseline)” mean?

[`contributions()`](https://elkronos.github.io/mediamix/reference/contributions.md)
gives each channel its coefficient times its transformed column, period
by period, and pools everything else the model predicts into a channel
called `"(baseline)"`: the intercept, trend, price, seasonality,
holidays and any other control. It is the model’s prediction minus the
media effect, which is what makes each period’s contributions sum to the
prediction. It holds whatever this decomposition does not attribute to
media, and reporting it as organic demand overstates what the model
knows. For periods the model was not fitted on, pass `newdata` so that
the baseline comes from
[`predict()`](https://rdrr.io/r/stats/predict.html); for a bare
coefficient vector it is the intercept alone.

``` r

north <- mm_weekly[mm_weekly$geo == "north", ]
truth <- attr(mm_weekly, "truth")
channels <- c("tv", "video", "search", "social", "display")
spec <- lapply(stats::setNames(channels, channels), function(ch) list(
  adstock = list(decay = truth$decay[[ch]]),
  saturation = list(half_max = truth$half_max[[ch]], shape = truth$shape[[ch]])
))
media <- transform_plan(north, spec)
media$week <- seq_len(nrow(media))
fit <- lm(revenue ~ tv + video + search + social + display + week + price +
            seasonality + holiday, data = media)

contrib <- contributions(media[channels], fit, index = media$date)
round(tapply(contrib$contribution, contrib$channel, sum))
#> (baseline)    display     search     social         tv      video 
#>    3190425      40853     208307     132709     349629     102461
```

### Should I use `roi()`, `marginal_roi()` or `mroi()`?

[`roi()`](https://elkronos.github.io/mediamix/reference/roi.md) is the
average return, each channel’s total contribution divided by its total
spend, which scores the period as a whole.
[`marginal_roi()`](https://elkronos.github.io/mediamix/reference/marginal_roi.md)
is the return on the next unit of spend. It re-runs carryover and
saturation on a budget 1% larger (`lift = 0.01`) and divides the extra
response by the extra spend, following Jin et al. (2017), so it is the
one to use when moving budget.
[`mroi()`](https://elkronos.github.io/mediamix/reference/roi.md) is the
slope of a saturation curve at one level of transformed media, a
property of the curve rather than a return on spend. Under a concave
curve the marginal return is below the average one everywhere; under an
S-shaped curve, below its inflection point, it can be above.

``` r

marginal_roi(north, coef(fit), spec = spec)
#>   channel  spend contribution   roi   mroi
#> 1      tv 266978       349629 1.310 1.2534
#> 2   video  53248       102461 1.924 1.1961
#> 3  search  74560       208307 2.794 1.3490
#> 4  social  36716       132709 3.614 2.0892
#> 5 display  26937        40853 1.517 0.9324
```

The `roi` column is the average return that
[`roi()`](https://elkronos.github.io/mediamix/reference/roi.md) reports
from these contributions, and `mroi` the marginal return beside it.

### Why does `marginal_roi()` stop counting at the last week?

By default (`extend = 0`) it sums response over the observed weeks only,
the weeks
[`contributions()`](https://elkronos.github.io/mediamix/reference/contributions.md)
and [`roi()`](https://elkronos.github.io/mediamix/reference/roi.md)
cover, so that its average return matches theirs. The carryover of spend
near the end of the data then falls outside the sum, which understates
the marginal return of slow channels. For a budget decision, pass
`extend = effective_window(decay, 0.99)`, which appends that many weeks
of zero spend so the late carryover plays out.

``` r

tv_mroi <- function(extend) {
  marginal_roi(north$tv, coefficient = coef(fit)[["tv"]],
               adstock = spec$tv$adstock, saturation = spec$tv$saturation,
               extend = extend)$mroi
}
c(observed_weeks = tv_mroi(0),
  with_run_out = tv_mroi(effective_window(truth$decay[["tv"]], 0.99)))
#> observed_weeks   with_run_out 
#>          1.253          1.305
```

### How do I report by geography or by year?

Give
[`contributions()`](https://elkronos.github.io/mediamix/reference/contributions.md)
the grouping as `by`, and pass the same grouping to
[`roi()`](https://elkronos.github.io/mediamix/reference/roi.md), which
then returns one row per channel per group. For one year or quarter of a
longer decomposition, give
[`roi()`](https://elkronos.github.io/mediamix/reference/roi.md) the
period of each row of spend as `index` and the periods to keep as
`periods`. `rows` in
[`marginal_roi()`](https://elkronos.github.io/mediamix/reference/marginal_roi.md)
raises spend only inside a planning window, and
[`simulate_plan()`](https://elkronos.github.io/mediamix/reference/simulate_plan.md)
and
[`allocate_budget()`](https://elkronos.github.io/mediamix/reference/allocate_budget.md)
take `by` for a panel.

``` r

year <- format(media$date, "%Y")
by_year <- contributions(media[channels], fit, index = media$date, by = year)
roi(by_year, north[channels], by = year)
#>    group channel contribution spend   roi
#> 1   2023  social        41852 11341 3.690
#> 2   2023  search        69308 24754 2.800
#> 3   2023   video        34499 19144 1.802
#> 4   2023 display        12225  8055 1.518
#> 5   2023      tv       113146 93732 1.207
#> 6   2024  social        48681 14037 3.468
#> 7   2024  search        71023 25540 2.781
#> 8   2024   video        34586 17499 1.976
#> 9   2024 display        14456  9626 1.502
#> 10  2024      tv       112666 77978 1.445
#> 11  2025  social        42177 11338 3.720
#> 12  2025  search        67976 24266 2.801
#> 13  2025   video        33376 16605 2.010
#> 14  2025 display        14172  9256 1.531
#> 15  2025      tv       123817 95268 1.300
```

### Where do intervals come from, and what do they leave out?

[`block_bootstrap()`](https://elkronos.github.io/mediamix/reference/block_bootstrap.md)
puts percentile intervals on any statistic computed from a weekly table,
such as coefficients, contributions or returns. It resamples blocks of
consecutive weeks, so the autocorrelation that carryover creates
survives (Künsch, 1989), then refits and recomputes through your
`statistic` function. Transform the media first and put the transformed
columns in the data, so that no block carries spend across a join. The
intervals cover sampling variation given the model and its transform
parameters; they leave out the uncertainty in choosing those parameters
and any error in the specification, and on short series they cover less
than their nominal level, so read them as a lower bound. Make blocks at
least as long as the longest channel’s
[`effective_window()`](https://elkronos.github.io/mediamix/reference/decay_vocabulary.md),
and for a panel pass `by`, so that every geography takes the same blocks
of weeks.

## Budgets

### How do I compare plans or split a budget?

[`simulate_plan()`](https://elkronos.github.io/mediamix/reference/simulate_plan.md)
runs each channel of a spend plan through its transform and coefficient,
and with `baseline` it reports each channel’s change against another
plan.
[`reallocate()`](https://elkronos.github.io/mediamix/reference/reallocate.md)
builds such a plan by moving an amount from one channel to another, and
[`allocate_budget()`](https://elkronos.github.io/mediamix/reference/allocate_budget.md)
splits a total across channels to maximise the predicted response within
`lower` and `upper` bounds, searching a grid of `steps` increments so
that S-shaped curves do not trap it. Both keep each channel’s weekly
pattern and change only its level, so neither can start a channel that
has no spend until it is given small weights in the weeks it should run;
to try a different timing, build the plan yourself and pass it to
[`simulate_plan()`](https://elkronos.github.io/mediamix/reference/simulate_plan.md).
The answers are only as good as the fitted curves, which are identified
only over the spend levels in the data, so a move far beyond them is
extrapolation and worth testing before it is made.

``` r

allocate_budget(north, spec, coef(fit), steps = 100)
#> 
#> ── Budget allocation
#> A total of 458,439, in 100 increments of 4,584.39.
#> Predicted media response: 833,959 now, 845,422 recommended.
#>  channel current_spend recommended_spend current_response recommended_response
#>       tv        266978            252141           349629               330634
#>    video         53248             45844           102461                93095
#>   search         74560             77935           208307               212779
#>   social         36716             64181           132709               177194
#>  display         26937             18338            40853                31719
#>   mroi
#>  1.299
#>  1.329
#>  1.289
#>  1.250
#>  1.203
#> The recommended plan is in $plan.
```

## Attribution

### Why do the attribution rules disagree?

Each rule is a convention for dividing a conversion among the touches
before it, and the conventions favour different positions: first-touch
credits the channels that open journeys and last-touch those that close
them.
[`attribute()`](https://elkronos.github.io/mediamix/reference/attribute.md)
runs all six rules on the same journeys,
[`attribution_spread()`](https://elkronos.github.io/mediamix/reference/attribution_spread.md)
reports each channel’s lowest and highest share, and
[`channel_positions()`](https://elkronos.github.io/mediamix/reference/channel_positions.md)
shows which channels open or close journeys. A wide spread is itself the
finding, since it measures how much of a channel’s credit comes from the
choice of rule rather than from the data.

``` r

paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                     timestamp = "timestamp", conversion = "conversion")
spread <- attribution_spread(attribute(paths))
spread[, c("channel", "min_share", "max_share", "first_last_ratio")]
#>           channel min_share max_share first_last_ratio
#> 1         display   0.07187   0.29113           4.0508
#> 2     paid_search   0.09157   0.27253           0.3360
#> 3           email   0.05655   0.18314           0.3088
#> 4          social   0.11638   0.22072           1.8966
#> 5  organic_search   0.12623   0.17877           0.7061
#> 6       affiliate   0.08063   0.11711           1.4525
#> 7          direct   0.02225   0.02683           1.0492
#> 8         (blank)   0.02955   0.03379           0.9419
#> 9          (none)   0.02736   0.02982           1.0133
#> 10      (missing)   0.01569   0.01637           1.0233
```

### Can I attribute by segment, with intervals?

Yes. Carry the segment column into the journeys with the `keep` argument
of
[`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md),
give each journey a single value, and pass the column to
[`attribute()`](https://elkronos.github.io/mediamix/reference/attribute.md)
as `by`; every rule, the Markov chain included, then runs on each
segment’s journeys alone. With `times` (and `seed`),
[`attribute()`](https://elkronos.github.io/mediamix/reference/attribute.md)
adds percentile intervals by resampling journeys with replacement. The
intervals show how far the shares move with the sample of journeys, with
the journey construction and each rule held fixed; they say nothing
about the choice of rule, which
[`attribution_spread()`](https://elkronos.github.io/mediamix/reference/attribution_spread.md)
measures.

### Can I fit higher-order Markov chains?

[`markov_removal()`](https://elkronos.github.io/mediamix/reference/markov_removal.md)
fits the first-order chain, following Anderl et al. (2016). For higher
orders, export the journeys with
[`as_channel_paths()`](https://elkronos.github.io/mediamix/reference/as_channel_paths.md)
and pass them to
[`ChannelAttribution::markov_model()`](https://rdrr.io/pkg/ChannelAttribution/man/markov_model.html)
with its `order` argument and the same `sep` on both sides;
[`ChannelAttribution::choose_order()`](https://rdrr.io/pkg/ChannelAttribution/man/choose_order.html)
suggests an order. That needs the ChannelAttribution package, which
mediamix suggests but does not require. For order 1 the two packages
implement the same removal-effect definition, ChannelAttribution by
simulation.

``` r

ca <- as_channel_paths(paths, sep = ">")
ChannelAttribution::markov_model(ca, var_path = "path",
                                 var_conv = "total_conversions",
                                 var_null = "total_null", order = 2, sep = ">")
```

### Why keep the journeys that did not convert?

Markov removal effects compare the chance of reaching a conversion with
the chance of reaching nothing, and the journeys that ended in nothing
are the only evidence of the second. Without them the baseline
conversion probability is 1, and every removal effect moves, up or down
depending on how the dropped journeys differed from the converting ones.
[`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md)
keeps them by default (`keep_null_paths = TRUE`),
[`as_channel_paths()`](https://elkronos.github.io/mediamix/reference/as_channel_paths.md)
counts them in `total_null`, and
[`markov_removal()`](https://elkronos.github.io/mediamix/reference/markov_removal.md)
warns when a table has none.

### How do I choose a lookback window?

Read it off your own journeys.
[`conversion_lag()`](https://elkronos.github.io/mediamix/reference/conversion_lag.md)
reports quantiles of the time from first touch to conversion, so a
lookback at its 90% point leaves nine in ten converting journeys whole.
Run it on journeys built without `lookback`, since a lookback caps every
lag it measures, and read the lags in the journey table’s `units`, days
by default.

``` r

conversion_lag(paths)
#>   quantile    lag
#> 1     0.50  4.535
#> 2     0.75  8.793
#> 3     0.90 13.492
#> 4     0.95 16.851
#> 5     0.99 23.538
#> 6     1.00 43.165
```

## Other tools

### Can I use the transforms with brms, Stan or glmnet?

Yes, with the transform parameters fixed before the fit. The transforms
are R functions on numeric vectors, so
[`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md)
or
[`transform_plan()`](https://elkronos.github.io/mediamix/reference/transform_plan.md)
produces columns that any model can take.
[`contributions()`](https://elkronos.github.io/mediamix/reference/contributions.md)
reads coefficients through
[`coef()`](https://rdrr.io/r/stats/coef.html), which serves
[`lm()`](https://rdrr.io/r/stats/lm.html),
[`glm()`](https://rdrr.io/r/stats/glm.html) and the fixed effects of
nlme and lme4; for glmnet, brms or Stan, pass the point estimates as a
named vector, naming the intercept `"(Intercept)"` or giving it as
`intercept`, and the baseline is then that intercept alone.
[`marginal_roi()`](https://elkronos.github.io/mediamix/reference/marginal_roi.md),
[`simulate_plan()`](https://elkronos.github.io/mediamix/reference/simulate_plan.md)
and
[`allocate_budget()`](https://elkronos.github.io/mediamix/reference/allocate_budget.md)
take coefficients as a named vector in any case. To estimate a decay or
a `half_max` inside a Bayesian model, the transform has to be written in
that model’s own language, Stan for brms.

``` r

# A ridge regression at a fixed penalty, chosen for illustration
X <- as.matrix(media[c(channels, "week", "price", "seasonality", "holiday")])
ridge <- glmnet::glmnet(X, media$revenue, alpha = 0, lambda = 50)
b <- as.matrix(coef(ridge))[, 1]
marginal_roi(north, b, spec = spec)
#>   channel  spend contribution   roi  mroi
#> 1      tv 266978       347225 1.301 1.245
#> 2   video  53248       101660 1.909 1.187
#> 3  search  74560       204208 2.739 1.322
#> 4  social  36716       126887 3.456 1.998
#> 5 display  26937        43968 1.632 1.003
```

### Can I reproduce Robyn’s transforms?

Yes, for geometric and Weibull CDF adstock with Hill saturation. Given
the series Robyn transformed and its modelling window,
[`from_robyn()`](https://elkronos.github.io/mediamix/reference/from_robyn.md)
converts one channel’s `theta` (or `shape` and `scale`), `alpha` and
`gamma` into the `adstock` and `saturation` lists that
[`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md)
takes, and the result matches Robyn’s series. Set `inflexion = "range"`
for a model fitted by Robyn 3.7.0 to 3.11.1, which placed the Hill
inflexion point differently. Robyn’s Weibull PDF has no exact equivalent
and is refused. [Migrating from
Robyn](https://elkronos.github.io/mediamix/articles/migrating-from-robyn.md)
works through an example.

### Is any of this causal?

No. A media mix model fitted to observational data is identified only as
well as its specification and the variation in the data allow, and
cross-validation shows predictive value, not a causal mechanism.
Attribution rules, Markov removal effects included, divide observed
conversions by a convention and cannot say what would have happened
without a channel. Randomised experiments, such as geo tests for media
and holdouts for digital channels, measure that, and they are the way to
check or calibrate a model; mediamix has no calibration step of its own.
[`reallocate()`](https://elkronos.github.io/mediamix/reference/reallocate.md)
with `by` sets up a move in one region, the plan for such a test.

## References

Anderl, E., Becker, I., von Wangenheim, F. and Schumann, J. H. (2016).
Mapping the customer journey: Lessons learned from graph-based online
attribution modeling. *International Journal of Research in Marketing*,
33(3), 457–474. <https://doi.org/10.1016/j.ijresmar.2016.03.001>

Breiman, L., Friedman, J., Olshen, R. and Stone, C. (1984).
*Classification and Regression Trees*. Wadsworth.
<https://doi.org/10.1201/9781315139470>

Hastie, T., Tibshirani, R. and Friedman, J. (2009). *The Elements of
Statistical Learning* (2nd ed.). Springer.
<https://doi.org/10.1007/978-0-387-84858-7>

Jin, Y., Wang, Y., Sun, Y., Chan, D. and Koehler, J. (2017). *Bayesian
methods for media mix modeling with carryover and shape effects.* Google
Inc. <https://research.google/pubs/pub46001/>

Künsch, H. R. (1989). The jackknife and the bootstrap for general
stationary observations. *The Annals of Statistics*, 17(3), 1217–1241.
<https://doi.org/10.1214/aos/1176347265>
