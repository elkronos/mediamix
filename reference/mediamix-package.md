# mediamix: media transforms and attribution path construction

Feature engineering for marketing mix modelling and multi-touch
attribution. mediamix turns raw marketing data into model-ready
features: media spend into carryover- and saturation-adjusted
regressors, and raw event logs into attributed customer journeys.

## Details

It is a preprocessing and reporting package. It does not fit models –
[`lm()`](https://rdrr.io/r/stats/lm.html), `glmnet` and `brms` do that
better than a marketing package would. It computes order-1 Markov
removal effects natively; for higher-order chains
[`as_channel_paths()`](https://elkronos.github.io/mediamix/reference/as_channel_paths.md)
hands your journeys to ChannelAttribution.

Full documentation, an end-to-end walkthrough and the methods behind
each function are at <https://elkronos.github.io/mediamix/>.

## Where to start

Five vignettes, in the order most people need them:

- [`vignette("mediamix")`](https://elkronos.github.io/mediamix/articles/mediamix.md):

  Getting started: raw weekly spend through diagnostics, transforms and
  a model to contributions and ROI.

- [`vignette("journeys")`](https://elkronos.github.io/mediamix/articles/journeys.md):

  Building journeys from event logs, and what a naive `group_by()` and
  [`paste()`](https://rdrr.io/r/base/paste.html) gets wrong.

- [`vignette("carryover")`](https://elkronos.github.io/mediamix/articles/carryover.md):

  Choosing decay and lag by cross-validation against your KPI rather
  than by eye.

- [`vignette("tidymodels")`](https://elkronos.github.io/mediamix/articles/tidymodels.md):

  Tuning carryover, saturation and model penalty jointly in one search.

- [`vignette("spine")`](https://elkronos.github.io/mediamix/articles/spine.md):

  Why carryover and credit are the same idea.

## The media half

- Carryover:

  [`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md)
  for the standard geometric kernel,
  [`adstock_weibull()`](https://elkronos.github.io/mediamix/reference/adstock_weibull.md)
  and
  [`adstock_delayed()`](https://elkronos.github.io/mediamix/reference/adstock_delayed.md)
  when the response peaks after the spend,
  [`adstock_filter()`](https://elkronos.github.io/mediamix/reference/adstock_filter.md)
  for a kernel of your own.
  [`adstock_weights()`](https://elkronos.github.io/mediamix/reference/adstock_weights.md)
  and
  [`adstock_weights_weibull()`](https://elkronos.github.io/mediamix/reference/adstock_weights_weibull.md)
  expose the kernels themselves, and
  [`adstock_state()`](https://elkronos.github.io/mediamix/reference/adstock_state.md)
  carries a filter across a boundary.

- Saturation:

  [`saturate_hill()`](https://elkronos.github.io/mediamix/reference/saturation.md),
  [`saturate_exponential()`](https://elkronos.github.io/mediamix/reference/saturation.md),
  [`saturate_michaelis_menten()`](https://elkronos.github.io/mediamix/reference/saturation.md),
  [`saturate_power()`](https://elkronos.github.io/mediamix/reference/saturation.md),
  and the
  [`saturate()`](https://elkronos.github.io/mediamix/reference/saturation.md)
  dispatcher.

- Composing them:

  [`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md)
  applies carryover then saturation, in that order, and will not do it
  backwards quietly.

- Choosing parameters:

  [`decay_from_half_life()`](https://elkronos.github.io/mediamix/reference/decay_vocabulary.md),
  [`half_life()`](https://elkronos.github.io/mediamix/reference/decay_vocabulary.md)
  and
  [`effective_window()`](https://elkronos.github.io/mediamix/reference/decay_vocabulary.md)
  translate between half-lives and decay coefficients.
  [`tune_carryover()`](https://elkronos.github.io/mediamix/reference/tune_carryover.md)
  and
  [`tune_carryover_joint()`](https://elkronos.github.io/mediamix/reference/tune_carryover_joint.md)
  select them by cross-validation against an actual KPI;
  [`adstock_steady_state()`](https://elkronos.github.io/mediamix/reference/adstock_steady_state.md)
  removes start-up bias.

## The attribution half

- Journeys:

  [`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md)
  turns an event log into journeys, handling the seven things that go
  wrong on the way.

- Credit:

  [`credit_linear()`](https://elkronos.github.io/mediamix/reference/credit.md),
  [`credit_first()`](https://elkronos.github.io/mediamix/reference/credit.md),
  [`credit_last()`](https://elkronos.github.io/mediamix/reference/credit.md),
  [`credit_position()`](https://elkronos.github.io/mediamix/reference/credit.md),
  [`credit_time_decay()`](https://elkronos.github.io/mediamix/reference/credit.md)
  and
  [`credit_custom()`](https://elkronos.github.io/mediamix/reference/credit.md),
  and the data-driven
  [`markov_removal()`](https://elkronos.github.io/mediamix/reference/markov_removal.md).
  [`attribute()`](https://elkronos.github.io/mediamix/reference/attribute.md)
  runs several at once and
  [`attribution_spread()`](https://elkronos.github.io/mediamix/reference/attribution_spread.md)
  reports how much the answer depends on which you picked.

- Diagnostics:

  [`path_summary()`](https://elkronos.github.io/mediamix/reference/path_summary.md),
  [`path_lengths()`](https://elkronos.github.io/mediamix/reference/path_lengths.md),
  [`channel_positions()`](https://elkronos.github.io/mediamix/reference/channel_positions.md),
  [`assisted_conversions()`](https://elkronos.github.io/mediamix/reference/assisted_conversions.md),
  [`top_paths()`](https://elkronos.github.io/mediamix/reference/top_paths.md)
  and
  [`conversion_lag()`](https://elkronos.github.io/mediamix/reference/conversion_lag.md),
  or
  [`path_diagnostics()`](https://elkronos.github.io/mediamix/reference/path_diagnostics.md)
  for all of them.

- Interop:

  [`as_channel_paths()`](https://elkronos.github.io/mediamix/reference/as_channel_paths.md)
  exports to ChannelAttribution's format.

## Reporting, diagnostics and tidymodels

[`diagnose_media()`](https://elkronos.github.io/mediamix/reference/diagnose_media.md)
checks whether the data can support a model at all – run it first.
[`contributions()`](https://elkronos.github.io/mediamix/reference/contributions.md),
[`roi()`](https://elkronos.github.io/mediamix/reference/roi.md),
[`marginal_roi()`](https://elkronos.github.io/mediamix/reference/marginal_roi.md),
[`mroi()`](https://elkronos.github.io/mediamix/reference/roi.md),
[`response_curve()`](https://elkronos.github.io/mediamix/reference/response_curve.md)
and
[`spend_for()`](https://elkronos.github.io/mediamix/reference/response_curve.md)
turn a fitted model into a deliverable, and
[`block_bootstrap()`](https://elkronos.github.io/mediamix/reference/block_bootstrap.md)
puts intervals on it. Each result has a
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) method;
[`mm_palette()`](https://elkronos.github.io/mediamix/reference/mm_palette.md)
exposes their colours.
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
and
[`step_saturation()`](https://elkronos.github.io/mediamix/reference/step_saturation.md)
are recipes steps that carry filter state across the train/test
boundary, with
[`carryover_decay()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md)
and friends as their dials parameters.

## Data

[mm_weekly](https://elkronos.github.io/mediamix/reference/mm_weekly.md)
is a synthetic weekly panel generated from known parameters, so a
workflow can be checked against the truth it is trying to recover.
[mm_events](https://elkronos.github.io/mediamix/reference/mm_events.md)
is a synthetic touchpoint log containing, on purpose, every case that
breaks a naive journey pipeline.

## Dependencies

The core transforms need only base R, `stats`, and `cli` for error
messages. `data.table` is used by the attribution half, where event logs
are large. recipes, dials and the rest of tidymodels are optional and
registered conditionally, so the transforms work equally well from a
Stan or brms workflow that will never touch them.

## See also

Useful links:

- <https://elkronos.github.io/mediamix/>

- <https://github.com/elkronos/mediamix>

- Report bugs at <https://github.com/elkronos/mediamix/issues>

## Author

**Maintainer**: Justin Chase <jchase.msu@gmail.com>

## Examples

``` r
# The media half: spend in, model-ready regressor out
spend <- c(0, 500, 800, 300, 0, 0, 1200, 400)
media_transform(
  spend,
  adstock = list(decay = decay_from_half_life(2)),
  saturation = list(half_max = 300)
)
#> [1] 0.0000000 0.3280272 0.5296832 0.5213606 0.4350985 0.3525949 0.6088682
#> [8] 0.5985976

# The attribution half: event log in, journeys out
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
