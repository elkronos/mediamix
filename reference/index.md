# Package index

## Check the data

Find collinear, flat or dark channels and inconsistent costs before
fitting anything.

- [`diagnose_media()`](https://elkronos.github.io/mediamix/reference/diagnose_media.md)
  : Diagnose media data before modelling
- [`plot(`*`<mm_diagnosis>`*`)`](https://elkronos.github.io/mediamix/reference/plot.mm_diagnosis.md)
  : Plot media diagnostics

## Carryover (adstock)

Spread each period’s media forward in time, and translate decay into a
half-life.

- [`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md)
  : Geometric adstock
- [`adstock_weibull()`](https://elkronos.github.io/mediamix/reference/adstock_weibull.md)
  : Weibull adstock
- [`adstock_weights_delayed()`](https://elkronos.github.io/mediamix/reference/adstock_delayed.md)
  [`adstock_delayed()`](https://elkronos.github.io/mediamix/reference/adstock_delayed.md)
  : Delayed adstock
- [`adstock_filter()`](https://elkronos.github.io/mediamix/reference/adstock_filter.md)
  : Adstock with an arbitrary kernel
- [`adstock_weights()`](https://elkronos.github.io/mediamix/reference/adstock_weights.md)
  : Geometric adstock weights
- [`adstock_weights_weibull()`](https://elkronos.github.io/mediamix/reference/adstock_weights_weibull.md)
  : Weibull adstock weights
- [`adstock_state()`](https://elkronos.github.io/mediamix/reference/adstock_state.md)
  : Terminal adstock state
- [`adstock_steady_state()`](https://elkronos.github.io/mediamix/reference/adstock_steady_state.md)
  : Steady-state starting value for an adstock filter
- [`decay_from_half_life()`](https://elkronos.github.io/mediamix/reference/decay_vocabulary.md)
  [`half_life()`](https://elkronos.github.io/mediamix/reference/decay_vocabulary.md)
  [`effective_window()`](https://elkronos.github.io/mediamix/reference/decay_vocabulary.md)
  : Carryover decay vocabulary

## Saturation

Diminishing returns as media pressure grows.

- [`saturate_hill()`](https://elkronos.github.io/mediamix/reference/saturation.md)
  [`saturate_exponential()`](https://elkronos.github.io/mediamix/reference/saturation.md)
  [`saturate_michaelis_menten()`](https://elkronos.github.io/mediamix/reference/saturation.md)
  [`saturate_power()`](https://elkronos.github.io/mediamix/reference/saturation.md)
  [`saturate()`](https://elkronos.github.io/mediamix/reference/saturation.md)
  : Saturation curves

## Composing transforms

Apply carryover then saturation to one channel or to every channel of a
plan, and convert Robyn’s hyperparameters to these arguments.

- [`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md)
  : Adstock then saturate, in that order
- [`transform_plan()`](https://elkronos.github.io/mediamix/reference/transform_plan.md)
  : Transform every channel of a media plan
- [`from_robyn()`](https://elkronos.github.io/mediamix/reference/from_robyn.md)
  : Convert Robyn hyperparameters to mediamix arguments

## Choosing carryover parameters

Score candidate decays and lags by how well a model predicts held-out
KPI, with seasonality terms as controls.

- [`tune_carryover()`](https://elkronos.github.io/mediamix/reference/tune_carryover.md)
  : Select carryover parameters by cross-validation against a KPI
- [`tune_carryover_joint()`](https://elkronos.github.io/mediamix/reference/tune_carryover_joint.md)
  : Tune every channel's carryover jointly, inside one model
- [`plot(`*`<mm_carryover>`*`)`](https://elkronos.github.io/mediamix/reference/plot_carryover.md)
  [`plot(`*`<mm_carryover_joint>`*`)`](https://elkronos.github.io/mediamix/reference/plot_carryover.md)
  : Plot a carryover cross-validation profile
- [`fit_ols()`](https://elkronos.github.io/mediamix/reference/fit_ols.md)
  [`predict(`*`<mm_ols>`*`)`](https://elkronos.github.io/mediamix/reference/fit_ols.md)
  [`coef(`*`<mm_ols>`*`)`](https://elkronos.github.io/mediamix/reference/fit_ols.md)
  [`fitted(`*`<mm_ols>`*`)`](https://elkronos.github.io/mediamix/reference/fit_ols.md)
  [`print(`*`<mm_ols>`*`)`](https://elkronos.github.io/mediamix/reference/fit_ols.md)
  : Default linear fitter for carryover tuning
- [`rmse()`](https://elkronos.github.io/mediamix/reference/metrics.md)
  [`mae()`](https://elkronos.github.io/mediamix/reference/metrics.md) :
  Root mean squared error and mean absolute error
- [`fourier_terms()`](https://elkronos.github.io/mediamix/reference/seasonality.md)
  [`holiday_flags()`](https://elkronos.github.io/mediamix/reference/seasonality.md)
  : Seasonality controls for a model frame

## Tuning with tidymodels

Recipe steps and tuning parameters for tuning carryover, saturation and
a model penalty together.

- [`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
  : Adstock transformation as a recipe step
- [`step_adstock_channels()`](https://elkronos.github.io/mediamix/reference/step_adstock_channels.md)
  : One adstock step per channel
- [`step_saturation()`](https://elkronos.github.io/mediamix/reference/step_saturation.md)
  : Saturation transformation as a recipe step
- [`carryover_decay()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md)
  [`carryover_max_lag()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md)
  [`carryover_shape()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md)
  [`carryover_scale()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md)
  [`carryover_peak()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md)
  [`saturation_half_max()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md)
  [`saturation_shape()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md)
  : Tuning parameters for media transforms

## Contributions and return on investment

Turn a fitted model into channel contributions, average and marginal
return, and response curves.

- [`contributions()`](https://elkronos.github.io/mediamix/reference/contributions.md)
  : Decompose predicted KPI into channel contributions
- [`plot(`*`<mm_contributions>`*`)`](https://elkronos.github.io/mediamix/reference/plot.mm_contributions.md)
  : Plot a contribution decomposition
- [`roi()`](https://elkronos.github.io/mediamix/reference/roi.md)
  [`mroi()`](https://elkronos.github.io/mediamix/reference/roi.md) :
  Return on investment and marginal return
- [`marginal_roi()`](https://elkronos.github.io/mediamix/reference/marginal_roi.md)
  : Average and marginal return by counterfactual simulation
- [`response_curve()`](https://elkronos.github.io/mediamix/reference/response_curve.md)
  [`spend_for()`](https://elkronos.github.io/mediamix/reference/response_curve.md)
  : Response curve for a channel
- [`plot(`*`<mm_response_curve>`*`)`](https://elkronos.github.io/mediamix/reference/plot.mm_response_curve.md)
  : Plot a response curve

## Uncertainty

Put intervals on any statistic by resampling blocks of consecutive
periods.

- [`block_bootstrap()`](https://elkronos.github.io/mediamix/reference/block_bootstrap.md)
  : Moving block bootstrap for time-series statistics
- [`plot(`*`<mm_bootstrap>`*`)`](https://elkronos.github.io/mediamix/reference/plot.mm_bootstrap.md)
  : Plot bootstrap intervals

## Budget planning

Simulate a plan through the fitted transforms, move budget between
channels, and split a total budget across channels.

- [`simulate_plan()`](https://elkronos.github.io/mediamix/reference/simulate_plan.md)
  : Predicted media response of a plan
- [`reallocate()`](https://elkronos.github.io/mediamix/reference/reallocate.md)
  : Move spend between two channels of a plan
- [`allocate_budget()`](https://elkronos.github.io/mediamix/reference/allocate_budget.md)
  : Allocate a budget across channels
- [`plot(`*`<mm_allocation>`*`)`](https://elkronos.github.io/mediamix/reference/plot.mm_allocation.md)
  : Plot a budget allocation

## Journey construction

Turn a raw event log into customer journeys, and combine or subset them.

- [`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md)
  : Build customer journeys from a raw event log
- [`bind_paths()`](https://elkronos.github.io/mediamix/reference/bind_paths.md)
  [`rbind(`*`<mm_paths>`*`)`](https://elkronos.github.io/mediamix/reference/bind_paths.md)
  : Combine journey tables
- [`` `[`( ``*`<mm_paths>`*`)`](https://elkronos.github.io/mediamix/reference/sub-.mm_paths.md)
  : Subset a journey table

## Journey diagnostics

Describe a journey table, including how long conversions take, before
choosing a lookback or a credit rule.

- [`path_summary()`](https://elkronos.github.io/mediamix/reference/path_summary.md)
  : Journey table summary
- [`path_lengths()`](https://elkronos.github.io/mediamix/reference/path_lengths.md)
  : Journey length distribution
- [`channel_positions()`](https://elkronos.github.io/mediamix/reference/channel_positions.md)
  : Where each channel sits in the journey
- [`assisted_conversions()`](https://elkronos.github.io/mediamix/reference/assisted_conversions.md)
  : Assisted conversion matrix
- [`top_paths()`](https://elkronos.github.io/mediamix/reference/top_paths.md)
  : Most common journeys
- [`conversion_lag()`](https://elkronos.github.io/mediamix/reference/conversion_lag.md)
  : Conversion lag distribution
- [`path_diagnostics()`](https://elkronos.github.io/mediamix/reference/path_diagnostics.md)
  : Run every journey diagnostic at once

## Attribution

Credit conversions to channels by rule or by Markov removal effects,
compare the rules, and export journeys to ChannelAttribution.

- [`credit_linear()`](https://elkronos.github.io/mediamix/reference/credit.md)
  [`credit_first()`](https://elkronos.github.io/mediamix/reference/credit.md)
  [`credit_last()`](https://elkronos.github.io/mediamix/reference/credit.md)
  [`credit_position()`](https://elkronos.github.io/mediamix/reference/credit.md)
  [`credit_time_decay()`](https://elkronos.github.io/mediamix/reference/credit.md)
  [`credit_custom()`](https://elkronos.github.io/mediamix/reference/credit.md)
  : Assign conversion credit across a journey
- [`markov_removal()`](https://elkronos.github.io/mediamix/reference/markov_removal.md)
  : Markov-chain removal effects
- [`attribute()`](https://elkronos.github.io/mediamix/reference/attribute.md)
  : Attribute conversions to channels under several rules at once
- [`attribution_spread()`](https://elkronos.github.io/mediamix/reference/attribution_spread.md)
  : Range of each channel's share across attribution rules
- [`plot(`*`<mm_attribution>`*`)`](https://elkronos.github.io/mediamix/reference/plot.mm_attribution.md)
  : Plot attribution shares across rules
- [`plot(`*`<mm_attribution_intervals>`*`)`](https://elkronos.github.io/mediamix/reference/plot.mm_attribution_intervals.md)
  [`plot(`*`<mm_attribution_segments>`*`)`](https://elkronos.github.io/mediamix/reference/plot.mm_attribution_intervals.md)
  : Plot attribution shares with intervals or by segment
- [`as_channel_paths()`](https://elkronos.github.io/mediamix/reference/as_channel_paths.md)
  : Export journeys in ChannelAttribution's format

## Example data

A simulated weekly media panel and event log, generated from known
parameters so results can be checked.

- [`mm_weekly`](https://elkronos.github.io/mediamix/reference/mm_weekly.md)
  : Synthetic weekly marketing mix panel
- [`mm_events`](https://elkronos.github.io/mediamix/reference/mm_events.md)
  : Synthetic touchpoint event log

## Package

The package overview, and the colour palette its plots use.

- [`mediamix`](https://elkronos.github.io/mediamix/reference/mediamix-package.md)
  [`mediamix-package`](https://elkronos.github.io/mediamix/reference/mediamix-package.md)
  : mediamix: media transforms and attribution path construction
- [`mm_palette()`](https://elkronos.github.io/mediamix/reference/mm_palette.md)
  : The package's chart palette
