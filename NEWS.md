# mediamix 0.4.0

First release.

## Media transforms

* `adstock_geometric()`, `adstock_weibull()`, `adstock_filter()` and
  `adstock_state()` apply carryover transforms with explicit, inspectable
  filter state, grouping via `by=`, and an explicit `normalise` argument whose
  consequence for coefficient interpretation is documented.
* `adstock_weights()` and `adstock_weights_weibull()` expose the kernels
  directly.
* `saturate_hill()`, `saturate_exponential()`, `saturate_michaelis_menten()`,
  `saturate_power()` and the `saturate()` dispatcher provide four
  diminishing-returns curves.
* `media_transform()` composes carryover and saturation in the conventional
  order and warns rather than silently reversing it.
* `decay_from_half_life()`, `half_life()` and `effective_window()` give both
  halves of the package one vocabulary.

## Carryover selection

* `tune_carryover()` selects `(max_lag, decay)` by cross-validation against an
  actual KPI using a user-supplied model, with forward-only resampling. It
  warns when the selected parameters sit on the edge of the search grid.
* `fit_ols()`, `rmse()` and `mae()` provide working defaults.

## Attribution

* `build_paths()` constructs customer journeys from a raw event log, handling
  journey splitting, lookback windows, non-converting paths, direct and missing
  channel labels, consecutive duplicates, deterministic tie-breaking, and the
  accounting for conversions left unattributable by filtering.
* `credit_linear()`, `credit_first()`, `credit_last()`, `credit_position()`,
  `credit_time_decay()` and `credit_custom()` assign credit; `attribute()` runs
  several rules at once and `attribution_spread()` reports how much the answer
  depends on the choice.
* `path_summary()`, `path_lengths()`, `channel_positions()`,
  `assisted_conversions()`, `top_paths()`, `conversion_lag()` and
  `path_diagnostics()` describe the journey table.
* `as_channel_paths()` exports journeys in ChannelAttribution's format.

## Recipe steps

* `step_adstock()` and `step_saturation()` carry filter state across the
  train/test boundary without changing row count or order, classifying new data
  as contiguous, overlapping, gapped or unseen and warm-starting only when that
  is correct.
* `tunable()` methods and the `carryover_decay()`, `carryover_max_lag()`,
  `saturation_half_max()` and `saturation_shape()` parameter objects support
  joint tuning with model hyperparameters.

## Reporting and diagnostics

* `contributions()`, `roi()`, `mroi()`, `response_curve()` and `spend_for()`
  turn a fitted model into a marketing mix deliverable.
* `diagnose_media()` reports collinearity, insufficient variation, flighting
  and implied CPM inconsistency.

## Data

* `mm_weekly`, a synthetic weekly panel generated from known parameters.
* `mm_events`, a synthetic touchpoint log containing the awkward cases on
  purpose.
