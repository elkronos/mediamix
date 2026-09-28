# mediamix (development version)

## Bug fixes

* Subsetting an `mm_paths` object so that part of a journey is removed now
  recomputes `touch_rank` and `touch_n`. Previously the stale ranks made
  `credit_first()`, `credit_last()` and `credit_position()` give some
  converting journeys no credit at all after, for example, filtering out one
  channel.
* `tune_carryover()`'s metric now means what it is called. Per-split metrics
  were averaged, so with the default one-period assessment sets the reported
  "rmse" was a mean absolute error. The default is now `aggregate = "pooled"`,
  which evaluates the metric once over every out-of-sample forecast;
  `aggregate = "mean"` keeps the old behaviour.
* The documentation and the getting-started vignette recommended computing
  marginal ROI on spend as the saturation slope times the kernel's first
  weight. That counts only the period of spend and understated slow
  channels' marginal return several-fold (six-fold for `mm_weekly`'s
  television), reversing a reallocation conclusion. See `marginal_roi()`.
* `build_paths()` now reports when journeys hold several conversion events
  (possible under `split_on = "none"` or `"gap"`), since each is credited as
  one conversion.

* `diagnose_media()` now computes the implied-CPM median and outlier rule
  within each series when `by` is supplied. Pooled across geographies, a
  region that buys media at a different price was flagged as a stream of
  join errors. The column formerly called `period`, which held a row number,
  is now `row`, and a `group` column is added with `by`.

## New features

* `marginal_roi()` computes average and marginal return by re-running
  carryover and saturation on a slightly larger budget, following Jin et al.
  (2017), with optional planning windows and carryover run-out.
* `markov_removal()` implements order-1 Markov removal-effect attribution
  (Anderl et al., 2016) exactly, in base R. `attribute()` includes it as the
  `"markov"` rule.
* `adstock_delayed()` and `adstock_weights_delayed()` add the delayed-peak
  kernel of Jin et al. (2017); `media_transform()` accepts
  `kernel = "delayed"`.
* `tune_carryover()` reports `std_err` and a `best_1se` choice using a paired
  one-standard-error rule, and gains `controls` (entered into the model and
  kept aligned with every split) and `warm_start`.
* `tune_carryover_joint()` tunes every channel's carryover inside one model
  with controls, by coordinate descent, reporting each channel's profile.
* `block_bootstrap()`: moving block bootstrap (Kunsch 1989) with percentile
  intervals for ROI, marginal ROI or any statistic.
* `adstock_steady_state()` seeds a filter at steady state, removing the
  start-up bias of long-carryover channels.
* `diagnose_media(decay =)` measures collinearity on adstocked media.
* `plot()` methods for carryover profiles, joint tuning, bootstrap intervals,
  contributions (over time or in total), response curves (with the
  extrapolation region shaded) and attribution shares, in one
  colour-vision-checked style. `mm_palette()` exports the colours.
  `contributions()`, `response_curve()` and `attribute()` now return
  subclassed data frames so the methods dispatch.
* `inst/CITATION` added.

## Performance

* `assisted_conversions()` is about 16x faster and `top_paths()` /
  `as_channel_paths()` about 2x faster on large journey tables, via
  data.table. `response_curve()` computes marginals in one vectorised pass.

## Documentation

* A pkgdown site with an end-to-end budget walkthrough, a methods and
  references article and a comparison with other tools, deployed to GitHub
  Pages by a new workflow.
* `mm_weekly` spend is rescaled so that average ROIs are between about 1 and
  4 rather than 0.07; revenue, decays and shapes are unchanged.
* README and vignette claims about other packages corrected: the README no
  longer says Robyn is not a CRAN package or that no other package builds
  journeys, and the normalised adstock kernel is now credited as the Jin et
  al. (2017) convention rather than a departure from the literature. The spine vignette no longer suggests an MMM half-life is an
  attribution lookback.
* The README no longer tells users to `install.packages("mediamix")`; the
  package is not on CRAN.

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
