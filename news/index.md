# Changelog

## mediamix (development version)

### Bug fixes

- Subsetting an `mm_paths` object so that part of a journey is removed
  now recomputes `touch_rank` and `touch_n`. Previously the stale ranks
  made
  [`credit_first()`](https://elkronos.github.io/mediamix/reference/credit.md),
  [`credit_last()`](https://elkronos.github.io/mediamix/reference/credit.md)
  and
  [`credit_position()`](https://elkronos.github.io/mediamix/reference/credit.md)
  give some converting journeys no credit at all after, for example,
  filtering out one channel.

- [`tune_carryover()`](https://elkronos.github.io/mediamix/reference/tune_carryover.md)’s
  metric now means what it is called. Per-split metrics were averaged,
  so with the default one-period assessment sets the reported “rmse” was
  a mean absolute error. The default is now `aggregate = "pooled"`,
  which evaluates the metric once over every out-of-sample forecast;
  `aggregate = "mean"` keeps the old behaviour.

- The documentation and the getting-started vignette recommended
  computing marginal ROI on spend as the saturation slope times the
  kernel’s first weight. That counts only the period of spend and
  understated slow channels’ marginal return several-fold (six-fold for
  `mm_weekly`’s television), reversing a reallocation conclusion. See
  [`marginal_roi()`](https://elkronos.github.io/mediamix/reference/marginal_roi.md).

- [`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md)
  now reports when journeys hold several conversion events (possible
  under `split_on = "none"` or `"gap"`), since each is credited as one
  conversion.

- [`diagnose_media()`](https://elkronos.github.io/mediamix/reference/diagnose_media.md)
  now computes the implied-CPM median and outlier rule within each
  series when `by` is supplied. Pooled across geographies, a region that
  buys media at a different price was flagged as a stream of join
  errors. The column formerly called `period`, which held a row number,
  is now `row`, and a `group` column is added with `by`.

### New features

- [`marginal_roi()`](https://elkronos.github.io/mediamix/reference/marginal_roi.md)
  computes average and marginal return by re-running carryover and
  saturation on a slightly larger budget, following Jin et al. (2017),
  with optional planning windows and carryover run-out.
- [`markov_removal()`](https://elkronos.github.io/mediamix/reference/markov_removal.md)
  implements order-1 Markov removal-effect attribution (Anderl et
  al., 2016) exactly, in base R.
  [`attribute()`](https://elkronos.github.io/mediamix/reference/attribute.md)
  includes it as the `"markov"` rule.
- [`adstock_delayed()`](https://elkronos.github.io/mediamix/reference/adstock_delayed.md)
  and
  [`adstock_weights_delayed()`](https://elkronos.github.io/mediamix/reference/adstock_delayed.md)
  add the delayed-peak kernel of Jin et al. (2017);
  [`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md)
  accepts `kernel = "delayed"`.
- [`tune_carryover()`](https://elkronos.github.io/mediamix/reference/tune_carryover.md)
  reports `std_err` and a `best_1se` choice using a paired
  one-standard-error rule, and gains `controls` (entered into the model
  and kept aligned with every split) and `warm_start`.
- [`tune_carryover_joint()`](https://elkronos.github.io/mediamix/reference/tune_carryover_joint.md)
  tunes every channel’s carryover inside one model with controls, by
  coordinate descent, reporting each channel’s profile.
- [`block_bootstrap()`](https://elkronos.github.io/mediamix/reference/block_bootstrap.md):
  moving block bootstrap (Kunsch 1989) with percentile intervals for
  ROI, marginal ROI or any statistic.
- [`adstock_steady_state()`](https://elkronos.github.io/mediamix/reference/adstock_steady_state.md)
  seeds a filter at steady state, removing the start-up bias of
  long-carryover channels.
- `diagnose_media(decay =)` measures collinearity on adstocked media.
- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods for
  carryover profiles, joint tuning, bootstrap intervals, contributions
  (over time or in total), response curves (with the extrapolation
  region shaded) and attribution shares, in one colour-vision-checked
  style.
  [`mm_palette()`](https://elkronos.github.io/mediamix/reference/mm_palette.md)
  exports the colours.
  [`contributions()`](https://elkronos.github.io/mediamix/reference/contributions.md),
  [`response_curve()`](https://elkronos.github.io/mediamix/reference/response_curve.md)
  and
  [`attribute()`](https://elkronos.github.io/mediamix/reference/attribute.md)
  now return subclassed data frames so the methods dispatch.
- `inst/CITATION` added.

### Performance

- [`assisted_conversions()`](https://elkronos.github.io/mediamix/reference/assisted_conversions.md)
  is about 16x faster and
  [`top_paths()`](https://elkronos.github.io/mediamix/reference/top_paths.md)
  /
  [`as_channel_paths()`](https://elkronos.github.io/mediamix/reference/as_channel_paths.md)
  about 2x faster on large journey tables, via data.table.
  [`response_curve()`](https://elkronos.github.io/mediamix/reference/response_curve.md)
  computes marginals in one vectorised pass.

### Documentation

- A pkgdown site with an end-to-end budget walkthrough, a methods and
  references article and a comparison with other tools, deployed to
  GitHub Pages by a new workflow.
- `mm_weekly` spend is rescaled so that average ROIs are between about 1
  and 4 rather than 0.07; revenue, decays and shapes are unchanged.
- README and vignette claims about other packages corrected: the README
  no longer says Robyn is not a CRAN package or that no other package
  builds journeys, and the normalised adstock kernel is now credited as
  the Jin et al. (2017) convention rather than a departure from the
  literature. The spine vignette no longer suggests an MMM half-life is
  an attribution lookback.
- The README no longer tells users to `install.packages("mediamix")`;
  the package is not on CRAN.

## mediamix 0.4.0

First release.

### Media transforms

- [`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md),
  [`adstock_weibull()`](https://elkronos.github.io/mediamix/reference/adstock_weibull.md),
  [`adstock_filter()`](https://elkronos.github.io/mediamix/reference/adstock_filter.md)
  and
  [`adstock_state()`](https://elkronos.github.io/mediamix/reference/adstock_state.md)
  apply carryover transforms with explicit, inspectable filter state,
  grouping via `by=`, and an explicit `normalise` argument whose
  consequence for coefficient interpretation is documented.
- [`adstock_weights()`](https://elkronos.github.io/mediamix/reference/adstock_weights.md)
  and
  [`adstock_weights_weibull()`](https://elkronos.github.io/mediamix/reference/adstock_weights_weibull.md)
  expose the kernels directly.
- [`saturate_hill()`](https://elkronos.github.io/mediamix/reference/saturation.md),
  [`saturate_exponential()`](https://elkronos.github.io/mediamix/reference/saturation.md),
  [`saturate_michaelis_menten()`](https://elkronos.github.io/mediamix/reference/saturation.md),
  [`saturate_power()`](https://elkronos.github.io/mediamix/reference/saturation.md)
  and the
  [`saturate()`](https://elkronos.github.io/mediamix/reference/saturation.md)
  dispatcher provide four diminishing-returns curves.
- [`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md)
  composes carryover and saturation in the conventional order and warns
  rather than silently reversing it.
- [`decay_from_half_life()`](https://elkronos.github.io/mediamix/reference/decay_vocabulary.md),
  [`half_life()`](https://elkronos.github.io/mediamix/reference/decay_vocabulary.md)
  and
  [`effective_window()`](https://elkronos.github.io/mediamix/reference/decay_vocabulary.md)
  give both halves of the package one vocabulary.

### Carryover selection

- [`tune_carryover()`](https://elkronos.github.io/mediamix/reference/tune_carryover.md)
  selects `(max_lag, decay)` by cross-validation against an actual KPI
  using a user-supplied model, with forward-only resampling. It warns
  when the selected parameters sit on the edge of the search grid.
- [`fit_ols()`](https://elkronos.github.io/mediamix/reference/fit_ols.md),
  [`rmse()`](https://elkronos.github.io/mediamix/reference/metrics.md)
  and
  [`mae()`](https://elkronos.github.io/mediamix/reference/metrics.md)
  provide working defaults.

### Attribution

- [`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md)
  constructs customer journeys from a raw event log, handling journey
  splitting, lookback windows, non-converting paths, direct and missing
  channel labels, consecutive duplicates, deterministic tie-breaking,
  and the accounting for conversions left unattributable by filtering.
- [`credit_linear()`](https://elkronos.github.io/mediamix/reference/credit.md),
  [`credit_first()`](https://elkronos.github.io/mediamix/reference/credit.md),
  [`credit_last()`](https://elkronos.github.io/mediamix/reference/credit.md),
  [`credit_position()`](https://elkronos.github.io/mediamix/reference/credit.md),
  [`credit_time_decay()`](https://elkronos.github.io/mediamix/reference/credit.md)
  and
  [`credit_custom()`](https://elkronos.github.io/mediamix/reference/credit.md)
  assign credit;
  [`attribute()`](https://elkronos.github.io/mediamix/reference/attribute.md)
  runs several rules at once and
  [`attribution_spread()`](https://elkronos.github.io/mediamix/reference/attribution_spread.md)
  reports how much the answer depends on the choice.
- [`path_summary()`](https://elkronos.github.io/mediamix/reference/path_summary.md),
  [`path_lengths()`](https://elkronos.github.io/mediamix/reference/path_lengths.md),
  [`channel_positions()`](https://elkronos.github.io/mediamix/reference/channel_positions.md),
  [`assisted_conversions()`](https://elkronos.github.io/mediamix/reference/assisted_conversions.md),
  [`top_paths()`](https://elkronos.github.io/mediamix/reference/top_paths.md),
  [`conversion_lag()`](https://elkronos.github.io/mediamix/reference/conversion_lag.md)
  and
  [`path_diagnostics()`](https://elkronos.github.io/mediamix/reference/path_diagnostics.md)
  describe the journey table.
- [`as_channel_paths()`](https://elkronos.github.io/mediamix/reference/as_channel_paths.md)
  exports journeys in ChannelAttribution’s format.

### Recipe steps

- [`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
  and
  [`step_saturation()`](https://elkronos.github.io/mediamix/reference/step_saturation.md)
  carry filter state across the train/test boundary without changing row
  count or order, classifying new data as contiguous, overlapping,
  gapped or unseen and warm-starting only when that is correct.
- [`tunable()`](https://generics.r-lib.org/reference/tunable.html)
  methods and the
  [`carryover_decay()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md),
  [`carryover_max_lag()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md),
  [`saturation_half_max()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md)
  and
  [`saturation_shape()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md)
  parameter objects support joint tuning with model hyperparameters.

### Reporting and diagnostics

- [`contributions()`](https://elkronos.github.io/mediamix/reference/contributions.md),
  [`roi()`](https://elkronos.github.io/mediamix/reference/roi.md),
  [`mroi()`](https://elkronos.github.io/mediamix/reference/roi.md),
  [`response_curve()`](https://elkronos.github.io/mediamix/reference/response_curve.md)
  and
  [`spend_for()`](https://elkronos.github.io/mediamix/reference/response_curve.md)
  turn a fitted model into a marketing mix deliverable.
- [`diagnose_media()`](https://elkronos.github.io/mediamix/reference/diagnose_media.md)
  reports collinearity, insufficient variation, flighting and implied
  CPM inconsistency.

### Data

- `mm_weekly`, a synthetic weekly panel generated from known parameters.
- `mm_events`, a synthetic touchpoint log containing the awkward cases
  on purpose.
