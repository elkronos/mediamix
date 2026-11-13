# mediamix 0.4.0.9000

## Breaking changes

* Recipes prepared with `step_adstock()` or `step_saturation()` by an earlier
  version must be prepared again; `bake()` stops on the old format. A prepared
  `step_adstock()` now stores each group's training time points and media
  values (about 25 KB for `mm_weekly`).
* `tidy()` on the recipe steps returns a tibble; an ungrouped adstock step
  shows `group` as `NA`, and `step_saturation()` shows `NA` for parameters its
  curve ignores. Keys for numeric and date-time `by` columns are written from
  their values (100000 rather than "1e+05"; date-times in UTC).
* `tune_carryover()`'s `metric` field is the score; the metric's name moved to
  `metric_name`, matching `tune_carryover_joint()`. `std_err` is the standard
  error of `metric`, so `best_1se` can differ from earlier versions, and
  `half_life` is `NA` for `max_lag = 1`.
* `tune_carryover()` pools out-of-sample forecasts by default
  (`aggregate = "pooled"`), so its reported RMSE is a root mean squared
  forecast error. Per-split metrics used to be averaged, so with the default
  one-period assessment sets the reported "rmse" was a mean absolute error.
  `aggregate = "mean"` restores the previous averaging of per-split metrics.
* `adstock_state()` returns an `mm_adstock_state` that records `max_lag` (and,
  for the infinite kernel, `decay` and `kernel`); the transforms refuse a state
  computed for another kernel. `as.numeric()` gives the bare value, and plain
  numbers are still accepted as `state`.
* `adstock_steady_state()` stops on missing values by default (new
  `na_action`), `rmse()` and `mae()` stop on empty input, and
  `block_bootstrap()` stops when `block_length` covers a whole series.
* Inputs that used to pass, or were altered without a message, now stop with a
  message naming the problem: text timestamps that do not parse completely,
  unrecognised conversion flags, missing values in controls, `by` columns or a
  step's time index, infinite spend or state, constant or unnamed media
  columns in the tuners, repeated media names in `diagnose_media()`, spend text
  that is not a number, `tol` outside (0, 0.5) in `step_adstock()`, and
  overlapping journey ids under `rbind()`.
* `build_paths()` drops exact duplicate events by default (`dedupe = TRUE`) and
  sorts a conversion after touches that share its timestamp. On `mm_events`
  this gives 6,105 journeys (was 6,137).
* `block_bootstrap(by =)` gives every group the same time blocks by default
  (`panel = "shared"`), so panel intervals change; unequal group sizes need
  `index` or `panel = "independent"`.
* `contributions()`, `response_curve()` and `attribute()` return subclassed
  data frames (`mm_contributions`, `mm_response_curve`, `mm_attribution`) so
  that `plot()` dispatches. Code that tests `class(x)` exactly needs updating.
* `plot()` methods return their input invisibly. Bootstrap, contributions,
  response-curve and attribution plots used to return a modified data frame.
* `diagnose_media()`'s CPM table names its row column `row` (it was `period`,
  but held a row number) and gains a `group` column when `by` is supplied.
* `mm_weekly` spend and half-saturation points are rescaled by 1/20, so that
  average ROIs are between about 1 and 4 rather than 0.07, and coefficients
  and ROIs computed from it differ from 0.4.0. Decays and shapes are
  unchanged, and revenue moves only through rounding of the rescaled spend.

## Bug fixes

* Grouping by several columns no longer merges two groups when a value
  contains the character used to join the columns into one key; such values
  are refused with a message.
* Error messages no longer suggest `Inf` for an argument that refuses it, and
  give an infinite bound as open when infinite values are refused.
* `rlang` is now in Imports. Every error, warning and message goes through
  `cli`, which calls `rlang`, so without it installed even a warning stopped
  the call with "there is no package called 'rlang'".
* `build_paths()` reads character timestamps completely: ISO 8601 with `T` or
  a space, fractional seconds and offsets such as `Z` or `+02:00`. Other text,
  or a column mixing dates and date-times, is an error. Such timestamps used
  to be cut to midnight, which reordered touches within a day.
* `build_paths()` now reports when journeys hold several conversion events
  (possible under `split_on = "none"` or `"gap"`), since each is credited as
  one conversion.
* dplyr verbs and `rbind()` keep journey tables valid: `touch_rank`, `touch_n`
  and the construction counts follow the rows, and functions that take a
  journey table recompute stale ranks with a message. New `bind_paths()`
  combines tables built by separate `build_paths()` calls.
* Subsetting an `mm_paths` object so that part of a journey is removed now
  recomputes `touch_rank` and `touch_n`. Previously the stale ranks made
  `credit_first()`, `credit_last()` and `credit_position()` give some
  converting journeys no credit at all after, for example, filtering out one
  channel.
* `credit_time_decay(decay = 0)` credits the most recent touch whatever the row
  order, and every rule sees journeys in `touch_rank` order. Character
  conversion flags are read as numbers first, numeric ids keep their digits,
  numeric timestamps record `"index"` units, and an `NA` row index no longer
  inserts rows of missing values.
* `media_transform()` honours `by` inside the `adstock` list.
* `effective_window()` is exact at whole half-life boundaries, transforms keep
  names and time-series attributes, `saturate_exponential()` keeps precision
  for small inputs, and `fit_ols()` keeps slopes at any scale.
* The saturate-before-adstock warning and docs give the reason that applies to
  the kernel: with the default normalised kernel the result stays under the
  ceiling. The Markov null-path warning no longer claims every removal effect
  is overstated.
* `step_adstock()` continues the filter exactly when new data repeats the end
  of the training data, as `rsample::rolling_origin(lag =)` assessment sets do;
  those rows were cold-started without a warning. A block that starts less than
  one period after a training time point warns (or errors under
  `carry_over = "warm"`), and the period is inferred within each group.
* `step_saturation()` offers for tuning only the parameters its curve uses,
  and `print()` and `tidy()` accept `tune()` placeholders.
* The tuners refuse missing controls instead of failing or shifting rows,
  compare every candidate on the splits all of them completed, warn at both
  edges of the searched grid, and quote the first failure of `fit_fn`,
  `predict_fn` or `metric_fn`. With `cores > 1`, one failing grid point no
  longer marks its neighbours as failed.
* The one-standard-error rule compares each candidate with the reported `best`,
  using the standard error of the paired difference in the same metric.
  `tune_carryover_joint()` reports the error of its combined `best_1se` set as
  `best_1se_metric`.
* `warm_start` seeds the filter only from media inside the first training
  window.
* `contributions()` no longer applies the training fit's fitted values to other
  rows (new `newdata`), keeps the controls in the baseline when the model
  dropped rows for missing values, matches back-ticked coefficient names,
  reports media columns that match no coefficient, warns for non-identity
  links, accepts one-row media and uses the fixed effects of nlme and lme4
  models.
* `roi()` and `diagnose_media()` read factor spend by its labels, `roi()` warns
  when contributions and spend cover different periods, and `diagnose_media()`
  flags CPM outliers when most CPMs are identical and accepts data.table input.
* `diagnose_media()` now computes the implied-CPM median and outlier rule
  within each series when `by` is supplied. Pooled across geographies, a
  region that buys media at a different price was flagged as a stream of
  join errors.
* `mroi()`, `response_curve()` and `spend_for()` accept `type = "none"`, scale
  their derivative step to the curve and refuse targets no spend can reach.
* The documentation and the getting-started vignette recommended computing
  marginal ROI on spend as the saturation slope times the kernel's first
  weight. That counts only the period of spend and understated slow
  channels' marginal return several-fold (six-fold for `mm_weekly`'s
  television), reversing a reallocation conclusion. See `marginal_roi()`.
* Plots: the joint-tuning plot draws one line per `max_lag`; the contributions
  chart sorts periods and labels date-times as dates; a missing contribution or
  share gives a warning instead of removing a channel; methods name any column
  they need that is missing.
* `block_bootstrap()` draws every resample before calling the statistic (a
  statistic that set a seed made replicates repeat), sets `seed` before the
  point estimate, matches replicates by name, counts missing values per term
  and reports the first failure.
* Group keys are type-stable (1e5 and 100000L are one group), a blank label is
  an ordinary group, and groups are ordered by first appearance, so seeded
  `block_bootstrap(by =)` results no longer depend on the locale.

## New features

* Budget planning from a per-channel spec, a named list of
  `media_transform()` arguments: `transform_plan()` transforms every channel
  of a plan, `simulate_plan()` reports each channel's spend and predicted
  response (with `baseline =`, the change from another plan), `reallocate()`
  moves spend between two channels and keeps both channels' flighting, and
  `allocate_budget()` splits a total across channels within per-channel
  bounds, by dynamic programming on a grid of spend levels, with a `plot()`
  method. `marginal_roi(spec =)` takes a data frame of spend and returns one
  row per channel.
* `from_robyn()` converts one channel's Robyn hyperparameters (geometric
  `theta`, Weibull CDF `shape` and `scale`, Hill `alpha` and `gamma`) into
  `media_transform()` arguments that reproduce Robyn's transformed series.
  `inflexion` chooses Robyn's Hill rule for version 3.12.0 and later, or for
  3.7.0 to 3.11.1.
* `fourier_terms()` and `holiday_flags()` build seasonality controls: sine and
  cosine pairs by time of year, and flags for the daily or weekly periods
  that contain a holiday.
* `step_adstock()` gains Weibull and delayed kernels (`kernel`, `shape`,
  `scale`, `type`, `peak`), steady-state starts (`start = "steady"`), and
  `keep_original_cols` with `prefix` to keep the raw spend beside the
  adstocked column. `step_adstock_channels()` adds one step per channel, each
  with its own decay or its own `tune()` id, and `carryover_shape()`,
  `carryover_scale()` and `carryover_peak()` are the new kernels' dials
  parameters.
* `diagnose_media(date =)` checks each series for rows out of time order,
  repeated periods, missing periods and uneven steps.
* `roi()` gains `by`, `index` and `periods` for returns by group and period.
* `block_bootstrap(cores =)` evaluates replicates in parallel, with results
  that do not depend on `cores`. `tune_carryover()` and
  `tune_carryover_joint()` keep out-of-sample predictions with `save_pred`.
* `build_paths(keep =)` carries other event columns, such as campaign, device
  or cost, into the journey table, and `conversion_touch = "drop"` stops the
  channel recorded on a conversion from counting as a touch.
* `attribute(by =)` attributes each segment's journeys separately, and
  `attribute(times =, level =, seed =)` adds percentile intervals on the
  shares from resampled journeys, with `plot()` methods for both.
  `markov_removal()` gains `p_without` and returns the fitted transition
  matrix as an attribute.
* `decay_from_half_life()`, `half_life()` and `effective_window()` are
  vectorised over their first argument.
* `marginal_roi()` computes average and marginal return by re-running
  carryover and saturation on a slightly larger budget, following Jin et al.
  (2017), with optional planning windows and carryover run-out.
* `tune_carryover_joint()` tunes every channel's carryover inside one model
  with controls, by coordinate descent, reporting each channel's profile.
* `block_bootstrap()`: moving block bootstrap (Künsch, 1989) with percentile
  intervals for ROI, marginal ROI or any statistic.
* `markov_removal()` implements order-1 Markov removal-effect attribution
  (Anderl et al., 2016) exactly, in base R. `attribute()` includes it as the
  `"markov"` rule.
* `adstock_delayed()` and `adstock_weights_delayed()` add the delayed-peak
  kernel of Jin et al. (2017); `media_transform()` accepts
  `kernel = "delayed"`.
* `adstock_steady_state()` seeds a filter at steady state, removing the
  start-up bias of long-carryover channels.
* `tune_carryover()` reports `std_err` and a `best_1se` choice using a paired
  one-standard-error rule, and gains `controls` (entered into the model and
  kept aligned with every split) and `warm_start`.
* `by` and `index` in `tune_carryover()` and `tune_carryover_joint()` tune a
  panel of geographies; `se_block` gives a block-jackknife standard error;
  `tune_carryover_joint()` gains `cores`.
* `diagnose_media(decay =)` measures collinearity on adstocked media.
* `plot()` methods for carryover profiles, joint tuning, bootstrap intervals,
  contributions (over time or in total), response curves (with the
  extrapolation region shaded) and attribution shares, in one
  colour-vision-checked style. `mm_palette()` exports the colours.
* A `plot()` method for `diagnose_media()` results. `plot()` methods accept
  `main`, `xlab`, `ylab` and `col`, use k, M and B axis labels, and fold more
  than eight series into "Other" instead of dropping them.
* `as.data.frame()` and `tidy()` methods for `tune_carryover()`,
  `tune_carryover_joint()`, `diagnose_media()` and `block_bootstrap()` results;
  `coef()`, `fitted()` and `predict()` without `newdata` for `fit_ols()`.
* `contributions(newdata =)`; `block_bootstrap(panel =, index =, circular =)`.
* `step_saturation(by =)` gives each group its own reference level;
  `step_adstock(period =)` overrides the inferred spacing.
* `media_transform()` accepts `adstock = NULL` or `saturation = NULL` to switch
  a stage off, and the adstock functions gain `missing_state = "cold"`.
* `bind_paths()`, and `build_paths(timestamp_format =, dedupe =)`;
  `path_summary()` reports `duplicate_events` and `journeys_before_filtering`.
* `mm_weekly`'s `"truth"` attribute records each geography's scale factor and
  a per-geography table of channel parameters (`by_geo`), so a model fitted to
  any geography can be checked; the data are unchanged.
* `inst/CITATION` added.

## Performance

* `build_paths()` is about four times faster (1.2 million events in about
  2 s), `assisted_conversions()` is linear in journey length,
  `markov_removal()` solves all removal effects from one matrix inverse,
  `tune_carryover_joint()` is about four times faster, and grouped adstock and
  `step_adstock()`'s `bake()` are linear in the number of groups.
* `assisted_conversions()` is about 16 times faster and `top_paths()` /
  `as_channel_paths()` about twice as fast on large journey tables, via
  data.table. `response_curve()` computes marginals in one vectorised pass.

## Documentation

* References carry a DOI wherever one exists.
* The budget walkthrough checks its recommendation against the parameters the
  data was simulated from. The fitted model ranks the channels differently
  from the truth, and the article now presents the reallocation as a test
  rather than a finding. The getting-started vignette sets fitted marginal
  returns against true ones in the same way.
* The tidymodels vignette's per-channel loop used `all_of(ch)`, which is
  evaluated after the loop ends, so all five steps adstocked `display`. It now
  uses `all_of(!!ch)` and prints what each step selects.
* The methods article cites Tashman (2000) for rolling-origin evaluation;
  Bergmeir and Benítez (2012) recommend blocked cross-validation.
* The README is knitted from `README.Rmd`, so its outputs and figures come from
  the code shown, and numbers in the vignettes' prose are computed inline.
* Claims corrected: dropping null paths does not inflate every removal
  effect; `step_naomit()` does not break `tune_grid()`; out-of-sample
  prediction is evidence of predictive value, not of a causal mechanism; and
  the joint tuner is closer to the truth than one-at-a-time tuning but not
  better for every channel.
* README and vignette claims about other packages corrected: the README no
  longer says Robyn is not a CRAN package or that no other package builds
  journeys, and the normalised adstock kernel is now credited as the Jin et
  al. (2017) convention rather than a departure from the literature. The
  spine vignette no longer suggests an MMM half-life is an attribution
  lookback.
* The README no longer tells users to `install.packages("mediamix")`; the
  package is not on CRAN.
* A pkgdown site with an end-to-end budget walkthrough, a methods and
  references article and a comparison with other tools, deployed to GitHub
  Pages by a new workflow.

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
