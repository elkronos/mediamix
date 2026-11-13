# Which function do I need?

Each table lists tasks in the order a media mix or attribution project
meets them, with the function that does each one and, where it is the
part that does the job, the argument. `mm_weekly`, a simulated weekly
media panel, and `mm_events`, a simulated event log, are there to try
each one on.

## Check the data

| Task | Function |
|----|----|
| Find channels that move together | [`diagnose_media()`](https://elkronos.github.io/mediamix/reference/diagnose_media.md), its `collinearity` table |
| Measure that collinearity after adstock, as the model sees it | [`diagnose_media()`](https://elkronos.github.io/mediamix/reference/diagnose_media.md) with `decay` |
| Find channels that barely vary or are mostly dark | [`diagnose_media()`](https://elkronos.github.io/mediamix/reference/diagnose_media.md), its `variation` table |
| Check spend against impressions for join errors | [`diagnose_media()`](https://elkronos.github.io/mediamix/reference/diagnose_media.md) with `spend` and `impressions`, its `cpm` table |
| Find missing, repeated or out-of-order weeks | [`diagnose_media()`](https://elkronos.github.io/mediamix/reference/diagnose_media.md) with `date`, its `dates` table |
| Check each geography on its own | [`diagnose_media()`](https://elkronos.github.io/mediamix/reference/diagnose_media.md) with `by` |
| Plot the checks | [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on the result; see [`plot.mm_diagnosis()`](https://elkronos.github.io/mediamix/reference/plot.mm_diagnosis.md) |

## Transform media

### Carryover

| Task | Function |
|----|----|
| Spread spend forward with a constant decay | [`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md) |
| Let the effect peak a few periods after the spend | [`adstock_delayed()`](https://elkronos.github.io/mediamix/reference/adstock_delayed.md) with `peak`, or [`adstock_weibull()`](https://elkronos.github.io/mediamix/reference/adstock_weibull.md) with `type = "pdf"` |
| Use a Weibull kernel | [`adstock_weibull()`](https://elkronos.github.io/mediamix/reference/adstock_weibull.md) |
| Apply a kernel you already have as weights | [`adstock_filter()`](https://elkronos.github.io/mediamix/reference/adstock_filter.md) with `weights` |
| Look at a kernel’s weights | [`adstock_weights()`](https://elkronos.github.io/mediamix/reference/adstock_weights.md), [`adstock_weights_weibull()`](https://elkronos.github.io/mediamix/reference/adstock_weights_weibull.md), [`adstock_weights_delayed()`](https://elkronos.github.io/mediamix/reference/adstock_delayed.md) |
| Turn a half-life into a decay, and back | [`decay_from_half_life()`](https://elkronos.github.io/mediamix/reference/decay_vocabulary.md), [`half_life()`](https://elkronos.github.io/mediamix/reference/decay_vocabulary.md); `period` converts between daily and weekly data |
| Decide how many lags a kernel needs | [`effective_window()`](https://elkronos.github.io/mediamix/reference/decay_vocabulary.md) |
| Keep the coefficient per unit of spend, or match Robyn’s scale | `normalise = TRUE`, the default, or `normalise = FALSE` as in Robyn |
| Adstock several geographies at once | `by` in the adstock functions and in [`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md) |
| Treat missing spend as no spend | `na_action = "zero"` in the adstock functions, the default in [`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md) |
| Continue a series from where an earlier chunk stopped | [`adstock_state()`](https://elkronos.github.io/mediamix/reference/adstock_state.md), passed as `state` |
| Remove the low first weeks of a long carryover | [`adstock_steady_state()`](https://elkronos.github.io/mediamix/reference/adstock_steady_state.md), passed as `state` |

### Saturation

| Task | Function |
|----|----|
| Saturate with a Hill curve, concave or S-shaped | [`saturate_hill()`](https://elkronos.github.io/mediamix/reference/saturation.md) with `half_max` and `shape` |
| Saturate with another curve | [`saturate_exponential()`](https://elkronos.github.io/mediamix/reference/saturation.md), [`saturate_michaelis_menten()`](https://elkronos.github.io/mediamix/reference/saturation.md), [`saturate_power()`](https://elkronos.github.io/mediamix/reference/saturation.md); [`saturate()`](https://elkronos.github.io/mediamix/reference/saturation.md) picks one by `type` |

### Both, channel by channel

| Task | Function |
|----|----|
| Adstock then saturate one channel | [`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md) |
| Transform every channel of a data frame, each with its own parameters | [`transform_plan()`](https://elkronos.github.io/mediamix/reference/transform_plan.md) with `spec` |
| Reproduce a Robyn model’s transforms | [`from_robyn()`](https://elkronos.github.io/mediamix/reference/from_robyn.md), then [`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md) |
| Transform inside a tidymodels recipe | [`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md), then [`step_saturation()`](https://elkronos.github.io/mediamix/reference/step_saturation.md) |
| Give each channel its own decay in a recipe | [`step_adstock_channels()`](https://elkronos.github.io/mediamix/reference/step_adstock_channels.md), with `decay = c(tv = 0.85, search = 0.15)` |
| Keep the raw spend beside the adstocked column in a recipe | [`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md) with `keep_original_cols = TRUE` |
| Start each series in a recipe at its steady state | [`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md) with `start = "steady"` |

## Choose carryover

| Task | Function |
|----|----|
| Choose one channel’s decay against the KPI | [`tune_carryover()`](https://elkronos.github.io/mediamix/reference/tune_carryover.md) |
| Choose every channel’s decay inside one model | [`tune_carryover_joint()`](https://elkronos.github.io/mediamix/reference/tune_carryover_joint.md) |
| Hold trend, price and seasonality fixed while choosing | `controls` in either tuner |
| Tune a panel of geographies | `by` and `index` in either tuner |
| Remove the start-up bias of long carryover while tuning | `warm_start = TRUE` |
| Take the shortest carryover the data cannot rule out | the `best_1se` element of the result |
| See how sharply the data identifies each decay | [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on the result; see [`plot.mm_carryover()`](https://elkronos.github.io/mediamix/reference/plot_carryover.md) |
| Score candidates with your own model or metric | `fit_fn`, `predict_fn` and `metric_fn`; [`rmse()`](https://elkronos.github.io/mediamix/reference/metrics.md) is the default metric and [`mae()`](https://elkronos.github.io/mediamix/reference/metrics.md) the alternative supplied |
| Keep the out-of-sample forecasts | `save_pred = TRUE` |
| Tune carryover, saturation and a model penalty together | [`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md) or [`step_adstock_channels()`](https://elkronos.github.io/mediamix/reference/step_adstock_channels.md) with [`step_saturation()`](https://elkronos.github.io/mediamix/reference/step_saturation.md), marked with [`tune()`](https://hardhat.tidymodels.org/reference/tune.html) and searched by the tune package |
| Set the ranges a tidymodels search covers | [`carryover_decay()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md), [`carryover_max_lag()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md), [`carryover_shape()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md), [`carryover_scale()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md), [`carryover_peak()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md), [`saturation_half_max()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md), [`saturation_shape()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md) |

## Add controls such as seasonality

| Task | Function |
|----|----|
| Model a smooth yearly cycle | [`fourier_terms()`](https://elkronos.github.io/mediamix/reference/seasonality.md) with `harmonics` |
| Flag the weeks that contain a holiday | [`holiday_flags()`](https://elkronos.github.io/mediamix/reference/seasonality.md), with `align = "end"` for week-ending dates |
| Use them while choosing carryover | `controls` in [`tune_carryover()`](https://elkronos.github.io/mediamix/reference/tune_carryover.md) or [`tune_carryover_joint()`](https://elkronos.github.io/mediamix/reference/tune_carryover_joint.md) |

For a trend, a column counting the weeks is enough.

## Fit a model

mediamix has no model of its own. The transformed columns are ordinary
numeric vectors, so [`lm()`](https://rdrr.io/r/stats/lm.html),
[`glmnet::glmnet()`](https://glmnet.stanford.edu/reference/glmnet.html),
a brms or Stan model, or a tidymodels workflow can take them.
[`fit_ols()`](https://elkronos.github.io/mediamix/reference/fit_ols.md),
a one-regressor least-squares fit, is there as
[`tune_carryover()`](https://elkronos.github.io/mediamix/reference/tune_carryover.md)’s
default model.

## Report contributions and returns

| Task | Function |
|----|----|
| Split the model’s predictions into channel contributions and a baseline | [`contributions()`](https://elkronos.github.io/mediamix/reference/contributions.md) |
| Decompose a holdout or reporting period | [`contributions()`](https://elkronos.github.io/mediamix/reference/contributions.md) with `newdata` |
| Decompose each geography | [`contributions()`](https://elkronos.github.io/mediamix/reference/contributions.md) with `by` |
| Plot contributions over time, or in total | [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on the result, with `type`; see [`plot.mm_contributions()`](https://elkronos.github.io/mediamix/reference/plot.mm_contributions.md) |
| Average return per channel | [`roi()`](https://elkronos.github.io/mediamix/reference/roi.md) |
| Average return per geography or per year | [`roi()`](https://elkronos.github.io/mediamix/reference/roi.md) with `by`, or with `index` and `periods` |
| Return on the next unit of spend | [`marginal_roi()`](https://elkronos.github.io/mediamix/reference/marginal_roi.md) |
| Marginal return for every channel in one call | [`marginal_roi()`](https://elkronos.github.io/mediamix/reference/marginal_roi.md) on a data frame of spend, with `spec` |
| Count the carryover that falls after the last week | [`marginal_roi()`](https://elkronos.github.io/mediamix/reference/marginal_roi.md) with `extend` |
| Raise spend only in a planning window | [`marginal_roi()`](https://elkronos.github.io/mediamix/reference/marginal_roi.md) with `rows` |
| Slope of a saturation curve at one level | [`mroi()`](https://elkronos.github.io/mediamix/reference/roi.md) |
| A channel’s response across spend levels | [`response_curve()`](https://elkronos.github.io/mediamix/reference/response_curve.md); [`plot.mm_response_curve()`](https://elkronos.github.io/mediamix/reference/plot.mm_response_curve.md) shades the levels outside `observed` |
| Spend that reaches a target response | [`spend_for()`](https://elkronos.github.io/mediamix/reference/response_curve.md) |

## Measure uncertainty

| Task | Function |
|----|----|
| Intervals on coefficients, contributions or returns | [`block_bootstrap()`](https://elkronos.github.io/mediamix/reference/block_bootstrap.md) |
| Choose how many weeks a resampled block holds | `block_length` in [`block_bootstrap()`](https://elkronos.github.io/mediamix/reference/block_bootstrap.md), at least the longest [`effective_window()`](https://elkronos.github.io/mediamix/reference/decay_vocabulary.md) |
| Resample a panel so shocks shared by every geography stay together | `by` in [`block_bootstrap()`](https://elkronos.github.io/mediamix/reference/block_bootstrap.md), with `panel = "shared"`, the default |
| Spread the replicates over several cores | `cores` in [`block_bootstrap()`](https://elkronos.github.io/mediamix/reference/block_bootstrap.md) |
| Plot the intervals | [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on the result, with `reference`; see [`plot.mm_bootstrap()`](https://elkronos.github.io/mediamix/reference/plot.mm_bootstrap.md) |
| Intervals on attribution shares | [`attribute()`](https://elkronos.github.io/mediamix/reference/attribute.md) with `times` |

## Plan budgets

| Task | Function |
|----|----|
| Predict each channel’s response to a spend plan | [`simulate_plan()`](https://elkronos.github.io/mediamix/reference/simulate_plan.md) |
| Compare one plan with another | [`simulate_plan()`](https://elkronos.github.io/mediamix/reference/simulate_plan.md) with `baseline` |
| Move spend between two channels, keeping each one’s flighting | [`reallocate()`](https://elkronos.github.io/mediamix/reference/reallocate.md) |
| Set up a move in one region only, as a geo test | [`reallocate()`](https://elkronos.github.io/mediamix/reference/reallocate.md) with `by`, and `amount` named by region |
| Split a total budget to maximise the predicted response | [`allocate_budget()`](https://elkronos.github.io/mediamix/reference/allocate_budget.md) with `total` |
| Keep each channel’s spend within limits | [`allocate_budget()`](https://elkronos.github.io/mediamix/reference/allocate_budget.md) with `lower` and `upper` |
| Plot an allocation | [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on the result; see [`plot.mm_allocation()`](https://elkronos.github.io/mediamix/reference/plot.mm_allocation.md) |

## Build journeys

| Task | Function |
|----|----|
| Turn a raw event log into customer journeys | [`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md) |
| Start a new journey after each conversion or a quiet spell | [`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md) with `split_on` and `gap` |
| Drop touches older than a window | [`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md) with `lookback` |
| Choose that window from your own data | [`conversion_lag()`](https://elkronos.github.io/mediamix/reference/conversion_lag.md) |
| Keep, drop or relabel direct and missing channels | [`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md) with `direct` |
| Carry campaign, device or cost columns into the journeys | [`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md) with `keep` |
| Stop counting a checkout page as a touch | [`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md) with `conversion_touch = "drop"` |
| Read text timestamps in a format other than ISO 8601 | [`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md) with `timestamp_format` |
| Count events in, journeys out and conversions lost | [`path_summary()`](https://elkronos.github.io/mediamix/reference/path_summary.md) |
| Describe the journeys | [`path_lengths()`](https://elkronos.github.io/mediamix/reference/path_lengths.md), [`channel_positions()`](https://elkronos.github.io/mediamix/reference/channel_positions.md), [`assisted_conversions()`](https://elkronos.github.io/mediamix/reference/assisted_conversions.md), [`top_paths()`](https://elkronos.github.io/mediamix/reference/top_paths.md); all of them at once with [`path_diagnostics()`](https://elkronos.github.io/mediamix/reference/path_diagnostics.md) |
| Combine journeys built in separate batches | [`bind_paths()`](https://elkronos.github.io/mediamix/reference/bind_paths.md) |
| Subset journeys and keep their touch ranks valid | `[` or dplyr verbs on the journey table; see [`[.mm_paths`](https://elkronos.github.io/mediamix/reference/sub-.mm_paths.md) |

## Credit conversions

| Task | Function |
|----|----|
| Credit by a fixed rule | [`credit_first()`](https://elkronos.github.io/mediamix/reference/credit.md), [`credit_last()`](https://elkronos.github.io/mediamix/reference/credit.md), [`credit_linear()`](https://elkronos.github.io/mediamix/reference/credit.md), [`credit_position()`](https://elkronos.github.io/mediamix/reference/credit.md), [`credit_time_decay()`](https://elkronos.github.io/mediamix/reference/credit.md) |
| Credit by a rule of your own | [`credit_custom()`](https://elkronos.github.io/mediamix/reference/credit.md) with `fn` |
| Credit by Markov removal effects | [`markov_removal()`](https://elkronos.github.io/mediamix/reference/markov_removal.md) |
| Inspect the fitted Markov chain | the `transitions` and `p_conversion` attributes of the result of [`markov_removal()`](https://elkronos.github.io/mediamix/reference/markov_removal.md) |
| Run several rules side by side | [`attribute()`](https://elkronos.github.io/mediamix/reference/attribute.md) with `rules` |
| See how much each channel’s share depends on the rule | [`attribution_spread()`](https://elkronos.github.io/mediamix/reference/attribution_spread.md) |
| Credit each segment, such as device, separately | [`attribute()`](https://elkronos.github.io/mediamix/reference/attribute.md) with `by` |
| Plot shares across rules | [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on the result; see [`plot.mm_attribution()`](https://elkronos.github.io/mediamix/reference/plot.mm_attribution.md) and [`plot.mm_attribution_intervals()`](https://elkronos.github.io/mediamix/reference/plot.mm_attribution_intervals.md) |

## Export

| Task | Function |
|----|----|
| Hand journeys to ChannelAttribution for higher-order chains | [`as_channel_paths()`](https://elkronos.github.io/mediamix/reference/as_channel_paths.md), with the same `sep` on both sides |
| Get a tuner, diagnosis, bootstrap or allocation result as a plain table | [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) or [`tidy()`](https://generics.r-lib.org/reference/tidy.html) on it |
| Read a recipe step’s fitted state and parameters | [`tidy()`](https://generics.r-lib.org/reference/tidy.html) on the prepared recipe |
| Match your own charts to the package’s | [`mm_palette()`](https://elkronos.github.io/mediamix/reference/mm_palette.md) |
