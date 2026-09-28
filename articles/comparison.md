# How mediamix relates to other tools

mediamix is deliberately narrower than the full marketing mix modelling
frameworks. This page sets out what each tool is for, where they
overlap, and when to reach for which. Tools change quickly; this
reflects them as of 2026, and each project’s own documentation is the
authority on its current features.

## The landscape

**Robyn** (Meta, R). An end-to-end MMM: geometric and Weibull adstock,
Hill saturation, ridge regression, multi-objective hyperparameter search
with the Python library `nevergrad` (reached through `reticulate`),
calibration to experiments, and a budget allocator. Use it when you want
Meta’s full, opinionated pipeline.

**Meridian** (Google, Python). A Bayesian hierarchical MMM, built for
geo-level data, with priors on ROI, reach and frequency support and
experiment calibration. Its methodology descends from Jin et al. (2017).

**PyMC-Marketing** (PyMC Labs, Python). Bayesian MMM with configurable
adstock and saturation, budget optimisation, and customer-lifetime-value
models, built on PyMC.

**ChannelAttribution** (R and Python). Markov-chain attribution of any
order, and heuristic models, in C++, starting from aggregated path
strings.

**recipes / tune** (R, tidymodels). General-purpose preprocessing and
hyperparameter tuning, with no media-specific steps.

## Where mediamix fits

mediamix is the transform, attribution and reporting layer on its own,
in R, with no compiled code and no Python:

| Need | mediamix | Full framework |
|----|----|----|
| Adstock and saturation inside your own [`lm()`](https://rdrr.io/r/stats/lm.html), `brms`, `glmnet` or Stan model | Yes — plain functions on vectors | Usually inside the framework’s model |
| Carryover tuned jointly with a model penalty in tidymodels | [`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md) keeps state across resampling boundaries | Framework-specific search |
| Per-channel carryover by cross-validation in your own model | [`tune_carryover_joint()`](https://elkronos.github.io/mediamix/reference/tune_carryover_joint.md) | Framework-specific search |
| Marginal ROI by counterfactual simulation | [`marginal_roi()`](https://elkronos.github.io/mediamix/reference/marginal_roi.md) | Robyn, Meridian, PyMC-Marketing all report one |
| Journeys built from a raw event log | [`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md) | Not covered by the MMM frameworks; ChannelAttribution starts from paths |
| Heuristic and Markov attribution side by side | [`attribute()`](https://elkronos.github.io/mediamix/reference/attribute.md), [`attribution_spread()`](https://elkronos.github.io/mediamix/reference/attribution_spread.md) | ChannelAttribution (Markov and heuristics) |
| Uncertainty intervals | [`block_bootstrap()`](https://elkronos.github.io/mediamix/reference/block_bootstrap.md) around any model | Posterior intervals in Meridian, PyMC-Marketing; bootstrap in Robyn |
| Bayesian estimation with priors | No — use `brms` or Stan on mediamix transforms | Meridian, PyMC-Marketing |
| Budget optimisation | No — [`marginal_roi()`](https://elkronos.github.io/mediamix/reference/marginal_roi.md) and scenario simulation only | Robyn, Meridian, PyMC-Marketing |
| Reach and frequency, geo hierarchies | No | Meridian |

## Choosing

- **You want a complete, supported MMM product and are happy with its
  modelling choices:** Robyn in R, or Meridian or PyMC-Marketing in
  Python.
- **You already have a modelling approach** — a regression, a `brms`
  model, a tidymodels workflow — and need correct media transforms and
  honest reporting around it: mediamix.
- **You have an event log and need journeys, rule-based credit and
  Markov removal effects without building the paths by hand:** mediamix,
  with
  [`as_channel_paths()`](https://elkronos.github.io/mediamix/reference/as_channel_paths.md)
  to hand off to ChannelAttribution for higher-order chains.
- **You need incrementality:** none of these on their own. Run an
  experiment, and use it to check or calibrate whichever model you
  choose.

## Moving parameters between tools

- Geometric decay means the same thing everywhere, but mediamix
  normalises the kernel by default and Robyn does not. Use
  `normalise = FALSE` to reproduce Robyn’s adstocked series; the decay
  itself is unchanged.
- Robyn’s Weibull `scale` is a quantile of the window length, not a
  number of periods, so it does not transfer directly to
  [`adstock_weibull()`](https://elkronos.github.io/mediamix/reference/adstock_weibull.md).
- Hill `half_max` is in the units of the adstocked media. Some tools
  express it as a fraction of a column’s range or maximum instead, as
  [`step_saturation()`](https://elkronos.github.io/mediamix/reference/step_saturation.md)
  does.
- For `order = 1`,
  [`markov_removal()`](https://elkronos.github.io/mediamix/reference/markov_removal.md)
  and
  [`ChannelAttribution::markov_model()`](https://rdrr.io/pkg/ChannelAttribution/man/markov_model.html)
  implement the same removal-effect definition; small differences come
  from ChannelAttribution’s simulation.

## Reference

Jin, Y., Wang, Y., Sun, Y., Chan, D. and Koehler, J. (2017). *Bayesian
methods for media mix modeling with carryover and shape effects.* Google
Inc.
