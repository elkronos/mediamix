# mediamix

<!-- badges: start -->
[![R-CMD-check](https://github.com/elkronos/mediamix/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/elkronos/mediamix/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Composable media transforms and attribution path construction for R.

mediamix turns raw marketing data into model-ready features: media spend into
carryover- and saturation-adjusted regressors, and raw event logs into
attributed customer journeys.

It is a preprocessing and feature-engineering package. It is not a marketing
mix modelling framework, not a modelling engine, and not an attribution
methodology. `lm()`, `glmnet` and `brms` fit models better than a marketing
package would; `ChannelAttribution` computes Markov removal effects in C++;
`tune` does hyperparameter search properly. mediamix does the step in front of
all of them.

## Installation

```r
install.packages("mediamix")

# Development version
# install.packages("pak")
pak::pak("elkronos/mediamix")
```

## A light dependency footprint

The only adstock on CRAN today lives inside Robyn, which is not itself a CRAN
package and which imports `reticulate` — a Python runtime — for what is five
lines of arithmetic.

mediamix imports `cli` for its error messages and `data.table` for the
attribution half, where event logs get large. Nothing else, and no compiled
code. `recipes`, `dials` and the rest of tidymodels are in `Suggests` and
registered conditionally, so the transforms work equally well from a Stan or
`brms` workflow that will never touch them.

```r
library(mediamix)

adstock_geometric(c(100, 0, 0, 0, 0, 0), decay = 0.5)
#> [1] 50.0000 25.0000 12.5000  6.2500  3.1250  1.5625
```

## The half-life vocabulary

Practitioners say "television works for about three weeks". They do not say
"television has a decay coefficient of 0.7937". The package translates, and
uses one vocabulary throughout:

```r
theta <- decay_from_half_life(3)     # 0.7937005
half_life(theta)                     # 3
effective_window(theta, 0.90)        # 10 periods
```

That last line is how you decide whether your data can identify a carryover
this long: a decay implying a 40-week effective window on 104 weeks of data
cannot be estimated, however confident the cross-validation looks.

## Journey construction, which nothing else does

`ChannelAttribution` has consumed `"a > b > c"` strings for eleven years and
has never shipped the step that produces them. That step usually ends up as a
`group_by()` and a `paste(collapse = " > ")`, and that loses a great deal.

On the package's 20,799-event synthetic log:

|                                   | journeys | mean length |
|-----------------------------------|---------:|------------:|
| naive one-path-per-customer       |    5,400 |        3.85 |
| split on conversion               |    6,137 |        3.39 |
| + 30-day inactivity gap           |    6,158 |        3.38 |
| + collapse consecutive duplicates |    6,158 |        2.81 |

14% more journeys, 27% shorter. Journey counts and lengths feed a Markov
transition matrix directly, so those are not rounding differences.

```r
data(mm_events)
paths <- build_paths(
  mm_events,
  id = "customer_id", channel = "channel", timestamp = "timestamp",
  conversion = "conversion", value = "value",
  lookback = 30,                       # or read it off conversion_lag()
  split_on = c("conversion", "gap"), gap = 30,
  collapse_repeats = TRUE,
  keep_null_paths = TRUE,              # Markov removal effects need these
  direct = "label"
)
```

`build_paths()` handles the seven things that go wrong between an event log
and a set of journeys — journey splitting, lookback windows, non-converting
paths, direct/none/missing channels, consecutive duplicates, deterministic
tie-breaking, and accounting for conversions that filtering leaves
unattributable. Each is an argument, not an assumption.

That last one matters more than it sounds. Dropping direct traffic can strip a
converting journey of every touchpoint it had. mediamix fixes the conversion
facts before any filtering, so those conversions are counted and reported as
`conversions_unattributable` rather than quietly shrinking the denominator.

## The spread is the finding

Any single credit rule gives you a number. Several give you the range that
number could have been:

```r
attribution_spread(attribute(paths))
#>       channel  min_share max_share mean_share    spread first_last_ratio
#> 1     display 0.07187158 0.2904050  0.1742354 0.2185334        4.0406091
#> 2 paid_search 0.09230208 0.2725283  0.1851354 0.1802262        0.3386881
#> 3       email 0.05654870 0.1831448  0.1227534 0.1265961        0.3087649
#> 4      social 0.11638088 0.2207224  0.1658973 0.1043415        1.8965517
```

Display's share runs from 7% to 29% depending only on the convention chosen.
That channel has not been measured; it has been assigned a number.

## The conceptual spine

Adstock takes one impulse of spend and spreads its effect *forward* with
geometric decay. Time-decay attribution takes one conversion and spreads its
credit *backward* with the same kernel. Same arithmetic, opposite arrow — and
in the API, the same `decay` argument:

```r
# One evenly spaced four-touch journey
j <- data.frame(
  cust = "a", channel = c("display", "social", "email", "paid_search"),
  ts = as.POSIXct("2026-01-01", tz = "UTC") + (0:3) * 86400,
  conv = c(0, 0, 0, 1)
)
one <- build_paths(j, id = "cust", channel = "channel",
                   timestamp = "ts", conversion = "conv")

adstock_weights(max_lag = 4, decay = 0.5)
#> [1] 0.53333333 0.26666667 0.13333333 0.06666667

rev(credit_time_decay(one, decay = 0.5)$credit)
#> [1] 0.53333333 0.26666667 0.13333333 0.06666667

all.equal(adstock_weights(4, 0.5), rev(credit_time_decay(one, decay = 0.5)$credit))
#> [1] TRUE
```

Not analogous. Identical — exactly, for the geometric kernel on evenly spaced
touches. See `vignette("spine")`, which also sets out where the symmetry stops.

## Carryover parameters chosen against the KPI

It is tempting to choose decay by asking which transformed series is
best-behaved. That criterion has no dependent variable in it, so it ranks
filters by how hard they smooth rather than by anything about the outcome.

`tune_carryover()` scores each candidate by how well a model you supply
predicts *held-out KPI*:

```r
tune_carryover(spend, kpi, fit_fn = my_fit, predict_fn = my_predict,
               max_lags = Inf, decays = seq(0.05, 0.95, by = 0.05),
               scheme = "rolling_origin")
```

The test suite generates data from a known decay and requires it to be
recovered — exactly, at every value tested. See `vignette("carryover")`, which
also shows how much harder this gets with five correlated channels and 156
weeks, and why that is a fact about the model rather than about the objective.

## Recipe steps that survive the train/test boundary

`recipes::step_lag()` emits `NA` at a prep/bake boundary, silently, and its
documented remedy changes the row count — which breaks `tune_grid()`.

`step_adstock()` stores *filter state* instead of rows. Geometric adstock is
an IIR filter whose entire memory is one number per series, so `prep()` keeps
one double per column per group and `bake()` warm-starts from it. Row count
and order are preserved unconditionally.

```r
recipe(revenue ~ ., data = train) |>
  step_adstock(all_of(channels), index = "date", by = "geo", decay = tune()) |>
  step_saturation(all_of(channels), half_max = tune(), shape = tune())
```

`bake()` classifies the new data against what `prep()` saw — contiguous,
overlapping, gapped, an unseen group, or an unknown period — and warm-starts
only when that is correct, warning loudly otherwise. This is what makes joint
tuning of carryover, saturation and model penalty possible in one
`tune_bayes()` call.

## Scope

| In | Out | Because |
|---|---|---|
| Adstock and saturation transforms | Model fitting | `lm`, `glmnet`, `brms` already exist |
| Carryover selection by cross-validation | Bespoke optimisers | `tune` does this properly |
| Journey construction from event logs | Markov removal effects | `ChannelAttribution` owns it, in C++ |
| Rule-based attribution credit | Causal incrementality | Requires experiments, not logs |
| Contribution and ROI decomposition | Budget optimisation | Deserves its own design pass |
| Media data diagnostics | Generic regression diagnostics | `broom`, `performance` |

`as_channel_paths()` exports journeys straight into `ChannelAttribution`'s
format. Interop, not competition.

## A caveat worth stating

Rule-based attribution is a bookkeeping convention, not a causal estimate. It
divides observed conversions among observed touchpoints by a rule you chose; it
cannot tell you what would have happened if a channel had not run, because that
outcome is not in the log. Only an experiment answers that.

## Vignettes

- `vignette("mediamix")` — getting started: raw data to contributions and ROI
- `vignette("journeys")` — building journeys from event logs
- `vignette("carryover")` — choosing decay and lag by cross-validation
- `vignette("tidymodels")` — joint tuning of transform and model parameters
- `vignette("spine")` — carryover and credit are the same idea

## References

Broadbent, S. (1979). One way TV advertisements work. *Journal of the Market
Research Society*, 21(3), 139–166.

Hill, A. V. (1910). The possible effects of the aggregation of the molecules of
haemoglobin on its dissociation curves. *The Journal of Physiology*, 40,
iv–vii.
