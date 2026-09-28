# mediamix: adversarial review

Scope: all R sources, tests, vignettes, README, DESCRIPTION, data-generating
scripts and CI, reviewed against four questions: does it make sense, is it
accurate, is it easy to use, and is it useful beyond existing tools?

Method: every finding below was either reproduced by running code or checked
against a primary source. The package was built and checked under R 4.3.3
(`R CMD check`, 835 existing tests passing before any change). `tune`,
`parsnip`, `workflows`, `yardstick` and `ChannelAttribution` could not be
installed in the review environment, so code gated on them was read, not run.
CI installs them.

## Verdict

The engineering is careful: argument validation is thorough, edge cases are
tested, and the stateful recipe step is a real contribution. The problems
were in **methodology and claims**: one wrong recommendation that reversed a
budget conclusion, a metric that was not what it said, a subsetting bug that
silently broke credit rules, a tutorial dataset in which every channel lost
money, and README claims about other packages that did not hold up. All of
these are fixed on this branch.

## Findings, most severe first

### 1. Marginal ROI understated slow channels ~6x and reversed a recommendation (fixed)

`?roi` and the getting-started vignette said to turn `mroi()` into a return
on spend by multiplying by the kernel's first weight, `1 - decay`. That counts
only the week of spend. Under a normalised kernel the weights sum to one, so
most of a slow channel's effect arrives later and was discarded. For
`mm_weekly` television (decay 0.85) the recipe gave 0.21 against a
counterfactual marginal return of 1.25, and the vignette concluded from it
that "reallocating on the marginal column would not" fund television.

Fix: new `marginal_roi()`, which re-runs carryover and saturation on a 1%
larger budget and divides the extra response by the extra spend, following
Jin et al. (2017) and Meridian. Docs and vignette rewritten; the vignette now
shows the shortcut and the simulation side by side.

### 2. `tune_carryover()`'s "rmse" was a mean absolute error (fixed)

Per-split metrics were averaged, and the default `assess = 1` makes each split
one point, so each per-split RMSE was an absolute error. Verified: reported
5.765 = MAE; true pooled RMSE 7.313.

Fix: `aggregate = "pooled"` (default) evaluates the metric once over all
out-of-sample forecasts; `"mean"` keeps the tidymodels convention, with the
caveat documented.

### 3. Row subsets of `mm_paths` left stale ranks, breaking credit rules (fixed)

`paths[paths$channel != "display", ]` kept the original `touch_rank` and
`touch_n`. `credit_first()` then gave 720 of 2,662 converting journeys no
credit (their first touch was gone), and position-based credit misassigned
middles. `[.mm_paths` now recomputes ranks and lengths when rows change.

### 4. The tutorial dataset had every channel losing money (fixed)

`mm_weekly` average ROIs were 0.07 to 0.18: every channel returned pennies
per pound, which makes every ROI discussion in the docs nonsensical for a
practitioner. Spend and `half_max` are rescaled by 1/20 (revenue, decays,
shapes and seeds unchanged), giving average ROIs of 1.3 to 3.6 and one channel
(display) whose *marginal* return is below 1, a realistic teaching case.

### 5. The textbook one-SE rule is useless on time-series splits (addressed)

Added `best_1se` to `tune_carryover()`. The unpaired rule selected decay 0.05
for all five `mm_weekly` channels, because the standard error of each
candidate's mean is dominated by how hard each week is to forecast, which is
common to all candidates. Implemented the *paired* form (standard error of the
per-split difference from the best candidate), which recovers informative
choices (TV 0.60 vs. truth 0.85) and is tested to stay near a clearly
identified truth.

### 6. README claims that did not hold up (fixed)

- "The only adstock on CRAN today lives inside Robyn, which is not itself a
  CRAN package": Robyn has a CRAN listing, and the sentence contradicts
  itself. Rewritten neutrally.
- "Journey construction, which nothing else does" / "No other package offers
  this": unverifiable superlatives, removed.
- `install.packages("mediamix")`: the package is not on CRAN. Replaced with the
  GitHub install.
- "Robyn and most of the MMM literature use the unnormalised form": Jin et al.
  (2017) normalise. Corrected here and in `?adstock_geometric`.
- "Recovered exactly, at every value tested": the test allows one grid step.
  Reworded.

### 7. Spine vignette equated an MMM half-life with an attribution lookback (fixed)

It claimed a weekly TV half-life "also specified a defensible time-decay
attribution window". They measure different things (aggregate demand response
vs. a convention over one customer's clicks) on different time scales. The
passage now says the vocabulary is shared, the values are not, and points to
`conversion_lag()`.

### 8. `diagnose_media()` CPM check ignored `by` (fixed)

The median and MAD were pooled across geographies, so a region that buys
media at a different price was flagged as a run of "join errors"; the
`period` column was actually a row number. Now computed within series, column
renamed to `row`, `group` added.

### 9. Multiple conversions in one journey were counted once, silently (fixed)

Under `split_on = "none"` or `"gap"`, 2,741 conversion events became 2,036
credited conversions without comment. `build_paths()` now reports it and
suggests splitting on conversion.

### 10. Smaller documentation issues (fixed)

- Weibull `"cdf"` docs called each factor "the probability of surviving
  another period"; it is the unconditional survival value, and the kernel is
  its running product. Reworded, and the relationship to Robyn's parameters
  stated.
- `?contributions` repeated a paragraph verbatim.
- tidymodels vignette: "five figures a week" (stale after rescale), and
  unverifiable asides about a closed feature request and `modeltime`
  precedent, removed. `recipes` does not re-export `tune()`; the new
  per-channel example uses `hardhat::tune()`.
- Several numbers in prose that no longer matched output are now inline code.

## Usefulness: what was added

| Addition | Why | Source |
|---|---|---|
| `marginal_roi()` | Counterfactual marginal ROI, with planning window and carryover run-out | Jin et al. (2017); Meridian |
| `markov_removal()`, `"markov"` rule in `attribute()` | Data-driven attribution next to the heuristics, exact via the fundamental matrix, no C++ dependency | Anderl et al. (2016) |
| `adstock_delayed()` | Delayed-peak kernel with the peak read in periods | Jin et al. (2017) |
| `best_1se`, `std_err` in `tune_carryover()` | Honest reporting of weakly identified carryover | Breiman et al. (1984); Hastie et al. (2009) |
| Per-channel carryover pattern in the tidymodels vignette | One step applies one decay; channels rarely share one | — |
| pkgdown site + GitHub Pages workflow | Browsable reference and articles | — |
| Walkthrough article | Question to recommendation, including sensitivity and extrapolation checks | — |
| Methods article | Every function mapped to its method and reference | — |
| Comparison article | When to use mediamix vs. Robyn, Meridian, PyMC-Marketing, ChannelAttribution | — |

## Follow-up round: open items implemented

| Item | Status |
|---|---|
| `tune_carryover()` was single-channel | **Done.** `tune_carryover_joint()` tunes every channel inside one model with controls by coordinate descent and plots each channel's profile. On `mm_weekly` it lands within 0.1 of the truth for three channels and 0.2 for a fourth (one-at-a-time missed by up to 0.45), and shows search's carryover is unidentified. `tune_carryover()` also gained `controls`. |
| No uncertainty on contributions or ROI | **Done.** `block_bootstrap()` (moving block bootstrap, Kunsch 1989). The walkthrough now shows that the display-to-social ranking holds in 72% of replicates: evidence for a test, not a rollout. |
| VIFs on raw spend | **Done.** `diagnose_media(decay =)` measures collinearity on adstocked media; on `mm_weekly` the TV–video correlation doubles (0.16 to 0.32). |
| Cold-start bias | **Done.** `adstock_steady_state()`, and `warm_start` in both tuners. |
| ChannelAttribution cross-check | **Added as a test** (`test-interop.R`), skipped where ChannelAttribution is absent, so it runs in CI. |
| `cran-comments.md` environments | **Not changed.** Only the author can confirm which external builders were run. |
| Enable GitHub Pages | **Needs the repository owner:** Settings → Pages → deploy from `gh-pages`, after the pkgdown workflow runs on `main`. |

Also added in this round:

- **Visuals:** `plot()` methods for every main result, with a fixed,
  colour-vision-checked palette exported as `mm_palette()`. Figures were
  added to every vignette, the walkthrough and the README.
- **Bug found by the new plot tests:** graphical parameters were not
  restored after a plot.
- **Performance:** `assisted_conversions()` is about 16× faster and
  `top_paths()` about 2× faster.
- **`inst/CITATION`.**

## References

Anderl, E., Becker, I., von Wangenheim, F. and Schumann, J. H. (2016).
*International Journal of Research in Marketing*, 33(3), 457–474.
Breiman, L. et al. (1984). *Classification and Regression Trees.*
Kunsch, H. R. (1989). *The Annals of Statistics*, 17(3), 1217–1241.
Hastie, T., Tibshirani, R. and Friedman, J. (2009). *The Elements of
Statistical Learning*, 2nd ed.
Jin, Y. et al. (2017). *Bayesian methods for media mix modeling with carryover
and shape effects.* Google Inc.
