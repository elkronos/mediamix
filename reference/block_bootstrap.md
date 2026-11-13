# Moving block bootstrap for time-series statistics

Puts uncertainty intervals on anything computed from a weekly panel
(coefficients, contributions, average or marginal ROI) by resampling
*blocks* of consecutive rows, refitting, and recomputing. Resampling
single rows would destroy the autocorrelation that carryover creates and
make the intervals too narrow; resampling blocks keeps it within each
block (Künsch, 1989).

## Usage

``` r
block_bootstrap(
  data,
  statistic,
  times = 200L,
  block_length = NULL,
  by = NULL,
  level = 0.9,
  seed = NULL,
  panel = c("shared", "independent"),
  index = NULL,
  circular = FALSE,
  cores = 1L
)
```

## Arguments

- data:

  A data frame, rows in time order (within each group when `by` is
  supplied), unless `index` names a column to order them by. It should
  hold everything `statistic` needs: typically the KPI, the
  *transformed* media, the controls and the raw spend.

- statistic:

  A function of one data frame returning a named numeric vector. It is
  called once on `data` for the point estimate and once per replicate on
  a resampled copy. Replicates are matched to the estimate by name, so
  the order of the elements may change between calls; a replicate that
  returns a name the estimate does not have counts as failed, and a name
  it leaves out is missing for that replicate.

- times:

  Number of bootstrap replicates, at most 1,000,000.

- block_length:

  Rows per block, or periods per block when `by` is supplied with
  `panel = "shared"`. The default, `ceiling(n^(1/3))` with `n` the
  number of rows of `data`, is the usual rate-optimal order for the
  moving block bootstrap. It is often too short for media data; see
  "Choosing the block length".

- by:

  Optional character vector of grouping columns, for a panel such as one
  series per geography. How the groups are resampled is set by `panel`.

- level:

  Confidence level of the percentile intervals.

- seed:

  Optional random seed, one whole number, for reproducible intervals.
  Every replicate's rows are drawn and the point estimate is computed
  after seeding, and the caller's random number stream is restored
  afterwards. Each replicate's call to `statistic` starts from a seed of
  its own, drawn before any replicate is evaluated, so a statistic that
  draws random numbers is reproducible too.

- panel:

  How groups are resampled when `by` is supplied. `"shared"`, the
  default, draws one set of blocks of periods and applies it to every
  group, so a shock that hit every geography in the same week stays
  together in each replicate. `"independent"` draws separate blocks for
  each group. That treats the groups as unrelated series, and for a
  statistic pooled across groups it gives intervals that are too narrow
  whenever the groups share shocks.

- index:

  Optional name of a numeric, `Date` or `POSIXct` column holding each
  row's period. Rows are put in period order within each group, and with
  `panel = "shared"` they are matched across groups by period, so the
  groups may cover different periods. Without `index`, row `t` of every
  group is taken to be the same period, and `panel = "shared"` needs
  every group to have the same number of rows.

- circular:

  If `TRUE`, blocks wrap around from the end of each series to its start
  (the circular block bootstrap of Politis and Romano, 1992), so every
  row is equally likely to be drawn. The default `FALSE` is the moving
  block bootstrap, which draws the first and last `block_length - 1`
  rows of each series less often than the rest.

- cores:

  Number of cores over which the replicates are evaluated. Values above
  1 use
  [`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html) on
  Unix-alikes and a socket cluster elsewhere. Every replicate's rows,
  and the seed its call to `statistic` starts from, are drawn in this
  session before any replicate is evaluated, so the result does not
  depend on `cores`. The workers of a socket cluster are new R sessions
  that load mediamix and nothing else, so there `statistic` must call
  other packages with `pkg::fun()` and must not rely on functions or
  data defined in your session.

## Value

An object of class `mm_bootstrap`: a data frame with one row per element
of the statistic and columns `term`, `estimate` (on the original data),
`lower`, `upper` (percentile interval) and `std_error`, with the
replicate matrix attached as the attribute `"replicates"` (one row per
replicate, one column per term). Replicates in which `statistic` failed
are dropped and counted in a message that quotes the first error. Each
term's interval and standard error use the replicates in which that term
is finite; missing or infinite values stay in the replicate matrix and
are counted per term in a message.

Subsetting the rows keeps the replicate matrix in step with the terms.
Removing one of the five columns returns a plain data frame, as do
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) and, with
the broom column names `std.error`, `conf.low` and `conf.high`,
`tidy()`.

## Details

Transform the media *before* bootstrapping and put the transformed
columns in `data`. Resampled blocks are glued end to end, so re-running
the adstock filter on resampled rows would carry spend across block
boundaries that were never adjacent in time; the transformed columns
already hold each week's correct carryover.

The interval reflects sampling variation given the model specification,
including the transform parameters. It does not include the uncertainty
in choosing those parameters unless `statistic` re-tunes them, which is
possible but slow.

Every replicate's rows are drawn before `statistic` is first called, so
a statistic that sets its own seed, as a cross-validated fit often does,
cannot make the resamples repeat. Each replicate's call to `statistic`
then starts from a seed drawn for it in advance, which is what makes the
replicates the same in a worker process as in this one. Random numbers
that `statistic` draws on the replicates do not advance the caller's
stream.

## Panels

With `by` and the default `panel = "shared"`, each replicate draws one
set of blocks of periods and takes every group's rows for those periods,
the moving blocks bootstrap for panels of Gonçalves (2011). Shocks
common to every group, such as a national holiday or a competitor's
launch, then stay together, which they must for a statistic pooled
across groups. Each group's rows are passed to `statistic` together, in
time order, one group after another.

Groups are matched period by period. With `index`, a group has no row
for a period it does not cover, so group sizes vary from replicate to
replicate. Without `index`, row `t` of each group is period `t`, and
groups of different lengths are an error: supply `index`, or use
`panel = "independent"`.

With `panel = "independent"`, every group draws its own blocks and keeps
its own size. That is only right when the groups share no shocks.

## Choosing the block length

A block must be long enough to hold the dependence that matters for the
statistic. In media data that is mostly carryover, so a block at least
as long as the longest channel's
[`effective_window()`](https://elkronos.github.io/mediamix/reference/decay_vocabulary.md)
is a sensible floor. The default `ceiling(n^(1/3))` is a rate rather
than a length chosen for the data, and it is often shorter: 6 for 156
weekly rows.

Percentile intervals from a block bootstrap of a short series cover less
than their nominal level, and the shortfall grows with the
autocorrelation. For the mean of 156 values from an AR(1) process,
nominal 90% intervals from 200 replicates covered the true mean in about
81% of 2,000 simulated series when the autocorrelation was 0.5 and
blocks were 6 rows long, and in about 71% when it was 0.8, rising to
about 75% with blocks of 13 rows. Circular blocks gave much the same
figures. Read the intervals as a lower bound on the uncertainty.

The block length is checked against the series it resamples: longer than
half the shortest series gives a warning, because few distinct resamples
remain, and as long as the series is an error, because every replicate
would repeat the data.

## References

Gonçalves, S. (2011). The moving blocks bootstrap for panel linear
regression models with individual fixed effects. *Econometric Theory*,
27(5), 1048–1082.
[doi:10.1017/S0266466610000630](https://doi.org/10.1017/S0266466610000630)

Künsch, H. R. (1989). The jackknife and the bootstrap for general
stationary observations. *The Annals of Statistics*, 17(3), 1217–1241.
[doi:10.1214/aos/1176347265](https://doi.org/10.1214/aos/1176347265)

Lahiri, S. N. (2003). *Resampling Methods for Dependent Data*. Springer.
[doi:10.1007/978-1-4757-3803-2](https://doi.org/10.1007/978-1-4757-3803-2)

Politis, D. N. and Romano, J. P. (1992). A circular block-resampling
procedure for stationary data. In R. LePage and L. Billard (eds.),
*Exploring the Limits of Bootstrap*, 263–270. Wiley.

## See also

[`plot.mm_bootstrap()`](https://elkronos.github.io/mediamix/reference/plot.mm_bootstrap.md),
[`contributions()`](https://elkronos.github.io/mediamix/reference/contributions.md),
[`roi()`](https://elkronos.github.io/mediamix/reference/roi.md),
[`marginal_roi()`](https://elkronos.github.io/mediamix/reference/marginal_roi.md)

## Examples

``` r
data(mm_weekly)
north <- mm_weekly[mm_weekly$geo == "north", ]
channels <- c("tv", "video", "search", "social", "display")
truth <- attr(mm_weekly, "truth")

# Transform once, on the full series, then bootstrap the model on top.
spec <- lapply(stats::setNames(channels, channels), function(ch) list(
  adstock = list(decay = truth$decay[[ch]]),
  saturation = list(half_max = truth$half_max[[ch]],
                    shape = truth$shape[[ch]])))
media <- transform_plan(north[channels], spec)
names(media) <- paste0(channels, "_t")
d <- cbind(north, media, week = seq_len(nrow(north)))

average_roi <- function(d) {
  fit <- lm(revenue ~ tv_t + video_t + search_t + social_t + display_t +
              week + price + seasonality + holiday, data = d)
  contrib <- colSums(sweep(as.matrix(d[paste0(channels, "_t")]), 2,
                           coef(fit)[paste0(channels, "_t")], `*`))
  stats::setNames(contrib / colSums(d[channels]), channels)
}

boot <- block_bootstrap(d, average_roi, times = 50, block_length = 13,
                        seed = 1)
boot
#> 
#> ── Block bootstrap 
#> 50 replicates, blocks of 13 rows, 90% percentile intervals
#>     term estimate    lower upper std_error
#>       tv    1.310  1.10874 1.882    0.2579
#>    video    1.924  0.01862 3.495    1.1227
#>   search    2.794  1.58295 4.258    0.8991
#>   social    3.614  0.51808 5.213    1.4353
#>  display    1.517 -0.03334 5.217    1.6852

# A panel: every geography takes the same blocks of weeks.
pooled_mean <- function(d) c(revenue = mean(d$revenue))
block_bootstrap(mm_weekly, pooled_mean, times = 50, block_length = 13,
                by = "geo", index = "date", seed = 1)
#> 
#> ── Block bootstrap 
#> 50 replicates, blocks of 13 periods shared across groups, 90% percentile
#> intervals
#>     term estimate lower upper std_error
#>  revenue    26271 25485 26682     370.5
```
