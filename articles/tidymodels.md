# Tuning with tidymodels

Carryover decay, saturation shape and a model’s own penalty are not
independent. A longer carryover smooths the media series and lowers its
peaks, which changes which penalty is optimal, which changes which
saturation point best fits the residual curvature. Choosing them one at
a time (decay by cross-validation, saturation by eye, penalty by
`glmnet`’s own path) finds a corner of the parameter space and calls it
an optimum.

Tuning them jointly is what the recipe steps
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
and
[`step_saturation()`](https://elkronos.github.io/mediamix/reference/step_saturation.md)
are for. It is the entire reason these transforms are recipe steps
rather than functions you call beforehand.

The examples use `mm_weekly`, a simulated weekly panel of media spend
and revenue by geography:

``` r

library(mediamix)
library(recipes)
data(mm_weekly)

channels <- c("tv", "video", "search", "social", "display")
```

## The problem `step_adstock()` solves

Adstock is a filter with memory. Prepare a recipe on the first two and a
half years and bake it on the rest, and the first rows of the new data
have no history.

A lag built with
[`recipes::step_lag()`](https://recipes.tidymodels.org/reference/step_lag.html)
leaves `NA`, or the filler given as its `default`, in the first rows of
new data at every prep/bake boundary, and dropping those rows with
`step_naomit(skip = FALSE)` removes the boundary weeks from training and
evaluation.
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
carries the filter’s state across the boundary instead:

``` r

train <- mm_weekly[mm_weekly$date < as.Date("2025-07-01"), ]
test <- mm_weekly[mm_weekly$date >= as.Date("2025-07-01"), ]

rec <- recipe(revenue ~ ., data = train) |>
  step_adstock(all_of(channels), index = "date", by = "geo", decay = 0.7) |>
  prep()

tidy(rec, number = 1)
#> # A tibble: 15 × 13
#>    terms   group   kernel    decay max_lag shape scale type   peak start state
#>    <chr>   <chr>   <chr>     <dbl>   <dbl> <dbl> <dbl> <chr> <dbl> <chr> <dbl>
#>  1 tv      north   geometric   0.7     Inf    NA    NA NA       NA zero  7337.
#>  2 video   north   geometric   0.7     Inf    NA    NA NA       NA zero  1052.
#>  3 search  north   geometric   0.7     Inf    NA    NA NA       NA zero  1642.
#>  4 social  north   geometric   0.7     Inf    NA    NA NA       NA zero   407.
#>  5 display north   geometric   0.7     Inf    NA    NA NA       NA zero   219.
#>  6 tv      central geometric   0.7     Inf    NA    NA NA       NA zero  5755.
#>  7 video   central geometric   0.7     Inf    NA    NA NA       NA zero   832.
#>  8 search  central geometric   0.7     Inf    NA    NA NA       NA zero  1049.
#>  9 social  central geometric   0.7     Inf    NA    NA NA       NA zero   844.
#> 10 display central geometric   0.7     Inf    NA    NA NA       NA zero   516.
#> 11 tv      south   geometric   0.7     Inf    NA    NA NA       NA zero  8340.
#> 12 video   south   geometric   0.7     Inf    NA    NA NA       NA zero  1320.
#> 13 search  south   geometric   0.7     Inf    NA    NA NA       NA zero  1897.
#> 14 social  south   geometric   0.7     Inf    NA    NA NA       NA zero   949.
#> 15 display south   geometric   0.7     Inf    NA    NA NA       NA zero   973.
#> # ℹ 2 more variables: state_length <int>, id <chr>
```

The `state` column is the filter’s memory at the end of training for
each channel and geography. Geometric adstock is the recursion
$`a_t = x_t + \theta a_{t-1}`$, with $`\theta`$ the `decay`, so that
memory is one number per series: the last $`a_t`$, reported before the
`normalise` scaling by $`1 - \theta`$.
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) also
keeps each geography’s training time points and media values, so that
new data repeating the last periods of training is filtered exactly too.
For this training set they take up 23.4 kB, and they are media only,
never the outcome.

Baking new data that continues the training series warm-starts the
filter from that state:

``` r

baked <- bake(rec, new_data = test)
c(rows_in = nrow(test), rows_out = nrow(baked),
  missing = sum(is.na(baked$tv)))
#>  rows_in rows_out  missing 
#>       75       75        0
```

Every row comes back, in its original order, with no missing values at
the boundary, and the values are exactly what filtering the whole series
at once gives:

``` r

full <- mm_weekly[order(mm_weekly$geo, mm_weekly$date), ]
full$expected <- ave(full$tv, full$geo,
                     FUN = function(z) adstock_geometric(z, decay = 0.7))

check <- merge(baked[, c("date", "geo", "tv")],
               full[, c("date", "geo", "expected")],
               by = c("date", "geo"))
all.equal(check$tv, check$expected)
#> [1] TRUE
```

That equality is the step’s whole contract, and it is enforced by the
test suite.

## What happens when the data does not continue

[`bake()`](https://recipes.tidymodels.org/reference/bake.html) compares
the start of the new data with the training time points
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) stored,
and handles each case explicitly. New data that repeats training weeks
continues from the weeks before them:

``` r

# Repeating the last four training weeks, as rsample::rolling_origin(lag = 4)
# assessment sets do: the repeated rows continue from the weeks before them.
overlap <- mm_weekly[mm_weekly$date > max(train$date) - 4 * 7, ]
baked_overlap <- bake(rec, new_data = overlap)
check_overlap <- merge(baked_overlap[, c("date", "geo", "tv")],
                       full[, c("date", "geo", "expected")],
                       by = c("date", "geo"))
all.equal(check_overlap$tv, check_overlap$expected)
#> [1] TRUE

# Repeating the whole training period has nothing before it, so re-baking the
# training data reproduces the prepared output.
all.equal(bake(rec, new_data = train)$tv, juice(rec)$tv)
#> [1] TRUE
```

New data that starts after a gap cannot continue the filter, so it
cold-starts with a warning that names the gap:

``` r

# A gap: cold start, and say so.
gapped <- test[test$date >= as.Date("2025-10-01"), ]
invisible(tryCatch(bake(rec, new_data = gapped),
                   warning = function(w) message(conditionMessage(w))))
#> Cold-starting adstock for 3 series that start after a gap.
#> ℹ Carryover from before the gap is discarded.
#> ℹ Affected: "north (13 periods missing)", "central (13 periods missing)", and
#>   "south (13 periods missing)".
```

New data that starts less than one period after a training week, as when
daily rows are baked into a weekly step or the reporting day moves, is
misaligned; it cold-starts and warns the same way, as does an unseen
group. That is the default, `carry_over = "auto"`. `carry_over = "warm"`
turns those warnings into errors when you want the pipeline to insist on
contiguity; `carry_over = "cold"` disables state entirely, so each bake
filters the new data on its own, as a lag-based step does.

The step is strictly causal, unlike
[`recipes::step_window()`](https://recipes.tidymodels.org/reference/step_window.html)
and
[`recipes::step_impute_roll()`](https://recipes.tidymodels.org/reference/step_impute_roll.html),
which are centred and fill a series’ leading edge using future values.
Used on media, they would leak future spend into past adstock and
inflate in-sample fit. The step also sorts by `index` internally and
restores the original row order before returning, so nothing downstream
ever sees a reordered table.

## Grouping is an argument, not an inherited state

`recipes` steps have no built-in notion of groups, and a `grouped_df`
may not survive into
[`bake()`](https://recipes.tidymodels.org/reference/bake.html). So
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
takes the grouping columns explicitly, in `by`:

``` r

setequal(train$geo, as.character(baked$geo))
#> [1] TRUE
tidy(rec, number = 1)$group
#>  [1] "north"   "north"   "north"   "north"   "north"   "central" "central"
#>  [8] "central" "central" "central" "south"   "south"   "south"   "south"  
#> [15] "south"
```

Each geography carries its own state and its own last-seen index.
([`prep()`](https://recipes.tidymodels.org/reference/prep.html) converts
the character `geo` column to a factor along the way, so compare the
values rather than the vectors.)

## Saturation on a comparable scale

Television spend runs to thousands a week here and display to hundreds.
An absolute half-saturation point is therefore a different parameter for
every channel, which makes a single tunable parameter meaningless.

[`step_saturation()`](https://elkronos.github.io/mediamix/reference/step_saturation.md)
expresses it as a *fraction* of each column’s own reference level,
learned from the training data:

``` r

rec2 <- recipe(revenue ~ ., data = train) |>
  step_adstock(all_of(channels), index = "date", by = "geo", decay = 0.7) |>
  step_saturation(all_of(channels), half_max = 0.4, shape = 1.5) |>
  prep()

tidy(rec2, number = 2)
#> # A tibble: 5 × 6
#>   terms   type  half_max shape reference id              
#>   <chr>   <chr>    <dbl> <dbl>     <dbl> <chr>           
#> 1 tv      hill       0.4   1.5     3258. saturation_UUEdL
#> 2 video   hill       0.4   1.5      880. saturation_UUEdL
#> 3 search  hill       0.4   1.5      794. saturation_UUEdL
#> 4 social  hill       0.4   1.5      487. saturation_UUEdL
#> 5 display hill       0.4   1.5      431. saturation_UUEdL
```

`half_max = 0.4` reads the same way for every channel: half the ceiling
is reached at 40% of the largest adstocked value in that channel’s
training data (the `reference` column, under the default `ref = "max"`),
pooled over the geographies unless
[`step_saturation()`](https://elkronos.github.io/mediamix/reference/step_saturation.md)
is given `by`. One parameter now covers all five.

## The tuning parameters

Each step tells `tune` which of its arguments can be searched, and
mediamix supplies a `dials` parameter with a default range for each:
[`carryover_decay()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md)
and
[`carryover_max_lag()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md)
for
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md),
and
[`saturation_half_max()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md)
and
[`saturation_shape()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md)
for
[`step_saturation()`](https://elkronos.github.io/mediamix/reference/step_saturation.md).

``` r

carryover_decay()
#> Carryover Decay (quantitative)
#> Range: [0, 0.95]
saturation_shape()
#> Saturation Shape (quantitative)
#> Range: [0.5, 3]
```

``` r

tunable(rec2$steps[[1]])[, c("name", "source", "component")]
#>      name source    component
#> 1   decay recipe step_adstock
#> 2 max_lag recipe step_adstock
tunable(rec2$steps[[2]])[, c("name", "source", "component")]
#>       name source       component
#> 1 half_max recipe step_saturation
#> 2    shape recipe step_saturation
```

[`carryover_decay()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md)
stops at 0.95 rather than 1 on purpose. A decay of 0.99 implies a
half-life of 69 periods, which no ordinary media dataset identifies;
leaving it in the range mostly spends tuning iterations on values that
trade off against the intercept.

## Joint tuning

One search covers carryover, saturation and the model’s penalty
together, scored on rolling-origin resampling of the north geography:

``` r

north <- mm_weekly[mm_weekly$geo == "north", ]
north <- north[order(north$date), ]
```

``` r

library(rsample)
library(parsnip)
library(workflows)
library(tune)
library(dials)
#> Loading required package: scales

folds <- rolling_origin(north, initial = 104, assess = 13, skip = 12,
                        cumulative = TRUE)

tune_rec <- recipe(revenue ~ ., data = north) |>
  # `date` is the filter's index, not a predictor; `geo` is constant within
  # this subset. A recipe does not create dummies by itself, so a leftover
  # character column would reach glmnet's matrix interface and abort.
  update_role(date, new_role = "index") |>
  step_rm(geo) |>
  step_adstock(all_of(channels), index = "date", decay = tune()) |>
  step_saturation(all_of(channels), half_max = tune(), shape = tune()) |>
  step_normalize(all_numeric_predictors())

model <- linear_reg(penalty = tune(), mixture = 0) |>
  set_engine("glmnet")

wf <- workflow() |> add_recipe(tune_rec) |> add_model(model)

params <- extract_parameter_set_dials(wf)
#> Warning: Using `all_of()` outside of a selecting function was deprecated in tidyselect
#> 1.2.0.
#> ℹ See details at
#>   <https://tidyselect.r-lib.org/reference/faq-selection-context.html>
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
params
#> Collection of 4 parameters for tuning
#> 
#>  identifier     type    object
#>     penalty  penalty nparam[+]
#>       decay    decay nparam[+]
#>    half_max half_max nparam[+]
#>       shape    shape nparam[+]
#> 

# Kept deliberately small so the vignette rebuilds quickly on every check.
# A real search would use more of both.
set.seed(1)
res <- tune_bayes(wf, resamples = folds, param_info = params,
                  initial = 5, iter = 5,
                  metrics = yardstick::metric_set(yardstick::rmse))

show_best(res, metric = "rmse", n = 5)
#> # A tibble: 5 × 11
#>      penalty decay half_max shape .metric .estimator  mean     n std_err .config
#>        <dbl> <dbl>    <dbl> <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <chr>  
#> 1 0.00392    0.950    0.979  1.82 rmse    standard   1481.     4    220. iter1  
#> 2 1          0.95     0.525  1.75 rmse    standard   1536.     4    238. pre5_m…
#> 3 0.0903     0.917    1.000  2.75 rmse    standard   1591.     4    182. iter2  
#> 4 0.00000673 0.921    0.997  1.88 rmse    standard   1603.     4    203. iter4  
#> 5 0.0133     0.910    0.993  1.93 rmse    standard   1634.     4    187. iter5  
#> # ℹ 1 more variable: .iter <int>
```

[`show_best()`](https://tune.tidymodels.org/reference/show_best.html)
lists the five best candidates, each a combination of `decay`,
`half_max`, `shape` and `penalty` with its RMSE averaged over the
assessment sets. All four come out of one search, with one resampling
scheme and correct train/test boundaries throughout. Robyn does the
equivalent search with the Python library Nevergrad, so it needs a
Python runtime as well as R; `robyn_mmm()` stops with an error when
Nevergrad is not installed.

## One carryover per channel

`tune_rec` applies one `decay` to every column its
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
selects, so the search above tunes a single shared value, which keeps
the example short. Channels rarely share a carryover: in `mm_weekly` the
true decays, stored in `attr(mm_weekly, "truth")`, run from 0.15 for
search to 0.85 for television, so for a real model give each channel its
own step.
[`step_adstock_channels()`](https://elkronos.github.io/mediamix/reference/step_adstock_channels.md)
adds one
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
per channel, and with `decay = tune()` it gives each step a tuning
parameter of its own, named after the channel:

``` r

per_channel <- recipe(revenue ~ ., data = north) |>
  update_role(date, new_role = "index") |>
  step_rm(geo) |>
  # tune() is hardhat's; the tune package re-exports it.
  step_adstock_channels(channels, index = "date", decay = hardhat::tune()) |>
  step_saturation(all_of(channels), half_max = hardhat::tune(),
                  shape = hardhat::tune()) |>
  step_normalize(all_numeric_predictors())

hardhat::extract_parameter_set_dials(per_channel)
#> Collection of 7 parameters for tuning
#> 
#>     identifier     type    object
#>       decay_tv    decay nparam[+]
#>    decay_video    decay nparam[+]
#>   decay_search    decay nparam[+]
#>   decay_social    decay nparam[+]
#>  decay_display    decay nparam[+]
#>       half_max half_max nparam[+]
#>          shape    shape nparam[+]
#> 
```

The parameter set now holds five decays, `decay_tv` to `decay_display`,
beside the shared `half_max` and `shape`. Each step selects its channel
by name, fixed when the step is added; a loop of
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
calls with `all_of(ch)` would select the last channel in every step,
because selectors are evaluated at
[`prep()`](https://recipes.tidymodels.org/reference/prep.html), after
the loop has finished. The rest of the recipe is `tune_rec`’s, so
`update_recipe(wf, per_channel)` swaps it into the workflow and the
search covers five decays instead of one; give
[`tune_bayes()`](https://tune.tidymodels.org/reference/tune_bayes.html)
a larger `initial` and `iter` to match.

## Leakage, since a reviewer will ask

Under rolling-origin resampling,
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) re-runs
on each split’s analysis set. The stored state is therefore re-learned
from that split’s training rows alone and describes only media that
precedes the assessment set. Nothing carries across splits, and no
outcome information crosses any boundary: the state is a function of
past *spend*, which is genuinely known at prediction time.

The same argument covers
[`step_saturation()`](https://elkronos.github.io/mediamix/reference/step_saturation.md):
each column’s reference level is a training-set statistic, re-learned
per split, and a test-set value above it is not clipped but lands
further up the curve.

## If you skip the tuning, skip the step

The recipe steps add machinery: state, a contiguity guard, a boundary
contract to reason about. All of that earns its place because it makes
joint tuning possible. If you are going to fix decay and saturation by
hand anyway, use
[`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md)
on the vectors and keep your pipeline simpler.
