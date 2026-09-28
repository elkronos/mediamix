# Tuning with tidymodels

Carryover decay, saturation shape and a model’s own penalty are not
independent. A longer carryover shifts the media series’ scale, which
changes which penalty is optimal, which changes which saturation point
best fits the residual curvature. Choosing them one at a time — decay by
cross-validation, saturation by eye, penalty by `glmnet`’s own path —
finds a corner of the parameter space and calls it an optimum.

Tuning them jointly is what the recipes layer is for. It is the entire
reason these transforms are recipe steps rather than functions you call
beforehand.

``` r

library(mediamix)
library(recipes)
data(mm_weekly)

channels <- c("tv", "video", "search", "social", "display")
```

## The problem the step solves

Adstock is a filter with memory. Prepare a recipe on the first two years
and bake it on the third, and the first rows of the third year have no
history.

[`recipes::step_lag()`](https://recipes.tidymodels.org/reference/step_lag.html)
answers this by emitting `NA`, silently. Its documented remedy,
[`step_naomit()`](https://recipes.tidymodels.org/reference/step_naomit.html),
changes the row count — and a step that changes the row count breaks
[`tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html)
outright, because the resampling machinery needs assessment predictions
to line up with assessment rows. That is not an escape hatch; it is a
dead end.

[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
stores *filter state* instead of rows:

``` r

train <- mm_weekly[mm_weekly$date < as.Date("2025-07-01"), ]
test <- mm_weekly[mm_weekly$date >= as.Date("2025-07-01"), ]

rec <- recipe(revenue ~ ., data = train) |>
  step_adstock(all_of(channels), index = "date", by = "geo", decay = 0.7) |>
  prep()

tidy(rec, number = 1)
#>      terms   group decay  state state_length            id
#> 1       tv central   0.7 5755.3            1 adstock_SwlKL
#> 2    video central   0.7  832.3            1 adstock_SwlKL
#> 3   search central   0.7 1048.7            1 adstock_SwlKL
#> 4   social central   0.7  843.6            1 adstock_SwlKL
#> 5  display central   0.7  515.5            1 adstock_SwlKL
#> 6       tv   north   0.7 7337.2            1 adstock_SwlKL
#> 7    video   north   0.7 1051.8            1 adstock_SwlKL
#> 8   search   north   0.7 1641.9            1 adstock_SwlKL
#> 9   social   north   0.7  406.9            1 adstock_SwlKL
#> 10 display   north   0.7  218.9            1 adstock_SwlKL
#> 11      tv   south   0.7 8340.0            1 adstock_SwlKL
#> 12   video   south   0.7 1320.2            1 adstock_SwlKL
#> 13  search   south   0.7 1897.4            1 adstock_SwlKL
#> 14  social   south   0.7  949.1            1 adstock_SwlKL
#> 15 display   south   0.7  972.7            1 adstock_SwlKL
```

One number per column per geography. Not lag-1 rows of data — one
double. The geometric filter is an infinite impulse response whose
entire memory is $`a_{t-1}`$, so that single number *is* the history. No
training observations are retained, which sidesteps both the object-size
objection and the privacy one.

Baking on contiguous new data warm-starts from it:

``` r

baked <- bake(rec, new_data = test)
c(rows_in = nrow(test), rows_out = nrow(baked),
  missing = sum(is.na(baked$tv)))
#>  rows_in rows_out  missing 
#>       75       75        0
```

Row count preserved, order preserved, no missing values at the boundary.
And the result is exactly what you would have got by filtering the whole
series at once:

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
the start of the new data against what
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) saw and
acts accordingly. Four cases, all explicit:

``` r

# Overlapping the training period: cold start, so re-baking the training data
# reproduces the prepared output rather than double-counting its carryover.
all.equal(bake(rec, new_data = train)$tv, juice(rec)$tv)
#> [1] TRUE
```

``` r

# A gap: cold start, and say so.
gapped <- test[test$date >= as.Date("2025-10-01"), ]
invisible(tryCatch(bake(rec, new_data = gapped),
                   warning = function(w) message(conditionMessage(w))))
#> Cold-starting adstock for 3 series with a gap after the training period.
#> ℹ Carryover from before the gap is discarded.
#> ℹ Affected: "central (13 periods missing)", "north (13 periods missing)", and
#>   "south (13 periods missing)".
```

An unseen group warns the same way. `carry_over = "warm"` turns those
warnings into errors when you want the pipeline to insist on contiguity;
`carry_over = "cold"` disables state entirely and reproduces lag-style
behaviour.

Two things worth noting. The step is strictly causal, unlike
[`recipes::step_window()`](https://recipes.tidymodels.org/reference/step_window.html)
and
[`recipes::step_impute_roll()`](https://recipes.tidymodels.org/reference/step_impute_roll.html),
which are centred and fill a series’ leading edge using future values —
applied to a media filter that leaks future spend into past adstock and
inflates in-sample fit, which is the exact pathology marketing mix
models are accused of. And the step sorts by `index` internally and
restores the original row order before returning, so nothing downstream
ever sees a reordered table.

## Grouping is an argument, not an inherited state

`recipes` steps have no built-in notion of groups, and a `grouped_df`
may not survive into
[`bake()`](https://recipes.tidymodels.org/reference/bake.html). So
grouping is named explicitly:

``` r

setequal(train$geo, as.character(baked$geo))
#> [1] TRUE
tidy(rec, number = 1)$group
#>  [1] "central" "central" "central" "central" "central" "north"   "north"  
#>  [8] "north"   "north"   "north"   "south"   "south"   "south"   "south"  
#> [15] "south"
```

Each geography carries its own state and its own last-seen index.
([`prep()`](https://recipes.tidymodels.org/reference/prep.html) converts
the character column to a factor along the way, so compare the values
rather than the vectors.)

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
#>           terms type half_max shape reference               id
#> tv           tv hill      0.4   1.5    3258.3 saturation_UUEdL
#> video     video hill      0.4   1.5     879.8 saturation_UUEdL
#> search   search hill      0.4   1.5     793.8 saturation_UUEdL
#> social   social hill      0.4   1.5     487.1 saturation_UUEdL
#> display display hill      0.4   1.5     431.5 saturation_UUEdL
```

`half_max = 0.4` reads the same way for every channel: half the ceiling
is reached at 40% of that channel’s peak spend. One parameter now covers
all five.

## One carryover per channel

A step applies one `decay` to every column it selects, and the joint
search below tunes that single shared value for brevity. Channels rarely
share a carryover — in `mm_weekly` the truth runs from 0.15 for search
to 0.85 for television — so for a real model give each channel its own
step. [`tune()`](https://hardhat.tidymodels.org/reference/tune.html)
(from `hardhat`, re-exported by `tune`) takes an identifier, which keeps
the parameters apart in the search:

``` r

per_channel <- recipe(revenue ~ ., data = train)
for (ch in channels) {
  per_channel <- step_adstock(per_channel, all_of(ch), index = "date",
                              by = "geo",
                              decay = hardhat::tune(paste0("decay_", ch)))
}
per_channel <- step_saturation(per_channel, all_of(channels),
                               half_max = hardhat::tune(),
                               shape = hardhat::tune())
vapply(per_channel$steps[1:5], function(s) as.character(s$decay[[2]]),
       character(1))
#> [1] "decay_tv"      "decay_video"   "decay_search"  "decay_social" 
#> [5] "decay_display"
```

Swap `per_channel` for `tune_rec` in the workflow below and the search
covers five decays instead of one; give it a larger `initial` and `iter`
to match.

## The tuning parameters

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

Now the payoff. One search over carryover, saturation and penalty
together, scored on rolling-origin resampling.

``` r

library(rsample)
library(parsnip)
library(workflows)
library(tune)
library(dials)
#> Loading required package: scales

north <- mm_weekly[mm_weekly$geo == "north", ]
north <- north[order(north$date), ]

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

Four parameters, one search, one resampling scheme, correct train/test
boundaries throughout. Robyn reaches for `nevergrad` — and therefore a
Python runtime — to do the equivalent.

## Leakage, since a reviewer will ask

Under rolling-origin resampling,
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) re-runs
on each split’s analysis set. The stored state is therefore re-learned
from that split’s training rows alone and describes only media that
precedes the assessment set. Nothing carries across splits, and no
outcome information crosses any boundary — the state is a function of
past *spend*, which is genuinely known at prediction time.

The same argument covers
[`step_saturation()`](https://elkronos.github.io/mediamix/reference/step_saturation.md):
each column’s reference level is a training-set statistic, re-learned
per split, and a test-set spend above it is not clipped — it simply
lands further up the curve.

## If you skip the tuning, skip the step

The recipe steps add machinery: state, a contiguity guard, a boundary
contract to reason about. All of that earns its place because it makes
joint tuning possible. If you are going to fix decay and saturation by
hand anyway, use
[`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md)
on the vectors and keep your pipeline simpler.
