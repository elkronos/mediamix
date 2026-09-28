# Adstock transformation as a recipe step

Applies geometric adstock to one or more columns inside a recipes
pipeline, carrying the filter's state across the train/test boundary
instead of emitting missing values there.

## Usage

``` r
step_adstock(
  recipe,
  ...,
  index = NULL,
  by = NULL,
  decay = 0.5,
  max_lag = Inf,
  normalise = TRUE,
  carry_over = c("auto", "warm", "cold"),
  na_action = c("zero", "error"),
  tol = 0.25,
  role = NA,
  trained = FALSE,
  columns = NULL,
  states = NULL,
  last_index = NULL,
  period = NULL,
  skip = FALSE,
  id = .mm_rand_id("adstock")
)
```

## Arguments

- recipe:

  A recipe object.

- ...:

  One or more selector functions choosing the media columns.

- index:

  Name of the time column, as a string. Required: adstock is a
  time-ordered filter, so the step needs to know the order and the
  spacing. The column is used for ordering and validation and is not
  itself transformed.

- by:

  Optional character vector naming grouping columns. Adstock is applied
  independently within each group, which is what geo-level and panel
  models need. This is an explicit argument because recipes has no group
  awareness and a grouped data frame does not reliably survive into
  [`bake()`](https://recipes.tidymodels.org/reference/bake.html).

- decay:

  Geometric decay coefficient in `[0, 1]`. Tunable.

- max_lag:

  Kernel length; `Inf` (the default) uses the infinite recursive form.
  Tunable.

- normalise:

  Should the kernel sum to 1? See
  [`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md).

- carry_over:

  How to treat state at the start of new data. `"auto"` (the default)
  warm-starts when the new data continues the training series and
  cold-starts otherwise, warning when it does. `"warm"` errors rather
  than silently cold-starting on a gap or an unseen group; data that
  overlaps the training period still cold-starts, since re-baking data
  the step has already seen should reproduce the prepared output.
  `"cold"` always restarts from zero, which reproduces the behaviour of
  a lag-based step.

- na_action:

  What to do about missing media values. `"zero"` (the default) treats
  missing media as no media, which is usually right for spend and is
  what lets a recipe run over a real, patchy panel without stopping. It
  is still a substantive assumption – an `NA` becomes a real number in
  the output – so `"error"` is available when you would rather find out.
  Note this differs from
  [`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md),
  whose default is `"error"`; a step has to survive resampling, and a
  bare function call does not.

- tol:

  Relative tolerance on the step between periods when deciding whether
  two blocks are contiguous. The default of `0.25` is loose so that
  calendar months, which vary from 28 to 31 days, are not mistaken for
  gaps.

- role:

  Not used by this step, since new columns are not created.

- trained:

  Has the step been prepared?

- columns, states, last_index, period:

  Populated by
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html); not
  set directly.

- skip:

  Should the step be skipped when baking? Must remain `FALSE`: adstock
  is a predictor transform and has to run at prediction time.

- id:

  A unique step identifier.

## Value

An updated recipe with the new step added.

## The problem this solves

Adstock is a filter with memory. Prepare a recipe on January to June and
bake it on July to December, and the first rows of July have no history
to draw on.
[`recipes::step_lag()`](https://recipes.tidymodels.org/reference/step_lag.html)
answers this by emitting `NA`, silently; its documented remedy,
[`step_naomit()`](https://recipes.tidymodels.org/reference/step_naomit.html),
changes the row count, which breaks `fit_resamples()` outright. That is
not an escape hatch, it is a dead end.

The way out is to store *filter state* rather than rows. Geometric
adstock is an infinite impulse response filter whose entire memory is
one number per series, \$\$a_t = x_t + \theta a\_{t-1},\$\$ so
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) stores
one double per column per group. That is \\O(1)\\ state. It retains no
training observations, so it adds nothing meaningful to the size of a
fitted workflow and carries no raw data with it. Only the truncated
forms, where `max_lag` is finite, need an actual tail of `max_lag - 1`
values.

## What bake() does at the boundary

[`prep()`](https://recipes.tidymodels.org/reference/prep.html) records,
per group, the terminal filter state, the last time index seen, and the
period inferred from the median spacing of the index.
[`bake()`](https://recipes.tidymodels.org/reference/bake.html) then
compares the start of the new data against that record:

- Exactly contiguous:

  Warm-start from the stored state. The result is identical to filtering
  the whole series at once, with no prepended rows.

- Overlaps the training data:

  Cold-start. This is what catches `bake(rec, new_data = training)`,
  which would otherwise double-count the training period's own
  carryover.

- A gap:

  Cold-start, and warn, naming the gap size and the group.

- An unseen group:

  Cold-start, and warn.

Row count and row order are preserved in every case. The step sorts by
`index` internally and restores the original order before returning, so
nothing downstream sees a reordered table.

## Resampling and leakage

A reviewer will ask whether stored state leaks across a resampling
boundary. It does not. Under any rsample scheme,
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) re-runs
on each split's analysis set, so the state is re-learned from that
split's training rows alone and describes only media that precedes the
assessment set. The information that crosses the boundary is past *media
spend*, which is genuinely known at prediction time; no outcome
information crosses at all.

Note also that this step is strictly causal, unlike
[`recipes::step_window()`](https://recipes.tidymodels.org/reference/step_window.html)
and
[`recipes::step_impute_roll()`](https://recipes.tidymodels.org/reference/step_impute_roll.html),
which are centred and fill the leading edge of a series using future
values. Applied to a media filter, that would leak future spend into
past adstock and inflate in-sample fit, which is the exact pathology
marketing mix models are already accused of.

## See also

[`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md),
[`step_saturation()`](https://elkronos.github.io/mediamix/reference/step_saturation.md),
[`carryover_decay()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md)

## Examples

``` r
library(recipes)
#> Loading required package: dplyr
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
#> 
#> Attaching package: ‘recipes’
#> The following object is masked from ‘package:stats’:
#> 
#>     step
data(mm_weekly)

train <- mm_weekly[mm_weekly$date < as.Date("2025-01-01"), ]
test <- mm_weekly[mm_weekly$date >= as.Date("2025-01-01"), ]

rec <- recipe(revenue ~ ., data = train) |>
  step_adstock(tv, video, search, index = "date", by = "geo", decay = 0.7) |>
  prep()

# Row count and order are preserved; no NA at the boundary
baked <- bake(rec, new_data = test)
nrow(baked) == nrow(test)
#> [1] TRUE
sum(is.na(baked$tv))
#> [1] 0

# The stored state is one number per column per geo
tidy(rec, number = 1)
#>    terms   group decay     state state_length            id
#> 1     tv central   0.7 1731.6052            1 adstock_H5dyz
#> 2  video central   0.7  758.2532            1 adstock_H5dyz
#> 3 search central   0.7 1094.0381            1 adstock_H5dyz
#> 4     tv   north   0.7 1709.3561            1 adstock_H5dyz
#> 5  video   north   0.7 1173.6509            1 adstock_H5dyz
#> 6 search   north   0.7 1504.2630            1 adstock_H5dyz
#> 7     tv   south   0.7 5923.2555            1 adstock_H5dyz
#> 8  video   south   0.7 1331.0873            1 adstock_H5dyz
#> 9 search   south   0.7 2185.7450            1 adstock_H5dyz
```
