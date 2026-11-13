# Saturation transformation as a recipe step

Applies a diminishing-returns curve to media columns inside a recipes
pipeline, with the half-saturation point expressed relative to each
column's own scale so that one tunable parameter works across channels.

## Usage

``` r
step_saturation(
  recipe,
  ...,
  type = c("hill", "exponential", "michaelis_menten", "power"),
  half_max = 0.5,
  shape = 1,
  ref = c("max", "q99", "q95", "mean"),
  by = NULL,
  role = NA,
  trained = FALSE,
  columns = NULL,
  refs = NULL,
  group_refs = NULL,
  skip = FALSE,
  id = .mm_rand_id("saturation")
)
```

## Arguments

- recipe:

  A recipe object.

- ...:

  One or more selector functions choosing the media columns. As with
  [`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md),
  select media only;
  [`all_predictors()`](https://recipes.tidymodels.org/reference/has_role.html)
  also picks up the controls.

- type:

  Which curve: `"hill"` (the default), `"exponential"`,
  `"michaelis_menten"` or `"power"`.

- half_max:

  Half-saturation point, expressed as a *fraction* of the column's
  reference level, in `(0, 2]`. `0.5` means the curve reaches half its
  ceiling at half the reference level. Ignored when `type = "power"`,
  which has no half-saturation point, and it cannot be tuned there.
  Otherwise tunable, over `[0.05, 1]` by default.

- shape:

  Means different things to different curves, and is validated
  accordingly. For `"hill"` it is the Hill exponent, any positive
  number: `1` gives a concave curve and values above `1` an S-shape with
  a threshold. For `"power"` it is the exponent itself and must lie in
  `(0, 1]`, since a power above 1 is not saturating at all. Ignored by
  `"exponential"` and `"michaelis_menten"`, and it cannot be tuned
  there. Tunable for `"hill"` and `"power"`; the advertised range
  narrows to `[0.1, 1]` when `type = "power"`, so a joint search cannot
  propose a value the curve would reject.

- ref:

  How to set each column's reference level from the training data:
  `"max"` (the default), `"q99"`, `"q95"` or `"mean"`. Using a high
  quantile rather than the maximum keeps a single outlying week from
  setting the reference, but a channel that ran in only a few training
  periods can have a 95th percentile of zero.

- by:

  Optional character vector naming grouping columns. When given, each
  group gets its own reference level, so a small geography is placed on
  the curve by its own scale rather than by the largest one's. `NULL`
  (the default) pools all rows.

- role:

  Not used by this step, since new columns are not created.

- trained:

  Has the step been prepared?

- columns, refs, group_refs:

  Populated by
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html); not
  set directly.

- skip:

  Should the step be skipped when baking? Must remain `FALSE`.

- id:

  A unique step identifier.

## Value

An updated recipe with the new step added.

## Why the half-saturation point is relative

Television spend might run to six figures a week and affiliate to three.
An absolute half-saturation point is therefore a different parameter for
every channel, which makes joint tuning across channels either
meaningless or a separate parameter per column.

Expressing it as a fraction of each column's own reference level fixes
this: `half_max = 0.4` means "half the ceiling is reached at 40% of this
column's reference level" and reads the same way for every channel, so a
single tunable parameter covers them all. With the default `ref = "max"`
the reference is the column's training maximum. After
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
the column holds adstocked values, so that is the peak adstocked value,
not the peak weekly spend.
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) stores
one number per column, or per column and group with `by`, learned from
the training data only, and
[`bake()`](https://recipes.tidymodels.org/reference/bake.html) applies
it unchanged. A test-set value above the training reference is not
clipped: it lands further up the curve.

Under the hood, `half_max` is converted to whichever native parameter
the chosen curve uses (the Hill and Michaelis–Menten half-points
directly, and \\\log 2 / x\_{1/2}\\ for the exponential rate), so the
fraction means the same thing across all three bounded curves.

## Panel data

Without `by`, the reference level is computed over every row, so on a
panel it is set by the largest geography: a small one sits near the
bottom of the curve and never saturates. With `by`, each group's
reference comes from its own training rows. A group not seen during
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) uses the
pooled reference, the one computed over every training row, with a
warning. So does a group whose own training values give no positive
reference, such as one where a channel never ran;
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) warns
about those.

## Order

Put this step after
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md),
so that the curve acts on accumulated exposure rather than on each
period's spend alone. With the default normalised kernel the result
stays below the ceiling in either order; with `normalise = FALSE`,
saturating first would also let carryover accumulate the capped values
past the ceiling. See
[`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md).

When
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
keeps the raw spend (`keep_original_cols = TRUE`), the adstocked values
are in its new columns, so select those here, for example with
`starts_with("adstock_")`.

## Negative and missing values

Saturation curves are defined for non-negative media. Missing values are
treated as zero spend, and negative values are clamped to zero with a
warning. The commonest way to produce negatives is a
[`step_normalize()`](https://recipes.tidymodels.org/reference/step_normalize.html)
earlier in the recipe, which centres each column on its mean and
therefore sends about half of every column below zero. Saturate first,
or normalise something other than the media. Infinite values are an
error.

## Tidying

[`tidy()`](https://generics.r-lib.org/reference/tidy.html) returns a
tibble with one row per selected column, or per column and group with
`by`, and the columns `terms`, `group` (only with `by`), `type`,
`half_max`, `shape`, `reference` and `id`. `half_max` and `shape` are
`NA` where the curve ignores them, and before
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) when they
are marked with `tune()`. `reference` is the learned reference level,
`NA` before
[`prep()`](https://recipes.tidymodels.org/reference/prep.html).

## See also

[`saturate()`](https://elkronos.github.io/mediamix/reference/saturation.md),
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md),
[`saturation_half_max()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md)

## Examples

``` r
library(recipes)
data(mm_weekly)

rec <- recipe(revenue ~ ., data = mm_weekly) |>
  step_adstock(tv, video, search, index = "date", by = "geo", decay = 0.7) |>
  step_saturation(tv, video, search, half_max = 0.4, shape = 1.5) |>
  prep()

out <- bake(rec, new_data = NULL)
range(out$tv)
#> [1] 0.0000000 0.7980959
tidy(rec, number = 2)
#> # A tibble: 3 × 6
#>   terms  type  half_max shape reference id              
#>   <chr>  <chr>    <dbl> <dbl>     <dbl> <chr>           
#> 1 tv     hill       0.4   1.5     3258. saturation_nPUaO
#> 2 video  hill       0.4   1.5      880. saturation_nPUaO
#> 3 search hill       0.4   1.5      818. saturation_nPUaO

# One reference level per geography
rec_geo <- recipe(revenue ~ ., data = mm_weekly) |>
  step_adstock(tv, index = "date", by = "geo", decay = 0.7) |>
  step_saturation(tv, half_max = 0.4, shape = 1.5, by = "geo") |>
  prep()
tidy(rec_geo, number = 2)
#> # A tibble: 3 × 7
#>   terms group   type  half_max shape reference id              
#>   <chr> <chr>   <chr>    <dbl> <dbl>     <dbl> <chr>           
#> 1 tv    north   hill       0.4   1.5     2455. saturation_Q4dc6
#> 2 tv    central hill       0.4   1.5     1732. saturation_Q4dc6
#> 3 tv    south   hill       0.4   1.5     3258. saturation_Q4dc6
```
