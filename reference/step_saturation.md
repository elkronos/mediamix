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
  role = NA,
  trained = FALSE,
  columns = NULL,
  refs = NULL,
  skip = FALSE,
  id = .mm_rand_id("saturation")
)
```

## Arguments

- recipe:

  A recipe object.

- ...:

  One or more selector functions choosing the media columns.

- type:

  Which curve: `"hill"` (the default), `"exponential"`,
  `"michaelis_menten"` or `"power"`.

- half_max:

  Half-saturation point, expressed as a *fraction* of the column's
  reference level, in `(0, 2]`. `0.5` means the curve reaches half its
  ceiling at half the reference spend. Ignored when `type = "power"`,
  which has no half-saturation point. Tunable, over `[0.05, 1]` by
  default.

- shape:

  Means different things to different curves, and is validated
  accordingly. For `"hill"` it is the Hill exponent, any positive
  number: `1` gives a concave curve and values above `1` an S-shape with
  a threshold. For `"power"` it is the exponent itself and must lie in
  `(0, 1]`, since a power above 1 is not saturating at all. Ignored by
  `"exponential"` and `"michaelis_menten"`. Tunable; the advertised
  range narrows to `(0.1, 1]` when `type = "power"`, so a joint search
  cannot propose a value the curve would reject.

- ref:

  How to set each column's reference level from the training data:
  `"max"` (the default), `"q99"`, `"q95"` or `"mean"`. Using a high
  quantile rather than the maximum makes the reference robust to a
  single outlying week.

- role:

  Not used by this step, since new columns are not created.

- trained:

  Has the step been prepared?

- columns, refs:

  Populated by
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html); not
  set directly.

- skip:

  Should the step be skipped when baking? Must remain `FALSE`.

- id:

  A unique step identifier.

## Value

An updated recipe with the new step added.

## Negative and missing values

Saturation curves are defined for non-negative media. Missing values are
treated as zero spend, and negative values are clamped to zero with a
warning. The commonest way to produce negatives is a
[`step_normalize()`](https://recipes.tidymodels.org/reference/step_normalize.html)
earlier in the recipe, which centres each column on its mean and
therefore sends about half of every column below zero. Saturate first,
or normalise something other than the media.

## Why the half-saturation point is relative

Television spend might run to six figures a week and affiliate to three.
An absolute half-saturation point is therefore a different parameter for
every channel, which makes joint tuning across channels either
meaningless or a separate parameter per column.

Expressing it as a fraction of each column's own reference level fixes
this: `half_max = 0.4` means "half the ceiling is reached at 40% of this
channel's peak spend" and reads the same way for every channel, so a
single tunable parameter covers them all.
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) stores
one number per column, learned from the training data only, and
[`bake()`](https://recipes.tidymodels.org/reference/bake.html) applies
it unchanged. A test-set spend above the training reference is not
clipped: it simply lands further up the curve.

Under the hood, `half_max` is converted to whichever native parameter
the chosen curve uses – the Hill and Michaelis–Menten half-points
directly, and \\\log 2 / x\_{1/2}\\ for the exponential rate – so the
fraction means the same thing across all three bounded curves.

## Order

Put this step *after*
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md).
Saturating first caps each period's spend in isolation and then lets
carryover accumulate the capped values past the ceiling. See
[`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md).

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
#>         terms type half_max shape reference               id
#> tv         tv hill      0.4   1.5 3258.2947 saturation_8MGTZ
#> video   video hill      0.4   1.5  879.8041 saturation_8MGTZ
#> search search hill      0.4   1.5  818.0759 saturation_8MGTZ
```
