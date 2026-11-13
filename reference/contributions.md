# Decompose predicted KPI into channel contributions

Splits a fitted model's prediction into a per-channel contribution plus
a baseline. A marketing mix model is usually delivered as this
decomposition, the answer to "how much of last year's revenue did each
channel produce, and what would have happened anyway", rather than as
its coefficients.

## Usage

``` r
contributions(
  media,
  model,
  intercept = NULL,
  index = NULL,
  by = NULL,
  newdata = NULL
)
```

## Arguments

- media:

  A data frame of the *transformed* media regressors (adstocked and
  saturated), exactly as they entered the model. Column names must match
  the model's coefficient names. Backticks in a coefficient name are
  ignored, so a column named `TV Spend` matches the coefficient
  `` `TV Spend` ``. Numeric columns that match no coefficient are left
  out, with a message.

- model:

  A fitted model, or a named numeric vector of coefficients. Any model
  for which [`stats::coef()`](https://rdrr.io/r/stats/coef.html) returns
  a named numeric vector will do, such as an
  [`lm()`](https://rdrr.io/r/stats/lm.html) or
  [`glm()`](https://rdrr.io/r/stats/glm.html) fit. For glmnet, brms or
  Stan, pass the point estimates as a named vector. For a mixed model
  from nlme or lme4, the fixed effects are used, and group-level
  deviations from them are part of the baseline.

- intercept:

  Model intercept, used when the baseline is the intercept: for a bare
  coefficient vector, or for a fitted model that cannot predict the
  periods in `media`. Taken from `model` when it has one; supply it
  explicitly when passing a bare coefficient vector without an
  `"(Intercept)"` element.

- index:

  Optional vector, the same length as `nrow(media)`, labelling the
  periods. Used as the `period` column of the result; defaults to the
  row numbers.

- by:

  Optional grouping vector, the same length as `nrow(media)`, or a data
  frame of grouping columns with one row per row of `media`. With a data
  frame, `group` joins each row's values with `" / "`.

- newdata:

  Optional data frame holding every variable `model` needs, one row per
  row of `media`, for periods the model was not fitted on: a holdout, or
  a reporting period. The baseline is then `predict(model, newdata)`
  minus the media effect. Columns that appear in both `media` and
  `newdata` must hold the same values.

## Value

A data frame in long format with columns `period`, `group` (present only
when `by` is supplied), `channel` and `contribution`. The baseline
appears as a channel named `"(baseline)"`. The result has class
`mm_contributions`, with a
[plot()](https://elkronos.github.io/mediamix/reference/plot.mm_contributions.md)
method.

The baseline is the model's prediction minus the media effect, so the
contributions in each period sum to that prediction. Which prediction is
used depends on the rows of `media`:

- When `media` holds the rows the model was fitted on, it is the model's
  fitted value. The rows are recognised by comparing the media columns
  with the model frame. A row the model dropped for a missing value has
  a `NA` baseline, and a message says how many there are.

- For any other rows, it is `predict(model, newdata)`. Without
  `newdata`, `media` itself is used when it holds every variable the
  model needs; otherwise the baseline is the intercept alone, and a
  message says so. The fitted values of the training rows are never used
  for other rows.

- For a bare coefficient vector there is nothing to predict with, so the
  baseline is the intercept, and contributions sum to the media effect
  plus that intercept. The function says so when the model has terms
  that are not among the media columns.

## Details

Contributions are computed as \\\beta_j x\_{jt}\\ for each channel, with
everything else (intercept, trend, price, seasonality, controls) pooled
into `"(baseline)"`. That pooling is a deliberate simplification: it is
what makes contributions sum exactly to the fitted value, and it means
the baseline is not "organic demand" but "everything this decomposition
does not attribute to media". Reporting it as organic demand makes a
marketing mix deliverable look more precise than it is.

Contributions inherit whatever the model's identification is worth. If
two channels are collinear the split between them is arbitrary even when
the total is well estimated, and if the model dropped a coefficient
outright this function refuses rather than reporting `NA` contributions
that would quietly propagate into an ROI table. Run
[`diagnose_media()`](https://elkronos.github.io/mediamix/reference/diagnose_media.md)
before presenting any of this.

On panel data, pass one geography at a time or supply `by`. `index` is
used as a label, not a key: with `by` supplied and one date per
geography, a [`tapply()`](https://rdrr.io/r/base/tapply.html) over
`period` alone silently adds the geographies together.

## Models with a link function

For a generalised linear model whose link is not the identity, such as a
Poisson or log-link model, \\\beta_j x\_{jt}\\ is on the scale of the
linear predictor, not of the KPI. Contributions are then computed on
that scale throughout: the baseline is the linear predictor minus the
media effect, each period sums to the linear predictor rather than to
the fitted KPI, and a warning says so. Under a log link,
[`exp()`](https://rdrr.io/r/base/Log.html) of a channel's contribution
is the factor by which the channel multiplies the predicted KPI. To
express a channel's effect in KPI units, compare the model's predictions
with predictions from data in which that channel is set to zero.

## See also

[`roi()`](https://elkronos.github.io/mediamix/reference/roi.md),
[`response_curve()`](https://elkronos.github.io/mediamix/reference/response_curve.md),
[`diagnose_media()`](https://elkronos.github.io/mediamix/reference/diagnose_media.md),
[`plot.mm_contributions()`](https://elkronos.github.io/mediamix/reference/plot.mm_contributions.md)

## Examples

``` r
data(mm_weekly)
north <- mm_weekly[mm_weekly$geo == "north", ]
channels <- c("tv", "video", "search", "social", "display")

# Transform with the parameters the data was generated from, so the example
# shows a correctly specified decomposition rather than a misspecified one.
truth <- attr(mm_weekly, "truth")
spec <- lapply(stats::setNames(channels, channels), function(ch) list(
  adstock = list(decay = truth$decay[[ch]]),
  saturation = list(half_max = truth$half_max[[ch]],
                    shape = truth$shape[[ch]])
))
transformed <- transform_plan(north[channels], spec)

# Include the controls the data was simulated with. Without them, the media
# coefficients absorb price, trend and seasonality and two of them flip sign.
model_data <- transformed
model_data$week <- seq_len(nrow(model_data))
model_data$price <- north$price
model_data$seasonality <- north$seasonality
model_data$holiday <- north$holiday

fit <- stats::lm(north$revenue ~ ., data = model_data)
contrib <- contributions(transformed, fit, index = north$date)
head(contrib)
#>       period channel contribution
#> 1 2023-01-02      tv       0.0000
#> 2 2023-01-09      tv       0.0000
#> 3 2023-01-16      tv     265.1047
#> 4 2023-01-23      tv     206.4877
#> 5 2023-01-30      tv     160.4697
#> 6 2023-02-06      tv     471.9819

# Contributions sum exactly to the model's fitted value in every period
totals <- as.numeric(tapply(contrib$contribution, contrib$period, sum))
all.equal(totals, unname(stats::fitted(fit)))
#> [1] TRUE

# Channel totals over the three years. "(baseline)" is everything the model
# predicts that is not attributed to these five columns: trend, price,
# seasonality, holidays and the intercept. It is not organic demand.
round(tapply(contrib$contribution, contrib$channel, sum))
#> (baseline)    display     search     social         tv      video 
#>    3190425      40853     208307     132709     349629     102461 

# A period the model was not fitted on: fit on the first two years and
# decompose the third, with `newdata` holding the controls for those weeks.
early <- seq_len(104)
fit_early <- stats::lm(revenue ~ ., data = cbind(revenue = north$revenue,
                                                 model_data)[early, ])
holdout <- contributions(transformed[-early, ], fit_early,
                         index = north$date[-early],
                         newdata = model_data[-early, ])
all.equal(as.numeric(tapply(holdout$contribution, holdout$period, sum)),
          unname(stats::predict(fit_early, model_data[-early, ])))
#> [1] TRUE
```
