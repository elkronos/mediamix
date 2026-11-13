# One adstock step per channel

Adds one
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
per media column, so that each channel gets its own decay, or its own
tuning parameter. A single step applies one set of kernel parameters to
every column it selects, and channels rarely share a carryover:
`mm_weekly` was simulated with a decay of 0.85 for television and 0.15
for search.

## Usage

``` r
step_adstock_channels(recipe, channels, ...)
```

## Arguments

- recipe:

  A recipe object.

- channels:

  Character vector naming the media columns, one step each.

- ...:

  Named arguments for
  [`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md),
  such as `index`, `by`, `decay` and `kernel`, applied to every channel.
  The kernel parameters `decay`, `max_lag`, `shape`, `scale` and `peak`
  also take one value per channel, as a vector or list named by channel:
  `decay = c(tv = 0.85, search = 0.15)`. Marking one of them with
  `tune()` gives each channel a tuning parameter of its own, with the
  ids `decay_tv`, `decay_search` and so on; `tune("carry")` gives
  `carry_tv`. A `tune()` without an id inside a list named by channel
  gets the same form of id.

## Value

`recipe` with one
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
added per channel, in the order of `channels`.

## Details

Each step selects its channel with
[`all_of()`](https://tidyselect.r-lib.org/reference/all_of.html) and the
channel's name as a string, fixed when the step is added. A loop that
writes `all_of(ch)` selects the last channel in every step instead,
since selectors are evaluated at
[`prep()`](https://recipes.tidymodels.org/reference/prep.html), after
the loop has finished. Step ids start with `adstock_` and the channel's
name, so [`tidy()`](https://generics.r-lib.org/reference/tidy.html) on
the recipe shows which step is which.

With `tune()`,
[`hardhat::extract_parameter_set_dials()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
lists one parameter per channel, and
[`tune::tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html)
and
[`tune::tune_bayes()`](https://tune.tidymodels.org/reference/tune_bayes.html)
search them together with the rest of the workflow's parameters.

## See also

[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md)
for the arguments,
[`carryover_decay()`](https://elkronos.github.io/mediamix/reference/mediamix_params.md)
for the tuning ranges.

## Examples

``` r
library(recipes)
data(mm_weekly)

# A fixed decay for each channel
rec <- recipe(revenue ~ ., data = mm_weekly) |>
  step_adstock_channels(c("tv", "search"), index = "date", by = "geo",
                        decay = c(tv = 0.85, search = 0.15)) |>
  prep()
tidy(rec)
#> # A tibble: 2 × 6
#>   number operation type    trained skip  id                  
#>    <int> <chr>     <chr>   <lgl>   <lgl> <chr>               
#> 1      1 step      adstock TRUE    FALSE adstock_tv_vYRsf    
#> 2      2 step      adstock TRUE    FALSE adstock_search_RyDPM
tidy(rec, number = 2)
#> # A tibble: 3 × 13
#>   terms  group   kernel    decay max_lag shape scale type   peak start state
#>   <chr>  <chr>   <chr>     <dbl>   <dbl> <dbl> <dbl> <chr> <dbl> <chr> <dbl>
#> 1 search north   geometric  0.15     Inf    NA    NA NA       NA zero   592.
#> 2 search central geometric  0.15     Inf    NA    NA NA       NA zero   368.
#> 3 search south   geometric  0.15     Inf    NA    NA NA       NA zero   769.
#> # ℹ 2 more variables: state_length <int>, id <chr>

# One tuning parameter per channel
tuned <- recipe(revenue ~ ., data = mm_weekly) |>
  step_adstock_channels(c("tv", "search"), index = "date", by = "geo",
                        decay = hardhat::tune())
hardhat::extract_parameter_set_dials(tuned)
#> Collection of 2 parameters for tuning
#> 
#>    identifier  type    object
#>      decay_tv decay nparam[+]
#>  decay_search decay nparam[+]
#> 
```
