# Attribute conversions to channels under several rules at once

Runs a set of credit rules over the same journey table and returns their
results stacked, so the spread between rules is visible rather than
hidden by a choice of default. That spread is the most useful output of
rule-based attribution: it bounds how much a channel's apparent value
depends on the convention rather than the data.

## Usage

``` r
attribute(
  paths,
  rules = c("linear", "first", "last", "position", "time_decay", "markov"),
  first_weight = 0.4,
  last_weight = 0.4,
  decay = decay_from_half_life(7),
  period = 1
)
```

## Arguments

- paths:

  An `mm_paths` object from
  [`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md).

- rules:

  Character vector of rules to apply: any of `"linear"`, `"first"`,
  `"last"`, `"position"`, `"time_decay"` and `"markov"`. The first five
  are heuristic conventions; `"markov"` is the data-driven
  removal-effect model of
  [`markov_removal()`](https://elkronos.github.io/mediamix/reference/markov_removal.md).
  All six are run by default.

- first_weight, last_weight:

  Passed to
  [`credit_position()`](https://elkronos.github.io/mediamix/reference/credit.md).

- decay, period:

  Passed to
  [`credit_time_decay()`](https://elkronos.github.io/mediamix/reference/credit.md).

## Value

A data frame with one row per rule and channel:

- `rule`:

  Which credit rule produced the row.

- `channel`:

  Channel label.

- `conversions`:

  Fractional conversions credited to the channel.

- `share`:

  `conversions` as a share of all conversions credited under that rule.
  Shares sum to 1 within each rule.

- `value`:

  Credited conversion value, present only when the journey table carries
  values.

- `touches`:

  Number of touchpoints on the channel across all journeys, converting
  or not.

Rows are ordered by rule, then by descending conversions.

## See also

[`credit_linear()`](https://elkronos.github.io/mediamix/reference/credit.md)
and friends,
[`markov_removal()`](https://elkronos.github.io/mediamix/reference/markov_removal.md),
[`attribution_spread()`](https://elkronos.github.io/mediamix/reference/attribution_spread.md)

## Examples

``` r
data(mm_events)
paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                     timestamp = "timestamp", conversion = "conversion",
                     value = "value")

res <- attribute(paths)
head(res, 10)
#>      rule        channel conversions      share     value touches
#> 1  linear    paid_search   495.83294 0.18089491 63491.624    3205
#> 2  linear        display   482.86548 0.17616398 60331.954    3016
#> 3  linear         social   459.31032 0.16757035 59029.877    2823
#> 4  linear organic_search   424.83690 0.15499340 51258.640    2746
#> 5  linear          email   330.16865 0.12045555 42284.474    2182
#> 6  linear      affiliate   264.79881 0.09660664 33469.561    1737
#> 7  linear        (blank)    91.28690 0.03330423 11417.768     465
#> 8  linear         (none)    81.73690 0.02982010  9696.762     427
#> 9  linear         direct    66.58452 0.02429206  7778.715     444
#> 10 linear      (missing)    43.57857 0.01589879  5943.515     261

# Total credited conversions equals observed converting journeys, per rule
tapply(res$conversions, res$rule, sum)
#>      first       last     linear     markov   position time_decay 
#>       2741       2741       2741       2741       2741       2741 

# How much does the answer depend on the rule?
attribution_spread(res)
#>           channel  min_share  max_share mean_share       spread
#> 1         display 0.07187158 0.29149945 0.17387971 0.2196278730
#> 2     paid_search 0.09193725 0.27252827 0.18376127 0.1805910252
#> 3           email 0.05654870 0.18314484 0.12365610 0.1265961328
#> 4          social 0.11638088 0.21926304 0.16513028 0.1028821598
#> 5  organic_search 0.12805545 0.17876687 0.15500798 0.0507114192
#> 6       affiliate 0.08062751 0.11638088 0.09828284 0.0357533747
#> 7         (blank) 0.02918643 0.03390476 0.03171303 0.0047183334
#> 8          direct 0.02225465 0.02681421 0.02403805 0.0045595618
#> 9          (none) 0.02736228 0.02982010 0.02848199 0.0024578273
#> 10      (missing) 0.01568771 0.01641737 0.01604876 0.0007296607
#>    first_last_ratio
#> 1         4.0558376
#> 2         0.3373494
#> 3         0.3087649
#> 4         1.8840125
#> 5         0.7163265
#> 6         1.4434389
#> 7         0.9302326
#> 8         1.0327869
#> 9         1.0133333
#> 10        1.0465116
```
