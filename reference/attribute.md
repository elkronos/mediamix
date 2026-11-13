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
  period = 1,
  by = NULL,
  times = 0L,
  level = 0.9,
  seed = NULL
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
  All six are run by default. A rule named twice is run once.

- first_weight, last_weight:

  Passed to
  [`credit_position()`](https://elkronos.github.io/mediamix/reference/credit.md).

- decay, period:

  Passed to
  [`credit_time_decay()`](https://elkronos.github.io/mediamix/reference/credit.md).

- by:

  Optional names of columns of `paths` that divide the journeys into
  segments, such as a device column carried by the `keep` argument of
  [`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md).
  Every rule then runs on each segment's journeys alone, so shares sum
  to 1 within each segment and rule (or are all 0 in a segment with no
  conversions), and the Markov chain is fitted per segment. Each column
  must take one value per journey and have no missing values. See
  *Segments*.

- times:

  Number of bootstrap replicates behind intervals on the shares: 0, the
  default, for no intervals, or from 2 to 1,000,000. See *Intervals*.

- level:

  Confidence level of the percentile intervals, in `(0, 1)`.

- seed:

  Optional random seed, one whole number, for reproducible intervals.
  The caller's random number stream is restored afterwards. Giving
  `level` or `seed` with `times = 0` has no effect, and a warning says
  so.

## Value

A data frame with one row per rule and channel, or per segment, rule and
channel when `by` is given:

- The `by` columns:

  The segment, in the columns' own types. Present only with `by`.

- `rule`:

  Which credit rule produced the row.

- `channel`:

  Channel label.

- `conversions`:

  Fractional conversions credited to the channel.

- `share`:

  `conversions` as a share of all conversions credited under that rule.
  Shares sum to 1 within each rule, and within each segment; where there
  is no conversion to share, every share is 0.

- `lower`, `upper`:

  Percentile interval for `share` at `level`, present only when `times`
  is above 0. `NA` where fewer than two replicates define the share, as
  in a segment with no converting journey.

- `value`:

  Credited conversion value, present only when the journey table carries
  values.

- `touches`:

  Number of touchpoints on the channel across all journeys, converting
  or not.

Rows are ordered by segment, then by rule in the order of `rules`, then
by descending conversions, with ties in channel order (C locale, so the
same on every platform). Segments are sorted by their values, factors in
level order.

The result has class `mm_attribution`. With `by` it also has class
`mm_attribution_segments` and the attribute `"segments"`, naming the
segment columns. With intervals it also has class
`mm_attribution_intervals` and the attributes `"level"` and `"times"`.
For these two classes
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) draws each
rule's share per channel; see
[`plot.mm_attribution_intervals()`](https://elkronos.github.io/mediamix/reference/plot.mm_attribution_intervals.md).

## Segments

A segment holds whole journeys, so a column given to `by` must take one
value in each journey. A device column kept per touch can change within
a cross-device journey; give each journey one value first, for example
the device of its last touch:

    last <- paths$touch_rank == paths$touch_n
    at_last <- match(paths$path_id, paths$path_id[last])
    paths$journey_device <- paths$device[last][at_last]

Each segment is attributed as if it were the whole table: a channel's
share is its share of that segment's credited conversions, `touches`
counts that segment's touches, and a channel appears only in segments
where it has a touch. With the `"markov"` rule, one warning names every
segment with no non-converting journeys, as
[`markov_removal()`](https://elkronos.github.io/mediamix/reference/markov_removal.md)
warns about a whole table without them.

## Intervals

With `times` above 0, the journeys of each segment are resampled with
replacement `times` times, every rule is recomputed on each resample,
and `lower` and `upper` are percentile intervals of the resampled
shares. They show how far the shares move with the sample of journeys
observed, with the journey table's construction and each rule held
fixed. They say nothing about the choice of rule, which
[`attribution_spread()`](https://elkronos.github.io/mediamix/reference/attribution_spread.md)
measures.

A resample gives the same shares as `attribute()` on a journey table
holding the drawn journeys, each draw a separate journey: a heuristic
rule credits each journey on its own, so its resampled totals reuse the
credit every journey received on the full data, and the Markov chain is
refitted from the transitions of the drawn journeys. Journeys are drawn
independently, so a customer's several journeys are treated as
unrelated. A resample with no converting journey defines no share; it is
left out of the intervals and counted in a message.

Every draw comes after `seed` is set, and the caller's random number
stream is restored afterwards, as in
[`block_bootstrap()`](https://elkronos.github.io/mediamix/reference/block_bootstrap.md).
Without `seed` the draws advance the session's stream.

## See also

[`credit_linear()`](https://elkronos.github.io/mediamix/reference/credit.md)
and friends,
[`markov_removal()`](https://elkronos.github.io/mediamix/reference/markov_removal.md),
[`attribution_spread()`](https://elkronos.github.io/mediamix/reference/attribution_spread.md),
[`plot.mm_attribution()`](https://elkronos.github.io/mediamix/reference/plot.mm_attribution.md),
[`plot.mm_attribution_intervals()`](https://elkronos.github.io/mediamix/reference/plot.mm_attribution_intervals.md)

## Examples

``` r
data(mm_events)
paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                     timestamp = "timestamp", conversion = "conversion",
                     value = "value")

res <- attribute(paths)
head(res, 10)
#>      rule        channel conversions      share     value touches
#> 1  linear    paid_search   494.40675 0.18037459 63269.896    3203
#> 2  linear        display   482.35595 0.17597809 60241.653    3011
#> 3  linear         social   460.37460 0.16795863 59182.995    2818
#> 4  linear organic_search   425.06786 0.15507766 51503.251    2741
#> 5  linear          email   329.57579 0.12023925 42200.946    2178
#> 6  linear      affiliate   265.93214 0.09702012 33558.401    1734
#> 7  linear        (blank)    91.17024 0.03326167 11382.201     465
#> 8  linear         (none)    81.73690 0.02982010  9640.961     427
#> 9  linear         direct    66.63452 0.02431030  7771.148     444
#> 10 linear      (missing)    43.74524 0.01595959  5951.438     261

# Total credited conversions equals observed converting journeys, per rule
tapply(res$conversions, res$rule, sum)
#>      first       last     linear     markov   position time_decay 
#>       2741       2741       2741       2741       2741       2741 

# How much does the answer depend on the rule?
attribution_spread(res)
#>           channel  min_share  max_share mean_share       spread
#> 1         display 0.07187158 0.29113462 0.17382396 0.2192630427
#> 2     paid_search 0.09157242 0.27252827 0.18342704 0.1809558555
#> 3           email 0.05654870 0.18314484 0.12349952 0.1265961328
#> 4          social 0.11638088 0.22072236 0.16553747 0.1043414812
#> 5  organic_search 0.12623130 0.17876687 0.15470603 0.0525355710
#> 6       affiliate 0.08062751 0.11711054 0.09861140 0.0364830354
#> 7          direct 0.02225465 0.02682848 0.02410676 0.0045738324
#> 8         (blank) 0.02955126 0.03378661 0.03175589 0.0042353471
#> 9          (none) 0.02736228 0.02982010 0.02849282 0.0024578273
#> 10      (missing) 0.01568771 0.01637007 0.01603912 0.0006823617
#>    first_last_ratio
#> 1         4.0507614
#> 2         0.3360107
#> 3         0.3087649
#> 4         1.8965517
#> 5         0.7061224
#> 6         1.4524887
#> 7         1.0491803
#> 8         0.9418605
#> 9         1.0133333
#> 10        1.0232558

# How much does it depend on the sample? 90% intervals from 50 resamples
boot <- attribute(paths, rules = c("first", "last", "markov"), times = 50,
                  seed = 1)
head(boot)
#>    rule        channel conversions      share      lower      upper     value
#> 1 first        display         798 0.29113462 0.27584561 0.30603943 100719.02
#> 2 first         social         605 0.22072236 0.20889362 0.23312958  78372.54
#> 3 first organic_search         346 0.12623130 0.11601449 0.13602582  41455.44
#> 4 first      affiliate         321 0.11711054 0.10586710 0.12843015  40943.15
#> 5 first    paid_search         251 0.09157242 0.08385965 0.09976528  30259.63
#> 6 first          email         155 0.05654870 0.05136549 0.06496043  20063.21
#>   touches
#> 1    3011
#> 2    2818
#> 3    2741
#> 4    1734
#> 5    3203
#> 6    2178

# Credit per segment, here a device recorded once per customer
ev <- mm_events
ev$device <- ifelse(grepl("[02468]$", ev$customer_id), "mobile", "desktop")
kept <- build_paths(ev, id = "customer_id", channel = "channel",
                    timestamp = "timestamp", conversion = "conversion",
                    keep = "device")
by_device <- attribute(kept, rules = c("first", "last"), by = "device")
by_device[by_device$channel == "display", ]
#>     device  rule channel conversions      share touches
#> 1  desktop first display         376 0.27769572    1506
#> 16 desktop  last display         100 0.07385524    1506
#> 21  mobile first display         422 0.30425379    1505
#> 36  mobile  last display          97 0.06993511    1505
```
