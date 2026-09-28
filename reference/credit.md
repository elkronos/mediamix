# Assign conversion credit across a journey

Five rules for dividing one conversion among the touchpoints that
preceded it, plus a hook for your own. Each returns the journey table
with a `credit` column added, so they compose and can be compared side
by side.

## Usage

``` r
credit_linear(paths)

credit_first(paths)

credit_last(paths)

credit_position(paths, first_weight = 0.4, last_weight = 0.4)

credit_time_decay(paths, decay = decay_from_half_life(7), period = 1)

credit_custom(paths, fn, normalise = TRUE)
```

## Arguments

- paths:

  An `mm_paths` object from
  [`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md).

- first_weight, last_weight:

  Share of credit reserved for the first and last touch in
  `credit_position()`. Must be non-negative and sum to at most 1; the
  remainder is split evenly among the middle touches.

- decay:

  Geometric decay coefficient in `[0, 1)` for `credit_time_decay()`. Use
  [`decay_from_half_life()`](https://elkronos.github.io/mediamix/reference/decay_vocabulary.md)
  to express it as a half-life. The default is a seven-period half-life.

- period:

  Time span, in the journey table's time units, over which `decay`
  applies once. See
  [`decay_from_half_life()`](https://elkronos.github.io/mediamix/reference/decay_vocabulary.md).

- fn:

  For `credit_custom()`, a function of `(touch_rank, touch_n, recency)`
  returning a numeric vector of weights the same length as its inputs.
  `recency` is time from each touch to the conversion, in the journey
  table's units – see the `units` argument of
  [`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md),
  which sets them. `fn` is called once per *converting* journey and
  never for a non-converting one, so `recency` is always present and the
  weights need no missing-value handling. Weights must be finite and
  non-negative; returning all zeros declines the journey.

- normalise:

  For `credit_custom()`, should weights be rescaled to sum to 1 within
  each journey? Defaults to `TRUE`. Setting it to `FALSE` breaks the
  guarantee that total credit equals total conversions, and is only
  sensible when `fn` already returns weights that sum to 1.

## Value

The input `mm_paths` object with a numeric `credit` column added, and a
`credit_value` column when conversion values are present. Credit is `0`
on every touch of a non-converting journey, and sums to 1 within each
converting journey.

The five built-in rules always assign positive weight somewhere, so for
them total credit always equals the number of converting journeys.
`credit_custom()` can decline a journey by returning all-zero weights –
see its entry under *Custom rules*.

## Details

`credit_first()` and `credit_last()` are the two defaults most reporting
systems ship with, and they disagree with each other by design:
comparing them is the cheapest read on whether a channel opens journeys
or closes them.
[`attribute()`](https://elkronos.github.io/mediamix/reference/attribute.md)
runs several rules at once for exactly this reason.

`credit_position()` gives the first and last touch a fixed share and
splits the rest evenly. Two-touch journeys are a genuine edge case,
since there are no middle touches to receive the middle weight; here the
middle share is divided between the two touches rather than discarded,
so credit still sums to 1 and short journeys are not quietly
under-counted.

`credit_time_decay()` is
[`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md)
run backwards. Adstock takes one impulse of spend and spreads its effect
forward in time with geometric decay; time-decay attribution takes one
conversion and spreads its credit backward across prior touchpoints with
the same geometric kernel. Same arithmetic, opposite arrow, and the same
`decay` vocabulary in both directions.

## Custom rules

`credit_custom()` takes a function of `(touch_rank, touch_n, recency)`
and is called once per converting journey, with vectors as long as that
journey. Non-converting journeys are never passed to it; they always get
zero.

A rule that qualifies only some touches – "credit only touches within a
day of conversion", say – will meet journeys where *nothing* qualifies.
When `fn` returns all zeros for a journey, that journey receives no
credit at all and is counted in a message. Crediting its touches equally
instead would invent an answer the rule never gave, and it is a
surprisingly easy way to hand a channel thousands of conversions it was
never eligible for. The cost is that total credit is then *below* the
number of converting journeys, by exactly the number of declined
journeys.

## What credit is not

These rules divide credit; they do not measure contribution. A rule
cannot tell you what would have happened if a channel had not run,
because that outcome is not in the log. Use them for consistent
bookkeeping and for comparing channels' roles, and use experiments for
incrementality.

## See also

[`attribute()`](https://elkronos.github.io/mediamix/reference/attribute.md),
[`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md),
[`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md)

## Examples

``` r
data(mm_events)
paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                     timestamp = "timestamp", conversion = "conversion",
                     value = "value")

lin <- credit_linear(paths)
head(lin[, c("path_id", "channel", "touch_rank", "touch_n", "credit")])
#>        path_id        channel touch_rank touch_n credit
#> 1 cust_00001#1 organic_search          1       1    1.0
#> 2 cust_00001#2        display          1       5    0.2
#> 3 cust_00001#2 organic_search          2       5    0.2
#> 4 cust_00001#2          email          3       5    0.2
#> 5 cust_00001#2         social          4       5    0.2
#> 6 cust_00001#2 organic_search          5       5    0.2

# Credit sums to 1 within every converting journey
conv <- lin[lin$converted, ]
per_journey <- as.numeric(tapply(conv$credit, conv$path_id, sum))
all.equal(per_journey, rep(1, length(per_journey)))
#> [1] TRUE

# Time decay with a three-day half-life
td <- credit_time_decay(paths, decay = decay_from_half_life(3))

# Your own rule: credit only touches within a day of conversion. Journeys
# whose every touch is older than that qualify for nothing, and are reported
# rather than being credited equally.
recent_only <- credit_custom(paths, function(rank, n, recency) {
  as.numeric(recency <= 1)
})
#> 482 converting journeys received no credit: the rule assigned zero weight to
#> every touch.
#> ℹ Those conversions are not counted in any channel's total.
#> ℹ Total credit is 2259, against 2741 converting journeys.
c(credited = sum(recent_only$credit),
  converting = path_summary(paths)$converting_journeys)
#>   credited converting 
#>       2259       2741 
```
