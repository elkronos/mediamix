# Markov-chain removal effects

Data-driven attribution from a first-order Markov model of the journey
table (Anderl, Becker, von Wangenheim and Schumann, 2016). Journeys are
treated as walks from a start state through channel states to one of two
absorbing states, conversion or null. A channel's *removal effect* is
the proportional drop in the probability of reaching conversion when
every transition into that channel is redirected to the null state;
conversions are then divided among channels in proportion to their
removal effects.

## Usage

``` r
markov_removal(paths)
```

## Arguments

- paths:

  An `mm_paths` object from
  [`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md).
  Keep the non-converting journeys (`keep_null_paths = TRUE`, the
  default): they are what the null state is estimated from.

## Value

A data frame with one row per channel, ordered by descending
`conversions`: `channel`, `removal_effect`, `conversions` (credited),
`share`, and `value` when the journey table carries conversion values.
The baseline conversion probability is attached as the attribute
`"p_conversion"`.

## Details

The probabilities come from the absorbing-chain fundamental matrix, so
they are exact for the fitted transition matrix rather than simulated:
with \\Q\\ the transitions among transient states and \\R\\ those into
the absorbing states, absorption probabilities are \\(I - Q)^{-1} R\\.

This is the order-1 model, with no dependency beyond base R. For
higher-order chains, or journey tables with millions of distinct paths,
hand the output of
[`as_channel_paths()`](https://elkronos.github.io/mediamix/reference/as_channel_paths.md)
to ChannelAttribution, whose `markov_model()` implements the same
removal-effect definition in C++. The two should agree for `order = 1`.

Removal effects are sensitive to journey construction: repeats collapsed
or not, direct traffic kept or dropped, lookback length. That is the
argument for
[`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md)
making each an explicit choice.

## What this is not

A removal effect is a statement about the fitted transition matrix, not
about what customers would do if a channel were switched off. Removing a
state from a chain assumes that everyone who would have passed through
it is lost, and that nobody reaches the same conversion another way. It
is a more data-driven convention than first- or last-touch, and it is
still a convention. Use it alongside the heuristic rules in
[`attribute()`](https://elkronos.github.io/mediamix/reference/attribute.md),
where the spread between them is the honest summary, and use experiments
for incrementality.

## References

Anderl, E., Becker, I., von Wangenheim, F. and Schumann, J. H. (2016).
Mapping the customer journey: Lessons learned from graph-based online
attribution modeling. *International Journal of Research in Marketing*,
33(3), 457–474.
[doi:10.1016/j.ijresmar.2016.03.001](https://doi.org/10.1016/j.ijresmar.2016.03.001)

## See also

[`attribute()`](https://elkronos.github.io/mediamix/reference/attribute.md),
which includes this as the `"markov"` rule,
[`as_channel_paths()`](https://elkronos.github.io/mediamix/reference/as_channel_paths.md)
for ChannelAttribution.

## Examples

``` r
# A three-journey example small enough to check by hand: P(conversion) is
# 2/3; removing "a" halves it, removing "b" makes conversion impossible.
ev <- data.frame(
  id = c("j1", "j1", "j2", "j3"),
  ch = c("a", "b", "a", "b"),
  ts = c(1, 2, 1, 1),
  conv = c(0, 1, 0, 1)
)
p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
                 conversion = "conv")
markov_removal(p)
#>   channel removal_effect conversions     share
#> 1       b            1.0   1.3333333 0.6666667
#> 2       a            0.5   0.6666667 0.3333333

data(mm_events)
paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                     timestamp = "timestamp", conversion = "conversion",
                     value = "value")
markov_removal(paths)
#>           channel removal_effect conversions      share     value
#> 1     paid_search     0.43654077   488.15233 0.17809279 61389.098
#> 2         display     0.41761567   466.98974 0.17037203 58727.732
#> 3          social     0.40124498   448.68357 0.16369339 56425.583
#> 4  organic_search     0.37534315   419.71941 0.15312638 52783.106
#> 5           email     0.31479757   352.01561 0.12842598 44268.806
#> 6       affiliate     0.25588157   286.13406 0.10439039 35983.669
#> 7         (blank)     0.07586037    84.82923 0.03094828 10667.961
#> 8          (none)     0.06822517    76.29133 0.02783339  9594.251
#> 9          direct     0.06572696    73.49776 0.02681421  9242.937
#> 10      (missing)     0.03996230    44.68698 0.01630317  5619.748
```
