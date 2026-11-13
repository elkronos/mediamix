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
`share`, `value` when the journey table carries conversion values, and
`p_without`, the probability of conversion once the channel is removed.
Three attributes let you check the removal effects by hand:

- `p_conversion`:

  The baseline conversion probability. `removal_effect` is
  `1 - p_without / p_conversion`, or 0 when no journey converts.

- `transitions`:

  The fitted first-order transition matrix, with one row for each state
  a walk can leave (`"(start)"` and every channel) and one column for
  each state it can enter (every channel, `"(conversion)"` and
  `"(null)"`). Each row sums to 1.

- `transition_counts`:

  The transitions observed, in the same layout, from which `transitions`
  is estimated.

A zero-row journey table gives a zero-row result without these
attributes.

## Details

The probabilities come from the absorbing-chain fundamental matrix, so
they are exact for the fitted transition matrix rather than simulated:
with \\Q\\ the transitions among transient states and \\R\\ those into
the absorbing states, absorption probabilities are \\(I - Q)^{-1} R\\.
One inverse serves every channel. Once channel \\s\\ is removed, a walk
converts only if it converts without reaching \\s\\, so with \\N = (I -
Q)^{-1}\\ and \\x = N R\\ the conversion probability is \\x_1 - (N\_{1s}
/ N\_{ss}) x_s\\, where \\N\_{1s} / N\_{ss}\\ is the probability of ever
reaching \\s\\ from the start and \\x_s\\ that of converting from \\s\\.
That probability is the `p_without` column.

A channel named `"(start)"`, `"(conversion)"` or `"(null)"` shares its
label with a special state in `transitions`; index the matrix by
position in that case.

This is the order-1 model, with no dependency beyond base R. For
higher-order chains, or journey tables with millions of distinct paths,
hand the output of
[`as_channel_paths()`](https://elkronos.github.io/mediamix/reference/as_channel_paths.md)
to ChannelAttribution, whose `markov_model()` implements the same
removal-effect definition in C++. For `order = 1` the two agree up to
the error of ChannelAttribution's simulation.

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
mk <- markov_removal(p)
mk
#>   channel removal_effect conversions     share p_without
#> 1       b            1.0   1.3333333 0.6666667 0.0000000
#> 2       a            0.5   0.6666667 0.3333333 0.3333333

# The fitted chain behind those numbers
attr(mk, "transitions")
#>          to
#> from              a         b (conversion) (null)
#>   (start) 0.6666667 0.3333333            0    0.0
#>   a       0.0000000 0.5000000            0    0.5
#>   b       0.0000000 0.0000000            1    0.0
attr(mk, "p_conversion")
#> [1] 0.6666667
1 - mk$p_without / attr(mk, "p_conversion")
#> [1] 1.0 0.5

data(mm_events)
paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                     timestamp = "timestamp", conversion = "conversion",
                     value = "value")
markov_removal(paths)
#>           channel removal_effect conversions      share     value p_without
#> 1     paid_search     0.43712431   487.56069 0.17787694 61314.695 0.2527178
#> 2         display     0.41872296   467.03614 0.17038896 58733.567 0.2609796
#> 3          social     0.40268569   449.14846 0.16386299 56484.047 0.2681799
#> 4  organic_search     0.37610320   419.49882 0.15304590 52755.365 0.2801148
#> 5           email     0.31515234   351.51532 0.12824346 44205.890 0.3074803
#> 6       affiliate     0.25688705   286.52725 0.10453384 36033.116 0.3336401
#> 7         (blank)     0.07606490    84.84143 0.03095273 10669.496 0.4148249
#> 8          (none)     0.06855467    76.46466 0.02789663  9616.049 0.4181968
#> 9          direct     0.06592975    73.53687 0.02682848  9247.856 0.4193754
#> 10      (missing)     0.04022868    44.87035 0.01637007  5642.809 0.4309145
```
