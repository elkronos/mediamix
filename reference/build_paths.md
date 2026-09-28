# Build customer journeys from a raw event log

Turns a table of individual touchpoint events into the journeys that
attribution models consume. This is the step most attribution packages
assume you have already done, and the step where most attribution
numbers are quietly decided.

## Usage

``` r
build_paths(
  events,
  id,
  channel,
  timestamp,
  conversion = NULL,
  value = NULL,
  lookback = NULL,
  split_on = c("conversion", "gap", "none"),
  gap = NULL,
  units = c("days", "hours", "mins", "secs", "weeks"),
  collapse_repeats = TRUE,
  keep_null_paths = TRUE,
  direct = c("keep", "drop", "label"),
  direct_labels = c("direct", "(direct)", "none", "(none)", "(not set)", "unknown", ""),
  tz = "UTC"
)
```

## Arguments

- events:

  A data frame of touchpoint events, one row per event.

- id:

  Name of the column identifying the customer or device, as a string.

- channel:

  Name of the column naming the marketing channel, as a string.

- timestamp:

  Name of the column holding the event time, as a string. May be
  `POSIXct`, `Date`, or numeric (a period index).

- conversion:

  Name of a column flagging conversion events, as a string, or `NULL`.
  Logical, or numeric where non-zero means converted.

- value:

  Name of a column holding conversion value (revenue), as a string, or
  `NULL`. Only read on rows where `conversion` is true.

- lookback:

  Maximum age of a touchpoint relative to the end of its journey.
  Touches older than this are dropped. `NULL` (the default) keeps
  everything. Numeric, interpreted in `units`, or a `difftime`.

- split_on:

  How to divide a customer's events into journeys. Any of `"conversion"`
  (a new journey begins after each conversion), `"gap"` (a new journey
  begins after `gap` of inactivity), or `"none"` (one journey per
  customer). `"conversion"` and `"gap"` may be combined. Defaults to
  `"conversion"`.

- gap:

  Inactivity that starts a new journey, required when `split_on`
  includes `"gap"`. Numeric in `units`, or a `difftime`.

- units:

  Time unit in which durations are expressed, defaulting to `"days"`.
  This sets three things at once, so changing it changes more than it
  first appears: the unit for `lookback` and `gap` when they are given
  as plain numbers; the unit of the returned `time_to_conversion` column
  and of
  [`conversion_lag()`](https://elkronos.github.io/mediamix/reference/conversion_lag.md);
  and therefore the meaning of `period` in
  [`credit_time_decay()`](https://elkronos.github.io/mediamix/reference/credit.md),
  whose default half-life is seven *units*. Switching from `"days"` to
  `"hours"` for a gap will shorten a time-decay half-life by a factor of
  24 unless `period` is changed to match.

  For a numeric `timestamp` column the index is taken at face value and
  `units` has no effect; passing a `difftime` against a numeric index is
  an error, since a calendar duration has no meaning there.

- collapse_repeats:

  Collapse runs of the same channel (`A, A, A` becomes `A`)? Defaults to
  `TRUE`. See *Details*.

- keep_null_paths:

  Retain journeys that never converted? Defaults to `TRUE`, and you
  should think hard before changing it. See *Details*.

- direct:

  What to do with direct, none and missing channel labels: `"keep"` them
  as they are, `"drop"` those touches, or `"label"` them all as a single
  `"(direct)"` channel; defaults to `"keep"`. Under `"keep"`, a missing
  label becomes `"(missing)"` and a blank one `"(blank)"`, since a
  channel with no name at all cannot be indexed or reported on; every
  other label is left untouched.

- direct_labels:

  Channel labels treated as direct traffic, matched case-insensitively
  after trimming whitespace. Missing channel values are always treated
  as direct.

- tz:

  Time zone used when coercing a character `timestamp` column.

## Value

An object of class `mm_paths`, a data frame with one row per retained
touchpoint and the columns

- `path_id`:

  Journey identifier, unique across the whole table.

- `id`:

  The customer identifier, carried through.

- `channel`:

  Channel label.

- `timestamp`:

  Event time.

- `touch_rank`:

  Position of this touch within its journey, from 1.

- `touch_n`:

  Number of touches in the journey.

- `converted`:

  Did this journey end in a conversion?

- `conversion_value`:

  Journey-level conversion value, or `NA`.

- `time_to_conversion`:

  Time from this touch to the journey's conversion, in `units`. `NA` for
  non-converting journeys.

Construction counts are attached as attributes and reported by
[`path_summary()`](https://elkronos.github.io/mediamix/reference/path_summary.md).

## Details

Seven things go wrong between an event log and a set of journeys. All
seven are arguments here rather than assumptions.

**Journey splitting.** A customer who converts in March and again in
September is two journeys, not one nine-month journey with two
conversions. Treating them as one both understates journey counts and
lets March's touchpoints take credit for September's sale.

**Lookback windows.** A touch two years before a conversion did not
cause it. Thirty, sixty and ninety days are the usual choices;
[`conversion_lag()`](https://elkronos.github.io/mediamix/reference/conversion_lag.md)
shows you what your own data supports instead of guessing. For a
converting journey the window is measured back from the conversion; for
a non-converting one, from the last touch.

**Non-converting journeys.** These are kept by default because Markov
removal effects are computed *against* them: the transition matrix needs
to know how often a path ends in nothing. Dropping them does not merely
lose data, it biases every channel's estimated effect upward, and it
does so silently.

**Direct, none and missing channels.** Every real log has them and every
tutorial handles them differently. `"drop"` treats direct as
non-marketing noise; `"label"` keeps it as a channel so its assist role
stays visible. The choice moves the numbers, so it is explicit.

**Consecutive duplicates.** Three page views on the same channel are
usually one exposure recorded three times. `collapse_repeats = TRUE`
keeps the *first* touch of each run, which preserves when the channel
entered the journey. If you are using
[`credit_time_decay()`](https://elkronos.github.io/mediamix/reference/credit.md),
consider `FALSE`: keeping the first of a run pushes each channel's
apparent recency backward.

**Deterministic ordering.** Events sharing a timestamp are broken by
their original row order, so the same input always yields the same
journeys. Ties are common in logs written at second resolution, and a
non-deterministic tie-break makes first-touch and last-touch results
unreproducible.

**Accounting.** A direct-traffic rule can strip a converting journey of
every touchpoint, leaving a conversion that no channel can be credited
with. (A lookback alone cannot do this: the conversion touch is always
inside its own window.) Those conversions are counted and reported by
[`path_summary()`](https://elkronos.github.io/mediamix/reference/path_summary.md)
as `conversions_unattributable` rather than disappearing – including in
the degenerate case where filtering removes every row and the returned
table is empty.

## What is dropped, and when

Two things are removed that no argument controls, because they are not
choices so much as consequences of what a journey is.

Touches occurring *after* a journey's conversion are dropped. With the
default `split_on = "conversion"` this is invisible, since every journey
ends at its conversion by construction. Under `split_on = "none"` or
`"gap"` it is not: a customer's events after their last conversion are
discarded, because a touch that happened after the sale cannot have
caused it. If you want those events, they belong to the next journey –
split on conversion.

Journeys with no remaining touchpoints are dropped entirely, and any
conversion they carried is reported as unattributable rather than as
absent.

## On what these numbers mean

Rule-based attribution is a bookkeeping convention, not a causal
estimate. It divides observed conversions among observed touchpoints
according to a rule you chose; it does not tell you what would have
happened had a channel not run. Only an experiment or a credible
quasi-experiment answers that. A well-built journey table makes the
bookkeeping honest and comparable across rules, which is worth a great
deal, and it is not incrementality.

## See also

[`credit_linear()`](https://elkronos.github.io/mediamix/reference/credit.md)
and friends for assigning credit,
[`attribute()`](https://elkronos.github.io/mediamix/reference/attribute.md)
to compare rules,
[`conversion_lag()`](https://elkronos.github.io/mediamix/reference/conversion_lag.md)
to choose a lookback,
[`as_channel_paths()`](https://elkronos.github.io/mediamix/reference/as_channel_paths.md)
to hand off to ChannelAttribution.

## Examples

``` r
data(mm_events)
head(mm_events)
#>   customer_id        channel           timestamp conversion value
#> 1  cust_03227          email 2025-02-10 18:08:57          0    NA
#> 2  cust_03940                2025-05-20 04:44:17          0    NA
#> 3  cust_04838 organic_search 2025-04-04 22:20:01          0    NA
#> 4  cust_00749      affiliate 2025-09-27 03:31:38          0    NA
#> 5  cust_05340           <NA> 2025-02-24 22:17:01          0    NA
#> 6  cust_05271         social 2025-02-18 19:00:56          0    NA

paths <- build_paths(
  mm_events,
  id = "customer_id",
  channel = "channel",
  timestamp = "timestamp",
  conversion = "conversion",
  value = "value"
)
path_summary(paths)
#>   events_in events_out journeys converting_journeys mean_length median_length
#> 1     20799      17306     6137                2741    2.819945             3
#>   max_length direct_events conversion_events conversions_observed
#> 1          9          1597              2741                 2741
#>   conversions_unattributable
#> 1                          0

# A 30-day lookback, splitting also on two weeks of inactivity
paths30 <- build_paths(
  mm_events,
  id = "customer_id", channel = "channel", timestamp = "timestamp",
  conversion = "conversion", value = "value",
  lookback = 30,
  split_on = c("conversion", "gap"),
  gap = 14
)
path_summary(paths30)
#>   events_in events_out journeys converting_journeys mean_length median_length
#> 1     20799      17317     6215                2741    2.786323             3
#>   max_length direct_events conversion_events conversions_observed
#> 1          9          1597              2741                 2741
#>   conversions_unattributable
#> 1                          0
```
