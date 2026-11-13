# Build customer journeys from a raw event log

Turns a table of individual touchpoint events into the journeys that
attribution models consume. This is the step that attribution packages
such as ChannelAttribution leave to you, and the step where most
attribution numbers are quietly decided.

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
  tz = "UTC",
  timestamp_format = NULL,
  dedupe = TRUE,
  keep = NULL,
  conversion_touch = c("keep", "drop")
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
  `POSIXct`, `Date`, numeric (a period index), or character. See
  *Timestamps* for how character and numeric values are read.

- conversion:

  Name of a column flagging conversion events, as a string, or `NULL`.
  Logical, numeric where non-zero means converted, or character holding
  numbers or `"true"`/`"false"`, `"yes"`/`"no"`, `"t"`/`"f"`,
  `"y"`/`"n"`. Any other character value is an error.

- value:

  Name of a column holding conversion value (revenue), as a string, or
  `NULL`. Only read on rows where `conversion` is true.

- lookback:

  Maximum age of a touchpoint relative to the end of its journey.
  Touches older than this are dropped. `NULL` (the default) or `Inf`
  keeps everything. Numeric, interpreted in `units`, or a `difftime`.

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

  Durations are exact, so a day is 86,400 seconds and a week 604,800. In
  a time zone with daylight saving, a seven-day `lookback` that spans
  the change therefore reaches back to a wall-clock time one hour off.

  For a numeric `timestamp` column the index is taken at face value,
  `units` has no effect (supplying it gives a warning) and the table
  records its units as `"index"`; passing a `difftime` against a numeric
  index is an error, since a calendar duration has no meaning there.

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

  Time zone used when reading a character `timestamp` column. Values
  that carry their own UTC offset keep it; `tz` applies to the rest.

- timestamp_format:

  A [`strptime()`](https://rdrr.io/r/base/strptime.html) format for a
  character `timestamp` column, such as `"%d/%m/%Y %H:%M"`, or `NULL`
  (the default) to accept ISO 8601 dates and date-times only. Every
  value must match the whole format.

- dedupe:

  Drop exact duplicate events before building journeys? Defaults to
  `TRUE`. Two rows are duplicates when they share the customer, channel,
  timestamp, conversion flag and conversion value, as when a tracking
  pixel fires twice; without this a duplicated conversion row becomes a
  second converting journey.
  [`path_summary()`](https://elkronos.github.io/mediamix/reference/path_summary.md)
  reports how many rows were dropped.

- keep:

  Names of other columns of `events` to carry into the result, such as
  campaign, device or cost, or `NULL` (the default) for none. They
  travel per touch, each row of the result taking its values from the
  event it was built from, since one journey can span several campaigns
  or devices. Kept columns play no part in building journeys. Duplicates
  are still found on the five columns `dedupe` names, and a run that
  `collapse_repeats` merges keeps its first event's values, so a cost
  column then holds that event's cost rather than the run's total; set
  `collapse_repeats = FALSE` to keep every event. A kept column cannot
  share a name with a column of the result, or with `credit` and
  `credit_value`, which the credit rules add.

- conversion_touch:

  Does the channel recorded on a conversion event count as a touch?
  `"keep"` (the default) counts each conversion event as a touch, the
  last of its journey under the default `split_on`, so a sale logged on
  `"email"` gives email a touch. `"drop"` removes conversion events from
  the touches once the journey's conversion time and value have been
  read from them; use it when the conversion row records where the sale
  happened, such as a checkout page, rather than a marketing touch. A
  touch sharing the conversion's timestamp stays. A journey whose only
  event was its conversion is left with no touches, and its conversion
  is reported as unattributable.

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

followed by the columns named in `keep`, in that order. Construction
counts are attached as attributes and reported by
[`path_summary()`](https://elkronos.github.io/mediamix/reference/path_summary.md).

## Details

Seven things go wrong between an event log and a set of journeys. The
first five are arguments here rather than assumptions, and the last two,
ordering and accounting, are handled the same way every time.

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
to know how often a path ends in nothing. Without them the baseline
conversion probability is 1, and every channel's removal effect moves,
up or down depending on how the dropped journeys differed from the
converting ones.
[`markov_removal()`](https://elkronos.github.io/mediamix/reference/markov_removal.md)
warns when a table has none.

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
apparent recency backward. Exact duplicate rows are a different case:
they are one event logged twice, and `dedupe = TRUE` drops them before
journeys are built, so a conversion recorded twice is one conversion.

**Deterministic ordering.** Events sharing a timestamp are broken by
their original row order, so the same input always yields the same
journeys. The one exception is a conversion, which sorts after every
touch sharing its timestamp: a touch logged in the same second as a sale
belongs to the journey that sale closes, wherever the two rows sit in
the file. Ties are common in logs written at second resolution, and a
non-deterministic tie-break makes first-touch and last-touch results
unreproducible.

**Accounting.** A direct-traffic rule can strip a converting journey of
every touchpoint, leaving a conversion that no channel can be credited
with, and so can `conversion_touch = "drop"`, alone or with a lookback.
(A lookback alone cannot do this while the conversion event is a touch,
since it is always inside its own window.) Those conversions are counted
and reported by
[`path_summary()`](https://elkronos.github.io/mediamix/reference/path_summary.md)
as `conversions_unattributable` rather than disappearing, including in
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
caused it. If you want those events, split on conversion: they belong to
the next journey.

Journeys with no remaining touchpoints are dropped entirely, and any
conversion they carried is reported as unattributable rather than as
absent.

## Timestamps

A character `timestamp` column is parsed completely or not at all.
Without `timestamp_format`, every value must be an ISO 8601 date such as
`"2024-05-01"` or date-time such as `"2024-05-01 09:30"`,
`"2024-05-01T09:30:15.250Z"` or `"2024-05-01T11:30:15+02:00"`; a `/` may
replace the `-` in the date, and a trailing `UTC` or `GMT` reads as
offset zero. A value with a UTC offset is that instant. A value without
one is read in `tz`, and a date alone is midnight in `tz`. A column that
mixes dates with date-times is an error, because reading `"2024-05-01"`
as midnight would place it before every touch logged later that day. Any
other text, such as `"05/01/2024"`, is an error that names values which
did not parse; give `timestamp_format`, or convert the column with
[`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html) first.

A numeric `timestamp` column is a period index taken at face value:
`lookback`, `gap` and `time_to_conversion` are all in index units, and
`units` has no effect. Values that look like Unix epoch time give a
warning, since seconds since 1970 read as an index make `lookback = 30`
thirty seconds.

## On what these numbers mean

Rule-based attribution is a bookkeeping convention, not a causal
estimate. It divides observed conversions among observed touchpoints
according to a rule you chose; it does not tell you what would have
happened had a channel not run. Only an experiment or a credible
quasi-experiment answers that. A well-built journey table makes the
bookkeeping honest and comparable across rules, which is worth a great
deal, and it is not incrementality.

## See also

[`path_summary()`](https://elkronos.github.io/mediamix/reference/path_summary.md)
for what was dropped,
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
#> 1     20799      17282     6105                2741    2.830794             3
#>   max_length direct_events conversion_events conversions_observed
#> 1          9          1597              2741                 2741
#>   conversions_unattributable journeys_before_filtering duplicate_events
#> 1                          0                      6105              117

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
#> 1     20799      17289     6162                2741    2.805745             3
#>   max_length direct_events conversion_events conversions_observed
#> 1          9          1597              2741                 2741
#>   conversions_unattributable journeys_before_filtering duplicate_events
#> 1                          0                      6162              117

# Carry a campaign column, and stop counting the checkout page as a touch
ev <- data.frame(
  id = "a", ch = c("display", "search", "checkout"), ts = 1:3,
  conv = c(0, 0, 1), campaign = c("spring", "brand", NA)
)
build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
            conversion = "conv", keep = "campaign",
            conversion_touch = "drop")
#> <mm_paths>
#> 1 journey over 2 touchpoints (1 converting)
#> Mean journey length: 2
#>   path_id id channel timestamp touch_rank touch_n converted conversion_value
#> 1     a#1  a display         1          1       2      TRUE               NA
#> 2     a#1  a  search         2          2       2      TRUE               NA
#>   time_to_conversion campaign
#> 1                  2   spring
#> 2                  1    brand
```
