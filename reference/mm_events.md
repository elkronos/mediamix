# Synthetic touchpoint event log

A raw marketing event log of the kind exported from a web analytics or
customer data platform, containing on purpose every case that breaks a
naive journey-building pipeline.

## Usage

``` r
mm_events
```

## Format

A data frame with 20,799 rows and 5 columns:

- customer_id:

  Customer identifier.

- channel:

  Channel label. Includes direct-traffic labels (`"direct"`, `"(none)"`,
  `""`) and genuine `NA` values.

- timestamp:

  Event time, `POSIXct` in UTC.

- conversion:

  1 on a converting event, 0 otherwise.

- value:

  Conversion value, `NA` except on converting events.

## Source

Simulated. See `data-raw/make_mm_events.R` in the package sources.

## Details

The awkward cases are deliberate, because they are what a journey
builder has to handle:

- **Repeat converters.** Some customers convert two or three times,
  separated by dormant periods of 20 to 140 days. Treated as one journey
  per customer they become a single implausibly long path; split on
  conversion they are several short ones.

- **Consecutive duplicates.** Half of all multi-touch bursts contain the
  same channel twice in a row – some placed deliberately, the rest
  arising by chance from a six-channel sampler.

- **Direct, none, blank and missing channels.** A quarter of bursts
  contain one; 261 events have a genuinely missing channel.

- **Non-converters.** 62% of customers never convert. They are the null
  paths that Markov removal effects are computed against.

- **Timestamp ties.** 624 events share a customer and a timestamp
  exactly, so journey order depends on a deterministic tie-break.

- **Channels with positional roles.** Display and social tend to open
  journeys, paid search and email to close them, so first-touch and
  last-touch attribution genuinely disagree about them – which is what
  makes
  [`attribution_spread()`](https://elkronos.github.io/mediamix/reference/attribution_spread.md)
  and
  [`channel_positions()`](https://elkronos.github.io/mediamix/reference/channel_positions.md)
  worth looking at.

- **Unsorted rows.** The table is shuffled, as a real export would be.

## See also

[`build_paths()`](https://elkronos.github.io/mediamix/reference/build_paths.md),
[mm_weekly](https://elkronos.github.io/mediamix/reference/mm_weekly.md)
for the media-transform counterpart.

## Examples

``` r
data(mm_events)
str(mm_events)
#> 'data.frame':    20799 obs. of  5 variables:
#>  $ customer_id: chr  "cust_03227" "cust_03940" "cust_04838" "cust_00749" ...
#>  $ channel    : chr  "email" "" "organic_search" "affiliate" ...
#>  $ timestamp  : POSIXct, format: "2025-02-10 18:08:57" "2025-05-20 04:44:17" ...
#>  $ conversion : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ value      : num  NA NA NA NA NA NA NA NA NA NA ...

# The awkward cases really are in there
sum(is.na(mm_events$channel))
#> [1] 261
table(mm_events$channel, useNA = "ifany")
#> 
#>                        (none)      affiliate         direct        display 
#>            465            427           2016            444           3799 
#>          email organic_search    paid_search         social           <NA> 
#>           2537           3333           4017           3500            261 

# Rows are shuffled, as a real export would be
is.unsorted(mm_events$timestamp)
#> [1] TRUE

# Customers converting more than once
conv <- table(mm_events$customer_id[mm_events$conversion == 1])
table(conv)
#> conv
#>    1    2    3 
#> 1509  349  178 
```
