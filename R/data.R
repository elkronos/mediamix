#' Synthetic weekly marketing mix panel
#'
#' Three years of weekly media spend, price and revenue across three
#' geographies, generated from known adstock and saturation parameters so that
#' a modelling workflow can be checked against the truth it is trying to
#' recover.
#'
#' @format A data frame with 468 rows and 11 columns:
#' \describe{
#'   \item{date}{Week commencing, `Date`.}
#'   \item{geo}{Geography: `"north"`, `"central"` or `"south"`.}
#'   \item{tv, video, search, display, social}{Media spend for the week.}
#'   \item{price}{Average unit price.}
#'   \item{seasonality}{Underlying seasonal index used to generate the data.}
#'   \item{holiday}{1 in December weeks, 0 otherwise.}
#'   \item{revenue}{The KPI.}
#' }
#'
#' @details
#' Revenue was generated as a baseline plus trend, a price term, seasonality, a
#' December uplift, and for each channel
#' `beta * saturate_hill(adstock_geometric(spend, decay), half_max, shape)`,
#' with normally distributed noise. The generating parameters are attached as
#' the `"truth"` attribute:
#'
#' ```r
#' str(attr(mm_weekly, "truth"))
#' ```
#'
#' Television is the long-carryover, S-shaped channel (decay 0.85, shape 1.6)
#' and search is the short-carryover, immediately-responding one (decay 0.15).
#' Channels are flighted, so several contain runs of zero-spend weeks: dark
#' periods are the normal case in media data, not an edge case.
#'
#' Geographies differ by a single multiplicative scale factor applied to spend,
#' half-saturation points, coefficients and baseline, so a correctly specified
#' pooled model can fit them together and a per-geography model should recover
#' the same decay parameters in each.
#'
#' @source Simulated. See `data-raw/make_mm_weekly.R` in the package sources.
#'
#' @seealso [mm_events] for the attribution counterpart.
#'
#' @examples
#' data(mm_weekly)
#' str(mm_weekly)
#'
#' # The parameters the data was generated from
#' attr(mm_weekly, "truth")$decay
#'
#' # Flighting: several channels go dark for weeks at a time
#' north <- mm_weekly[mm_weekly$geo == "north", ]
#' mean(north$tv == 0)
"mm_weekly"

#' Synthetic touchpoint event log
#'
#' A raw marketing event log of the kind exported from a web analytics or
#' customer data platform, containing on purpose every case that breaks a naive
#' journey-building pipeline.
#'
#' @format A data frame with 20,799 rows and 5 columns:
#' \describe{
#'   \item{customer_id}{Customer identifier.}
#'   \item{channel}{Channel label. Includes direct-traffic labels
#'     (`"direct"`, `"(none)"`, `""`) and genuine `NA` values.}
#'   \item{timestamp}{Event time, `POSIXct` in UTC.}
#'   \item{conversion}{1 on a converting event, 0 otherwise.}
#'   \item{value}{Conversion value, `NA` except on converting events.}
#' }
#'
#' @details
#' The awkward cases are deliberate, because they are what a journey builder has
#' to handle:
#'
#' - **Repeat converters.** Some customers convert two or three times, separated
#'   by dormant periods of 20 to 140 days. Treated as one journey per customer
#'   they become a single implausibly long path; split on conversion they are
#'   several short ones.
#' - **Consecutive duplicates.** Half of all multi-touch bursts contain the same
#'   channel twice in a row -- some placed deliberately, the rest arising by
#'   chance from a six-channel sampler.
#' - **Direct, none, blank and missing channels.** A quarter of bursts contain
#'   one; 261 events have a genuinely missing channel.
#' - **Non-converters.** 62% of customers never convert. They are the null paths
#'   that Markov removal effects are computed against.
#' - **Timestamp ties.** 624 events share a customer and a timestamp exactly, so
#'   journey order depends on a deterministic tie-break.
#' - **Channels with positional roles.** Display and social tend to open
#'   journeys, paid search and email to close them, so first-touch and
#'   last-touch attribution genuinely disagree about them -- which is what makes
#'   [attribution_spread()] and [channel_positions()] worth looking at.
#' - **Unsorted rows.** The table is shuffled, as a real export would be.
#'
#' @source Simulated. See `data-raw/make_mm_events.R` in the package sources.
#'
#' @seealso [build_paths()], [mm_weekly] for the media-transform counterpart.
#'
#' @examples
#' data(mm_events)
#' str(mm_events)
#'
#' # The awkward cases really are in there
#' sum(is.na(mm_events$channel))
#' table(mm_events$channel, useNA = "ifany")
#'
#' # Rows are shuffled, as a real export would be
#' is.unsorted(mm_events$timestamp)
#'
#' # Customers converting more than once
#' conv <- table(mm_events$customer_id[mm_events$conversion == 1])
#' table(conv)
"mm_events"
