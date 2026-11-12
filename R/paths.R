#' Build customer journeys from a raw event log
#'
#' Turns a table of individual touchpoint events into the journeys that
#' attribution models consume. This is the step most attribution packages assume
#' you have already done, and the step where most attribution numbers are
#' quietly decided.
#'
#' @param events A data frame of touchpoint events, one row per event.
#' @param id Name of the column identifying the customer or device, as a string.
#' @param channel Name of the column naming the marketing channel, as a string.
#' @param timestamp Name of the column holding the event time, as a string. May
#'   be `POSIXct`, `Date`, numeric (a period index), or character. See
#'   *Timestamps* for how character and numeric values are read.
#' @param conversion Name of a column flagging conversion events, as a string,
#'   or `NULL`. Logical, numeric where non-zero means converted, or character
#'   holding numbers or `"true"`/`"false"`, `"yes"`/`"no"`, `"t"`/`"f"`,
#'   `"y"`/`"n"`. Any other character value is an error.
#' @param value Name of a column holding conversion value (revenue), as a
#'   string, or `NULL`. Only read on rows where `conversion` is true.
#' @param lookback Maximum age of a touchpoint relative to the end of its
#'   journey. Touches older than this are dropped. `NULL` (the default) or
#'   `Inf` keeps everything. Numeric, interpreted in `units`, or a `difftime`.
#' @param split_on How to divide a customer's events into journeys. Any of
#'   `"conversion"` (a new journey begins after each conversion),
#'   `"gap"` (a new journey begins after `gap` of inactivity), or `"none"`
#'   (one journey per customer). `"conversion"` and `"gap"` may be combined.
#'   Defaults to `"conversion"`.
#' @param gap Inactivity that starts a new journey, required when `split_on`
#'   includes `"gap"`. Numeric in `units`, or a `difftime`.
#' @param units Time unit in which durations are expressed, defaulting to
#'   `"days"`. This sets three things at once, so changing it changes more than
#'   it first appears: the unit for `lookback` and `gap` when they are given as
#'   plain numbers; the unit of the returned `time_to_conversion` column and of
#'   [conversion_lag()]; and therefore the meaning of `period` in
#'   [credit_time_decay()], whose default half-life is seven *units*. Switching
#'   from `"days"` to `"hours"` for a gap will shorten a time-decay half-life
#'   by a factor of 24 unless `period` is changed to match.
#'
#'   Durations are exact, so a day is 86,400 seconds and a week 604,800. In a
#'   time zone with daylight saving, a seven-day `lookback` that spans the
#'   change therefore reaches back to a wall-clock time one hour off.
#'
#'   For a numeric `timestamp` column the index is taken at face value, `units`
#'   has no effect (supplying it gives a warning) and the table records its
#'   units as `"index"`; passing a `difftime` against a numeric index is an
#'   error, since a calendar duration has no meaning there.
#' @param collapse_repeats Collapse runs of the same channel
#'   (`A, A, A` becomes `A`)? Defaults to `TRUE`. See *Details*.
#' @param keep_null_paths Retain journeys that never converted? Defaults to
#'   `TRUE`, and you should think hard before changing it. See *Details*.
#' @param direct What to do with direct, none and missing channel labels:
#'   `"keep"` them as they are, `"drop"` those touches, or `"label"` them all
#'   as a single `"(direct)"` channel; defaults to `"keep"`. Under `"keep"`, a
#'   missing label becomes
#'   `"(missing)"` and a blank one `"(blank)"`, since a channel with no name at
#'   all cannot be indexed or reported on; every other label is left untouched.
#' @param direct_labels Channel labels treated as direct traffic, matched
#'   case-insensitively after trimming whitespace. Missing channel values are
#'   always treated as direct.
#' @param tz Time zone used when reading a character `timestamp` column.
#'   Values that carry their own UTC offset keep it; `tz` applies to the rest.
#' @param timestamp_format A [strptime()] format for a character `timestamp`
#'   column, such as `"%d/%m/%Y %H:%M"`, or `NULL` (the default) to accept ISO
#'   8601 dates and date-times only. Every value must match the whole format.
#' @param dedupe Drop exact duplicate events before building journeys? Defaults
#'   to `TRUE`. Two rows are duplicates when they share the customer, channel,
#'   timestamp, conversion flag and conversion value, as when a tracking pixel
#'   fires twice; without this a duplicated conversion row becomes a second
#'   converting journey. [path_summary()] reports how many rows were dropped.
#'
#' @return An object of class `mm_paths`, a data frame with one row per retained
#'   touchpoint and the columns
#'   \describe{
#'     \item{`path_id`}{Journey identifier, unique across the whole table.}
#'     \item{`id`}{The customer identifier, carried through.}
#'     \item{`channel`}{Channel label.}
#'     \item{`timestamp`}{Event time.}
#'     \item{`touch_rank`}{Position of this touch within its journey, from 1.}
#'     \item{`touch_n`}{Number of touches in the journey.}
#'     \item{`converted`}{Did this journey end in a conversion?}
#'     \item{`conversion_value`}{Journey-level conversion value, or `NA`.}
#'     \item{`time_to_conversion`}{Time from this touch to the journey's
#'       conversion, in `units`. `NA` for non-converting journeys.}
#'   }
#'   Construction counts are attached as attributes and reported by
#'   [path_summary()].
#'
#' @details
#' Seven things go wrong between an event log and a set of journeys. All seven
#' are arguments here rather than assumptions.
#'
#' **Journey splitting.** A customer who converts in March and again in
#' September is two journeys, not one nine-month journey with two conversions.
#' Treating them as one both understates journey counts and lets March's
#' touchpoints take credit for September's sale.
#'
#' **Lookback windows.** A touch two years before a conversion did not cause it.
#' Thirty, sixty and ninety days are the usual choices; [conversion_lag()] shows
#' you what your own data supports instead of guessing. For a converting
#' journey the window is measured back from the conversion; for a
#' non-converting one, from the last touch.
#'
#' **Non-converting journeys.** These are kept by default because Markov removal
#' effects are computed *against* them: the transition matrix needs to know how
#' often a path ends in nothing. Without them the baseline conversion
#' probability is 1, and every channel's removal effect moves, up or down
#' depending on how the dropped journeys differed from the converting ones.
#' [markov_removal()] warns when a table has none.
#'
#' **Direct, none and missing channels.** Every real log has them and every
#' tutorial handles them differently. `"drop"` treats direct as non-marketing
#' noise; `"label"` keeps it as a channel so its assist role stays visible. The
#' choice moves the numbers, so it is explicit.
#'
#' **Consecutive duplicates.** Three page views on the same channel are usually
#' one exposure recorded three times. `collapse_repeats = TRUE` keeps the
#' *first* touch of each run, which preserves when the channel entered the
#' journey. If you are using [credit_time_decay()], consider `FALSE`: keeping
#' the first of a run pushes each channel's apparent recency backward. Exact
#' duplicate rows are a different case: they are one event logged twice, and
#' `dedupe = TRUE` drops them before journeys are built, so a conversion
#' recorded twice is one conversion.
#'
#' **Deterministic ordering.** Events sharing a timestamp are broken by their
#' original row order, so the same input always yields the same journeys. The
#' one exception is a conversion, which sorts after every touch sharing its
#' timestamp: a touch logged in the same second as a sale belongs to the
#' journey that sale closes, wherever the two rows sit in the file. Ties are
#' common in logs written at second resolution, and a non-deterministic
#' tie-break makes first-touch and last-touch results unreproducible.
#'
#' **Accounting.** A direct-traffic rule can strip a converting journey of every
#' touchpoint, leaving a conversion that no channel can be credited with. (A
#' lookback alone cannot do this: the conversion touch is always inside its own
#' window.) Those conversions are counted and reported by [path_summary()] as
#' `conversions_unattributable` rather than disappearing -- including in the
#' degenerate case where filtering removes every row and the returned table is
#' empty.
#'
#' @section What is dropped, and when:
#' Two things are removed that no argument controls, because they are not
#' choices so much as consequences of what a journey is.
#'
#' Touches occurring *after* a journey's conversion are dropped. With the
#' default `split_on = "conversion"` this is invisible, since every journey ends
#' at its conversion by construction. Under `split_on = "none"` or `"gap"` it is
#' not: a customer's events after their last conversion are discarded, because a
#' touch that happened after the sale cannot have caused it. If you want those
#' events, they belong to the next journey -- split on conversion.
#'
#' Journeys with no remaining touchpoints are dropped entirely, and any
#' conversion they carried is reported as unattributable rather than as absent.
#'
#' @section Timestamps:
#' A character `timestamp` column is parsed completely or not at all. Without
#' `timestamp_format`, every value must be an ISO 8601 date such as
#' `"2024-05-01"` or date-time such as `"2024-05-01 09:30"`,
#' `"2024-05-01T09:30:15.250Z"` or `"2024-05-01T11:30:15+02:00"`; a `/` may
#' replace the `-` in the date, and a trailing `UTC` or `GMT` reads as offset
#' zero. A value with a UTC offset is that instant. A value without one is read
#' in `tz`, and a date alone is midnight in `tz`. A column that mixes dates with
#' date-times is an error, because reading `"2024-05-01"` as midnight would
#' place it before every touch logged later that day. Any other text, such as
#' `"05/01/2024"`, is an error that names values which did not parse; give
#' `timestamp_format`, or convert the column with [as.POSIXct()] first.
#'
#' A numeric `timestamp` column is a period index taken at face value:
#' `lookback`, `gap` and `time_to_conversion` are all in index units, and
#' `units` has no effect. Values that look like Unix epoch time give a
#' warning, since seconds since 1970 read as an index make `lookback = 30`
#' thirty seconds.
#'
#' @section On what these numbers mean:
#' Rule-based attribution is a bookkeeping convention, not a causal estimate.
#' It divides observed conversions among observed touchpoints according to a
#' rule you chose; it does not tell you what would have happened had a channel
#' not run. Only an experiment or a credible quasi-experiment answers that. A
#' well-built journey table makes the bookkeeping honest and comparable across
#' rules, which is worth a great deal, and it is not incrementality.
#'
#' @seealso [credit_linear()] and friends for assigning credit,
#'   [attribute()] to compare rules, [conversion_lag()] to choose a lookback,
#'   [as_channel_paths()] to hand off to \pkg{ChannelAttribution}.
#'
#' @examples
#' data(mm_events)
#' head(mm_events)
#'
#' paths <- build_paths(
#'   mm_events,
#'   id = "customer_id",
#'   channel = "channel",
#'   timestamp = "timestamp",
#'   conversion = "conversion",
#'   value = "value"
#' )
#' path_summary(paths)
#'
#' # A 30-day lookback, splitting also on two weeks of inactivity
#' paths30 <- build_paths(
#'   mm_events,
#'   id = "customer_id", channel = "channel", timestamp = "timestamp",
#'   conversion = "conversion", value = "value",
#'   lookback = 30,
#'   split_on = c("conversion", "gap"),
#'   gap = 14
#' )
#' path_summary(paths30)
#' @export
build_paths <- function(events,
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
                        direct_labels = c("direct", "(direct)", "none",
                                          "(none)", "(not set)", "unknown", ""),
                        tz = "UTC",
                        timestamp_format = NULL,
                        dedupe = TRUE) {
  direct <- match.arg(direct)
  units_given <- !missing(units)
  units <- match.arg(units)
  if (missing(split_on)) {
    split_on <- "conversion"
  } else {
    split_on <- match.arg(split_on, c("conversion", "gap", "none"),
                          several.ok = TRUE)
  }
  if ("none" %in% split_on && length(split_on) > 1L) {
    cli::cli_abort('{.arg split_on} = "none" cannot be combined with other values.')
  }
  .mm_check_flag(collapse_repeats, "collapse_repeats")
  .mm_check_flag(keep_null_paths, "keep_null_paths")
  .mm_check_flag(dedupe, "dedupe")
  if (!is.null(timestamp_format) && !.mm_is_string(timestamp_format)) {
    cli::cli_abort(
      '{.arg timestamp_format} must be a single string, such as \\
       {.code "%d/%m/%Y %H:%M"}.'
    )
  }

  if (!is.data.frame(events)) {
    cli::cli_abort("{.arg events} must be a data frame, not \\
                    {.obj_type_friendly {events}}.")
  }
  cols <- .mm_resolve_cols(events, id = id, channel = channel,
                           timestamp = timestamp, conversion = conversion,
                           value = value)
  ts_raw <- events[[cols$timestamp]]
  # A numeric timestamp is a bare index with no calendar behind it, so its
  # durations cannot be in days or hours, whatever `units` says.
  is_index <- is.numeric(ts_raw)
  out_units <- if (is_index) "index" else units
  if (is_index && units_given) {
    cli::cli_warn(c(
      "{.arg units} has no effect with a numeric {.arg timestamp} column.",
      i = "A numeric timestamp is a period index: {.arg lookback}, {.arg gap} \\
           and {.field time_to_conversion} are in the index's own units."
    ))
  }
  if (!is.null(timestamp_format) && !is.character(ts_raw) &&
      !is.factor(ts_raw)) {
    cli::cli_warn(
      "{.arg timestamp_format} is ignored because column \\
       {.val {cols$timestamp}} does not hold text."
    )
  }
  if (nrow(events) == 0L) return(.mm_empty_paths(out_units))

  n_events_in <- nrow(events)
  ids <- .mm_id_chr(events[[cols$id]])
  if (anyNA(ids)) cli::cli_abort("{.arg id} column {.val {cols$id}} contains \\
                                  missing values.")
  ts <- .mm_coerce_time(ts_raw, cols$timestamp, tz, timestamp_format)
  if (anyNA(ts)) {
    cli::cli_abort("{.arg timestamp} column {.val {cols$timestamp}} contains \\
                    missing values.")
  }
  dt <- data.table::data.table(
    .row_id = seq_len(n_events_in),
    id = ids,
    channel = as.character(events[[cols$channel]]),
    timestamp = ts
  )
  time_num <- .mm_index_numeric(dt$timestamp)
  scale <- .mm_time_scale(dt$timestamp, units)
  is_calendar <- !is_index
  if (is_index && min(time_num) >= 1e8) {
    cli::cli_warn(c(
      "Numeric {.arg timestamp} column {.val {cols$timestamp}} looks like Unix \\
       epoch time.",
      i = "A numeric timestamp is read as a period index, so {.arg lookback} \\
           and {.arg gap} are in the same units as the column.",
      i = 'Convert epoch seconds with \\
           {.code as.POSIXct(x, origin = "1970-01-01", tz = "UTC")}, after \\
           dividing milliseconds by 1000.'
    ))
  }

  flag <- if (is.null(cols$conversion)) integer(n_events_in) else
    .mm_as_flag(events[[cols$conversion]], cols$conversion)
  dt[, "conversion" := flag]
  # Values are read only where they mean something. Text on a non-converting
  # row is not revenue and is not validated.
  val <- rep(NA_real_, n_events_in)
  if (!is.null(cols$value) && any(flag == 1L)) {
    on <- flag == 1L
    val[on] <- .mm_as_number(events[[cols$value]][on], cols$value)
  }
  dt[, "value" := val]

  # --- exact duplicates ------------------------------------------------------
  # A pixel that fires twice logs one event as two identical rows. Kept, a
  # duplicated conversion closes a second, one-touch journey and doubles the
  # value.
  n_dup <- 0L
  if (dedupe) {
    dup <- duplicated(dt, by = c("id", "channel", "timestamp", "conversion",
                                 "value"))
    n_dup <- sum(dup)
    if (n_dup > 0L) {
      n_dup_conv <- sum(dup & dt$conversion == 1L)
      dt <- dt[!dup]
      time_num <- time_num[!dup]
      if (n_dup_conv > 0L) {
        cli::cli_inform(c(
          "Dropped {n_dup_conv} duplicate conversion event{?s}.",
          i = "Each repeated another row's id, channel, timestamp and value.",
          i = "Set {.code dedupe = FALSE} to keep exact duplicates."
        ))
      }
    }
  }

  if (!is.null(gap) && !"gap" %in% split_on) {
    cli::cli_warn(c(
      '{.arg gap} was supplied but {.arg split_on} does not include "gap".',
      i = "No inactivity splitting will happen.",
      i = 'Add {.code "gap"} to {.arg split_on} to use it.'
    ))
  }
  if ("conversion" %in% split_on && is.null(cols$conversion)) {
    cli::cli_warn(c(
      'Splitting on conversion with no {.arg conversion} column.',
      i = "Every journey will be treated as non-converting."
    ))
  }

  # --- direct / none / missing channel labels --------------------------------
  # Relabelling happens now; *dropping* happens after conversion facts are
  # fixed, so that discarding a direct touch can never erase the conversion it
  # recorded.
  # Labels are classified once per distinct value, not once per event.
  ch <- dt$channel
  lab <- unique(ch)
  code <- match(ch, lab)
  lab_direct <- is.na(lab) |
    tolower(trimws(ifelse(is.na(lab), "", lab))) %in% tolower(trimws(direct_labels))
  is_direct <- lab_direct[code]
  n_direct <- sum(is_direct)
  if (direct == "label") {
    ch[is_direct] <- "(direct)"
  } else {
    # Under "keep", labels are preserved as they are -- except that an absent
    # label is given a name so it can be referred to at all. A channel called
    # "" cannot be indexed by name in a table or a matrix, so leaving it
    # nameless would make the diagnostics unable to talk about it.
    lab[is.na(lab)] <- "(missing)"
    lab[trimws(lab) == ""] <- "(blank)"
    ch <- lab[code]
  }
  dt[, "channel" := ch]
  dt[, ".is_direct" := is_direct]

  # --- deterministic ordering ------------------------------------------------
  # A conversion sorts after the touches sharing its timestamp. Listed first,
  # it would close its journey before them and push them into the next one.
  dt[, ".t" := time_num]
  data.table::setorderv(dt, c("id", ".t", "conversion", ".row_id"))

  # --- journey assignment ----------------------------------------------------
  gap_num <- if ("gap" %in% split_on) {
    if (is.null(gap)) {
      cli::cli_abort('{.arg gap} is required when {.arg split_on} includes "gap".')
    }
    .mm_time_amount(gap, units, scale, "gap", is_calendar)
  } else NA_real_

  # Rows are sorted by customer and time, so each journey is a contiguous run
  # of rows. `.j` numbers the runs across the whole table and `.jn` within a
  # customer; journey facts are computed on the runs with vector operations
  # rather than one R call per group.
  n <- nrow(dt)
  new_id <- c(TRUE, dt$id[-1L] != dt$id[-n])
  starts <- .mm_journey_starts(new_id, dt$conversion, dt$.t, split_on, gap_num)
  run <- cumsum(starts)
  dt[, ".j" := run]
  dt[, ".jn" := run - run[which(new_id)][cumsum(new_id)] + 1L]
  n_journeys_raw <- run[n]

  n_conv_events <- sum(dt$conversion == 1L)

  # --- journey-level conversion facts, computed from every event -------------
  # A journey's conversion time is that of its last conversion row: rows are
  # in time order, and assigning by run lets the last row of each run win.
  conv_row <- which(dt$conversion == 1L)
  conv_t_j <- rep(NA_real_, n_journeys_raw)
  conv_t_j[run[conv_row]] <- dt$.t[conv_row]
  converted_j <- !is.na(conv_t_j)
  dt[, "conv_t" := conv_t_j[run]]
  dt[, "conversion_value" := .mm_run_sums(dt$value, run, n_journeys_raw,
                                          dt$conversion == 1L)[run]]
  dt[, "converted" := converted_j[run]]

  # Under split_on = "none" or "gap" one journey can hold several conversion
  # events. The journey is then credited as ONE conversion (with the summed
  # value), so rule-based totals fall short of the conversion count. That is a
  # consequence of the split, not an error, but it should not be silent.
  per_journey <- tabulate(run[conv_row], nbins = n_journeys_raw)
  n_multi <- sum(per_journey > 1L)
  if (n_multi > 0L) {
    extra <- sum(per_journey[per_journey > 1L]) - n_multi
    cli::cli_inform(c(
      "{n_multi} journey{?s} contain{?s/} more than one conversion event.",
      i = "Each is credited as a single conversion, so {extra} conversion \\
           event{?s} will not appear in credited totals.",
      i = 'Add {.val conversion} to {.arg split_on} to give each conversion \\
           its own journey.'
    ))
  }

  # The lookback anchor is fixed here, alongside the conversion facts and
  # before any filtering. Computing a non-converting journey's anchor after
  # direct touches were dropped would let `direct = "drop"` slide the window
  # backwards and readmit touches a lookback had excluded. A non-converting
  # journey is anchored at its last touch.
  last_t_j <- dt$.t[c(which(starts)[-1L] - 1L, n)]
  dt[, "anchor" := ifelse(converted_j, conv_t_j, last_t_j)[run]]

  n_conv_journeys <- sum(converted_j)

  # Everything known before filtering. Carried into the early returns so that a
  # configuration which removes every touchpoint still reports how many events
  # went in and how many conversions can no longer be attributed, rather than
  # reporting zeroes.
  counts <- list(
    events_in = n_events_in, events_out = 0L, direct_events = n_direct,
    journeys = 0L, journeys_before_filtering = n_journeys_raw,
    converting_journeys = 0L, conversion_events = n_conv_events,
    conversions_observed = n_conv_journeys,
    conversions_unattributable = n_conv_journeys,
    duplicate_events = n_dup
  )

  # --- drop post-conversion touches within a journey -------------------------
  dt <- dt[is.na(conv_t) | .t <= conv_t]

  # --- drop direct traffic ---------------------------------------------------
  if (direct == "drop") {
    dt <- dt[.is_direct == FALSE]
    if (nrow(dt) == 0L) {
      cli::cli_warn(c(
        "Every event was direct traffic; no journeys remain.",
        i = if (n_conv_journeys > 0L) {
          "All {n_conv_journeys} observed conversion{?s} {?is/are} \\
           unattributable under these settings."
        } else {
          "No conversions were observed in the input."
        }
      ))
      return(.mm_empty_paths(out_units, counts))
    }
  }
  dt[, ".is_direct" := NULL]

  # --- lookback --------------------------------------------------------------
  if (!is.null(lookback)) {
    lb <- .mm_time_amount(lookback, units, scale, "lookback", is_calendar)
    dt <- dt[.t >= (anchor - lb)]
    if (nrow(dt) == 0L) {
      cli::cli_warn(c(
        "The lookback window removed every touchpoint.",
        i = if (n_conv_journeys > 0L) {
          "All {n_conv_journeys} observed conversion{?s} {?is/are} \\
           unattributable under these settings."
        } else {
          "No conversions were observed in the input."
        }
      ))
      return(.mm_empty_paths(out_units, counts))
    }
  }
  dt[, "anchor" := NULL]

  # --- collapse consecutive duplicates ---------------------------------------
  # Filtering keeps each journey's rows contiguous and in order, so a row
  # repeats its predecessor's channel within a journey exactly when both the
  # channel and the run match.
  if (collapse_repeats) {
    m <- nrow(dt)
    keep_row <- c(TRUE, dt$channel[-1L] != dt$channel[-m] |
                    dt$.j[-1L] != dt$.j[-m])
    dt <- dt[keep_row]
  }

  # --- null paths ------------------------------------------------------------
  if (!keep_null_paths) dt <- dt[converted == TRUE]
  if (nrow(dt) == 0L) {
    cli::cli_warn("No journeys remain after filtering.")
    return(.mm_empty_paths(out_units, counts))
  }

  # --- ranks -----------------------------------------------------------------
  m <- nrow(dt)
  first <- c(TRUE, dt$.j[-1L] != dt$.j[-m])
  grp <- cumsum(first)
  dt[, "touch_rank" := seq_len(m) - which(first)[grp] + 1L]
  dt[, "touch_n" := tabulate(grp)[grp]]
  dt[, "time_to_conversion" := (conv_t - .t) / scale]
  dt[, "path_id" := paste(dt$id, dt$.jn, sep = "#")]

  n_conv_attributable <- length(unique(dt$.j[dt$converted]))

  keep <- c("path_id", "id", "channel", "timestamp", "touch_rank", "touch_n",
            "converted", "conversion_value", "time_to_conversion")
  out <- as.data.frame(dt[, keep, with = FALSE])
  rownames(out) <- NULL

  structure(
    out,
    class = c("mm_paths", "data.frame"),
    mm_units = out_units,
    mm_counts = list(
      events_in = n_events_in,
      events_out = nrow(out),
      direct_events = n_direct,
      journeys = data.table::uniqueN(out$path_id),
      journeys_before_filtering = n_journeys_raw,
      converting_journeys = n_conv_attributable,
      conversion_events = n_conv_events,
      conversions_observed = n_conv_journeys,
      conversions_unattributable = n_conv_journeys - n_conv_attributable,
      duplicate_events = n_dup
    ),
    mm_settings = list(split_on = split_on, lookback = lookback, gap = gap,
                       collapse_repeats = collapse_repeats,
                       keep_null_paths = keep_null_paths, direct = direct,
                       dedupe = dedupe)
  )
}

# Printed with cat() so that the whole printout goes to standard output, where
# print() belongs; cli's text functions write to the message stream.
#' @export
print.mm_paths <- function(x, ...) {
  plural <- function(n, word) paste0(n, " ", word, if (n == 1L) "" else "s")
  cat("<mm_paths>\n")
  if (nrow(x) == 0L) {
    cat("No journeys.\n")
    return(invisible(x))
  }
  first <- !duplicated(x$path_id)
  n_conv <- sum(first & x$converted)
  cat(plural(sum(first), "journey"), " over ", plural(nrow(x), "touchpoint"),
      " (", n_conv, " converting)\n", sep = "")
  cat("Mean journey length: ", round(mean(x$touch_n[first]), 2), "\n", sep = "")
  cnt <- attr(x, "mm_counts")
  if (!is.null(cnt) && isTRUE(cnt$conversions_unattributable > 0L)) {
    cat("! ", plural(cnt$conversions_unattributable, "conversion"),
        " lost every touchpoint to filtering and cannot be attributed.\n",
        sep = "")
  }
  print(utils::head(as.data.frame(x), 6L), ...)
  if (nrow(x) > 6L) cat("# ... ", plural(nrow(x) - 6L, "more row"), "\n", sep = "")
  invisible(x)
}

# ---- internals ---------------------------------------------------------------

#' @keywords internal
#' @noRd
.mm_resolve_cols <- function(data, ..., call = parent.frame()) {
  args <- list(...)
  out <- list()
  for (nm in names(args)) {
    v <- args[[nm]]
    if (is.null(v)) {
      out[[nm]] <- NULL
      next
    }
    if (!.mm_is_string(v)) {
      cli::cli_abort(
        "{.arg {nm}} must be a single column name given as a string.",
        call = call
      )
    }
    if (!v %in% names(data)) {
      cli::cli_abort(c(
        "Column {.val {v}} (given as {.arg {nm}}) is not in the data.",
        i = "Available: {.val {utils::head(names(data), 12)}}."
      ), call = call)
    }
    out[[nm]] <- v
  }
  out
}

#' @keywords internal
#' @noRd
.mm_coerce_time <- function(x, nm, tz, format = NULL, call = parent.frame()) {
  if (inherits(x, "POSIXlt")) return(as.POSIXct(x))
  if (inherits(x, "POSIXct") || inherits(x, "Date") || is.numeric(x)) return(x)
  if (is.character(x) || is.factor(x)) {
    return(.mm_parse_time(as.character(x), nm, tz, format, call = call))
  }
  cli::cli_abort(
    "Column {.val {nm}} must be {.cls POSIXct}, {.cls Date}, numeric or \\
     character.",
    call = call
  )
}

# Character timestamps are parsed completely or not at all. Base
# `as.POSIXct()` stops at the first character its default formats do not
# expect, so "2024-05-01T09:00:00Z" read as midnight and "05/01/2024" as the
# year 5, with no warning. Missing and blank values come back as NA for the
# caller to report.
#' @keywords internal
#' @noRd
.mm_parse_time <- function(x, nm, tz, format = NULL, call = parent.frame()) {
  s <- trimws(x)
  s[!is.na(s) & !nzchar(s)] <- NA_character_
  out <- rep(NA_real_, length(s))
  todo <- which(!is.na(s))
  if (length(todo) == 0L) return(.POSIXct(out, tz = tz))
  st <- s[todo]
  hint <- 'Convert the column with {.fn as.POSIXct} first, or give \\
           {.arg timestamp_format}, such as {.code "%d/%m/%Y %H:%M"}.'

  if (!is.null(format)) {
    # strptime() ignores trailing text. A sentinel appended to both the value
    # and the format turns that into a mismatch.
    p <- strptime(paste0(st, "\001"), paste0(format, "\001"), tz = tz)
    v <- as.numeric(as.POSIXct(p, tz = tz))
    bad <- is.na(v)
    if (any(bad)) {
      cli::cli_abort(c(
        "Column {.val {nm}} does not match {.arg timestamp_format} \\
         {.val {format}}.",
        x = "{sum(bad)} value{?s} did not parse completely, for example \\
             {.val {utils::head(unique(st[bad]), 3)}}."
      ), call = call)
    }
    out[todo] <- v
    return(.POSIXct(out, tz = tz))
  }

  # ISO 8601: a date, then optionally a time of day with optional seconds and
  # fraction, then optionally a UTC offset.
  re <- paste0(
    "^([0-9]{4})[-/]([0-9]{1,2})[-/]([0-9]{1,2})",
    "(?:[Tt ]([0-9]{1,2}):([0-9]{2})(?::([0-9]{2}(?:[.,][0-9]+)?))?",
    " ?([Zz]|UTC|GMT|[+-][0-9]{2}(?::?[0-9]{2})?)?)?$"
  )
  m <- regexpr(re, st, perl = TRUE)
  unmatched <- m < 0L
  if (any(unmatched)) {
    cli::cli_abort(c(
      "Column {.val {nm}} could not be parsed as a date-time.",
      x = "{sum(unmatched)} value{?s} {?is/are} not an ISO 8601 date or \\
           date-time, for example {.val {utils::head(unique(st[unmatched]), 3)}}.",
      i = hint
    ), call = call)
  }
  start <- attr(m, "capture.start")
  len <- attr(m, "capture.length")
  part <- function(k) {
    p <- substring(st, start[, k], start[, k] + len[, k] - 1L)
    p[len[, k] <= 0L] <- ""
    p
  }
  date <- paste(part(1L), part(2L), part(3L), sep = "-")
  hour <- part(4L)
  has_time <- nzchar(hour)
  if (any(has_time) && !all(has_time)) {
    cli::cli_abort(c(
      "Column {.val {nm}} mixes dates with date-times.",
      x = "{sum(!has_time)} value{?s} {?has/have} no time of day, for example \\
           {.val {utils::head(unique(st[!has_time]), 3)}}.",
      i = "Read as midnight, a date would sort before every touch later that \\
           day.",
      i = hint
    ), call = call)
  }
  if (!any(has_time)) {
    v <- as.numeric(as.POSIXct(strptime(date, "%Y-%m-%d", tz = tz), tz = tz))
  } else {
    secs <- part(6L)
    secs[!nzchar(secs)] <- "00"
    secs <- sub(",", ".", secs, fixed = TRUE)
    stamp <- paste0(date, " ", hour, ":", part(5L), ":", secs)
    off <- part(7L)
    has_off <- nzchar(off)
    v <- numeric(length(st))
    if (any(!has_off)) {
      v[!has_off] <- as.numeric(as.POSIXct(
        strptime(stamp[!has_off], "%Y-%m-%d %H:%M:%OS", tz = tz), tz = tz
      ))
    }
    if (any(has_off)) {
      # Read the wall-clock part as UTC, then remove the offset.
      v[has_off] <- as.numeric(as.POSIXct(
        strptime(stamp[has_off], "%Y-%m-%d %H:%M:%OS", tz = "UTC"), tz = "UTC"
      )) - .mm_utc_offset(off[has_off])
    }
  }
  bad <- is.na(v)
  if (any(bad)) {
    cli::cli_abort(c(
      "Column {.val {nm}} could not be parsed as a date-time.",
      x = "{sum(bad)} value{?s} {?is/are} not a valid date or time, for \\
           example {.val {utils::head(unique(st[bad]), 3)}}."
    ), call = call)
  }
  out[todo] <- v
  .POSIXct(out, tz = tz)
}

# Seconds east of UTC for ISO 8601 offsets: "Z", "UTC", "+02", "+0200",
# "-05:30". An out-of-range offset gives NA.
#' @keywords internal
#' @noRd
.mm_utc_offset <- function(off) {
  secs <- numeric(length(off))
  num <- !off %in% c("Z", "z", "UTC", "GMT")
  if (any(num)) {
    o <- off[num]
    sgn <- ifelse(substr(o, 1L, 1L) == "-", -1, 1)
    digits <- gsub(":", "", substring(o, 2L), fixed = TRUE)
    hh <- as.numeric(substr(digits, 1L, 2L))
    mm <- ifelse(nchar(digits) >= 4L, as.numeric(substr(digits, 3L, 4L)), 0)
    secs[num] <- ifelse(hh > 23 | mm > 59, NA_real_,
                        sgn * (hh * 3600 + mm * 60))
  }
  secs
}

# Customer ids as text. `as.character()` writes large whole numbers in
# scientific notation ("1e+05"), which no CRM key matches.
#' @keywords internal
#' @noRd
.mm_id_chr <- function(x) {
  if (is.double(x) && !is.object(x)) {
    out <- as.character(x)
    whole <- !is.na(x) & is.finite(x) & x == trunc(x) & abs(x) < 2^53
    out[whole] <- sprintf("%.0f", x[whole])
    return(out)
  }
  as.character(x)
}

#' @keywords internal
#' @noRd
.mm_as_flag <- function(x, nm = "conversion", call = parent.frame()) {
  if (is.logical(x)) return(as.integer(!is.na(x) & x))
  if (is.numeric(x)) return(as.integer(!is.na(x) & x != 0))
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    s <- tolower(trimws(x))
    num <- suppressWarnings(as.numeric(s))
    yes <- s %in% c("true", "t", "yes", "y")
    no <- s %in% c("false", "f", "no", "n", "", "na", "nan")
    # A value that is neither a number nor a recognised word used to count as
    # "not converted", so "purchase" or "2.0" lost conversions silently.
    bad <- !is.na(s) & is.na(num) & !yes & !no
    if (any(bad)) {
      cli::cli_abort(c(
        "Column {.val {nm}} could not be read as a conversion flag.",
        x = "{sum(bad)} value{?s} {?is/are} neither a number nor a recognised \\
             flag, for example {.val {utils::head(unique(x[bad]), 3)}}.",
        i = 'Use numbers (non-zero means converted), or "true"/"false", \\
             "yes"/"no", "t"/"f" or "y"/"n".'
      ), call = call)
    }
    out <- as.integer(yes)
    is_num <- !is.na(num)
    out[is_num] <- as.integer(num[is_num] != 0)
    return(out)
  }
  cli::cli_abort("The {.arg conversion} column must be logical, numeric or \\
                  character.", call = call)
}

# Divisor converting the timestamp's native numeric units into `units`.
#' @keywords internal
#' @noRd
.mm_time_scale <- function(x, units) {
  per <- c(secs = 1, mins = 60, hours = 3600, days = 86400, weeks = 604800)
  if (inherits(x, "POSIXct")) return(unname(per[units]))
  if (inherits(x, "Date")) return(unname(per[units] / 86400))
  1
}

#' @keywords internal
#' @noRd
.mm_time_amount <- function(v, units, scale, arg, is_calendar = TRUE,
                            call = parent.frame()) {
  if (inherits(v, "difftime")) {
    if (!is_calendar) {
      cli::cli_abort(c(
        "{.arg {arg}} is a {.cls difftime}, but the timestamp column is a \\
         plain number.",
        i = "A calendar duration has no meaning against a bare numeric index.",
        i = "Give {.arg {arg}} as a number in the same units as \\
             {.arg timestamp}."
      ), call = call)
    }
    # A difftime skipped every check the numeric form gets: length, sign and
    # missingness were all unvalidated, and an NA propagated into path ids.
    if (length(v) != 1L || is.na(v)) {
      cli::cli_abort(
        "{.arg {arg}} must be a single non-missing duration.", call = call
      )
    }
    amount <- as.numeric(v, units = units)
    if (is.nan(amount) || amount <= 0) {
      cli::cli_abort("{.arg {arg}} must be a positive duration.", call = call)
    }
    return(amount * scale)
  }
  # Inf is a valid duration: a lookback that keeps everything, or a gap that
  # never splits.
  v <- .mm_check_scalar(v, arg, lower = 0, inclusive = c(FALSE, TRUE),
                        allow_inf = TRUE, call = call)
  v * scale
}

# Coerce a value column to numbers. A factor read with `as.numeric()` gives
# level codes, which is silently wrong revenue.
#' @keywords internal
#' @noRd
.mm_as_number <- function(x, nm, call = parent.frame()) {
  if (is.numeric(x)) return(as.numeric(x))
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    out <- suppressWarnings(as.numeric(x))
    bad <- is.na(out) & !is.na(x) & trimws(x) != ""
    if (any(bad)) {
      cli::cli_abort(c(
        "Column {.val {nm}} could not be read as a number.",
        x = "{sum(bad)} value{?s} did not parse, for example \\
             {.val {utils::head(unique(x[bad]), 3)}}."
      ), call = call)
    }
    return(out)
  }
  if (is.logical(x)) return(as.numeric(x))
  cli::cli_abort("Column {.val {nm}} must be numeric.", call = call)
}

# The first row of each journey, for rows sorted by customer and time: each
# customer's first event and, within a customer, the event after a conversion
# or after more than `gap_num` of inactivity, as `split_on` asks.
#' @keywords internal
#' @noRd
.mm_journey_starts <- function(new_id, conversion, t, split_on, gap_num) {
  n <- length(new_id)
  starts <- new_id
  if (n > 1L) {
    if ("conversion" %in% split_on) {
      starts[-1L] <- starts[-1L] | conversion[-n] == 1L
    }
    if ("gap" %in% split_on) {
      starts[-1L] <- starts[-1L] | diff(t) > gap_num
    }
  }
  starts
}

# Sum of the non-missing `x` on rows where `on` holds, for each of `n_runs`
# runs; NA for a run with none. A run with one value keeps it as it is and
# longer runs use sum(), so each total equals summing that run on its own.
#' @keywords internal
#' @noRd
.mm_run_sums <- function(x, run, n_runs, on) {
  out <- rep(NA_real_, n_runs)
  rows <- which(on & !is.na(x))
  if (length(rows) == 0L) return(out)
  r <- run[rows]
  several <- r %in% r[duplicated(r)]
  out[r[!several]] <- x[rows[!several]]
  if (any(several)) {
    sums <- vapply(split(x[rows[several]], r[several]), sum, numeric(1))
    out[as.integer(names(sums))] <- sums
  }
  out
}

#' @keywords internal
#' @noRd
.mm_empty_paths <- function(units, counts = NULL) {
  out <- data.frame(
    path_id = character(0), id = character(0), channel = character(0),
    timestamp = as.POSIXct(character(0)), touch_rank = integer(0),
    touch_n = integer(0), converted = logical(0),
    conversion_value = numeric(0), time_to_conversion = numeric(0),
    stringsAsFactors = FALSE
  )
  if (is.null(counts)) {
    counts <- list(events_in = 0L, events_out = 0L, direct_events = 0L,
                   journeys = 0L, journeys_before_filtering = 0L,
                   converting_journeys = 0L, conversion_events = 0L,
                   conversions_observed = 0L, conversions_unattributable = 0L,
                   duplicate_events = 0L)
  }
  structure(out, class = c("mm_paths", "data.frame"), mm_units = units,
            mm_counts = counts, mm_settings = list())
}

#' @keywords internal
#' @noRd
.mm_check_paths <- function(paths, call = parent.frame()) {
  if (!inherits(paths, "mm_paths")) {
    cli::cli_abort(c(
      "{.arg paths} must be an {.cls mm_paths} object.",
      i = "Build one with {.fn build_paths}."
    ), call = call)
  }
  missing_cols <- setdiff(.mm_path_cols(), names(paths))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "{.arg paths} is missing {length(missing_cols)} structural column{?s}.",
      x = "Missing: {.val {missing_cols}}."
    ), call = call)
  }
  if (nrow(paths) == 0L) return(paths)

  na_cols <- c("path_id", "channel", "touch_rank", "touch_n", "converted")
  has_na <- vapply(na_cols, function(nm) anyNA(paths[[nm]]), logical(1))
  if (any(has_na)) {
    cli::cli_abort(c(
      "{.arg paths} has missing values in {.field {na_cols[has_na]}}.",
      i = "Every touch needs a journey id, a channel, a rank and a \\
           conversion flag."
    ), call = call)
  }
  if (!is.logical(paths$converted)) {
    cli::cli_abort(
      "{.field converted} in {.arg paths} must be logical, not \\
       {.obj_type_friendly {paths$converted}}.",
      call = call
    )
  }
  # Objects can reach here without passing through `[` or a dplyr verb: from
  # rbind() in an older session, data.table, or columns edited by hand. A
  # journey whose touches disagree on `converted` cannot be repaired, since
  # nothing says which flag is right; stale ranks can.
  st <- .mm_rank_state(paths)
  if (length(st$mixed) > 0L) {
    cli::cli_abort(c(
      "{length(st$mixed)} journey{?s} in {.arg paths} {?has/have} touches \\
       that disagree on {.field converted}, for example \\
       {.val {utils::head(st$mixed, 3)}}.",
      i = "Every {.fn build_paths} call numbers journeys from 1, so tables \\
           built separately reuse ids. Combine them with {.fn bind_paths}."
    ), call = call)
  }
  if (any(st$stale)) {
    n_stale <- length(unique(paths$path_id[st$stale]))
    cli::cli_inform(c(
      "Recomputed {.field touch_rank} and {.field touch_n} in {n_stale} \\
       journey{?s} where they did not match the rows present.",
      i = "The construction counts no longer describe the table and were \\
           dropped."
    ))
    paths$touch_rank <- st$rank
    paths$touch_n <- st$total
    attr(paths, "mm_counts") <- NULL
  }
  paths
}

#' Subset a journey table
#'
#' Behaves like subsetting a data frame, with three additions. If the result
#' no longer carries the columns that make a journey table a journey table,
#' the `mm_paths` class is dropped and a plain data frame is returned, so a
#' column subset cannot produce an object that claims to be an `mm_paths` but
#' cannot answer any question about journeys. Rows selected by a missing
#' (`NA`) row index are dropped with a message, as [subset()] drops them. And
#' `touch_rank` and `touch_n` are recomputed whenever they no longer match the
#' rows present.
#'
#' When a row subset removes part of a journey, dropping one channel say,
#' the first remaining touch becomes rank 1 and the result is a valid journey
#' table in its own right, so every credit rule sums to 1 per converting
#' journey. A row selected twice counts as two touches. Construction counts
#' describe the table [build_paths()] returned, so they are dropped once rows
#' change. Convert with `as.data.frame()` first to inspect the original ranks
#' of a partial selection.
#'
#' The \pkg{dplyr} verbs follow the same rules: `filter()`, `slice()`,
#' `arrange()` and `distinct()` subset rows as `[` does, and `mutate()` and
#' the joins recompute ranks the same way. Journey tables built by separate
#' [build_paths()] calls share journey ids; combine them with [bind_paths()].
#'
#' @param x An `mm_paths` object.
#' @param ... Passed to the data frame method.
#'
#' @return An `mm_paths` object when the structural columns survive, otherwise
#'   a plain data frame.
#'
#' @seealso [build_paths()], [path_summary()], [bind_paths()]
#'
#' @examples
#' data(mm_events)
#' paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
#'                      timestamp = "timestamp", conversion = "conversion")
#'
#' # Row subset: still a journey table
#' class(paths[paths$converted, ])
#'
#' # Column subset that drops the structure: plain data frame
#' class(paths[, c("channel", "touch_rank")])
#' @export
`[.mm_paths` <- function(x, ...) {
  keep_attrs <- attributes(x)[c("mm_units", "mm_counts", "mm_settings")]
  n_before <- nrow(x)
  out <- NextMethod()
  if (!is.data.frame(out)) return(out)
  if (!all(.mm_path_cols() %in% names(out))) return(.mm_as_plain(out))
  for (nm in names(keep_attrs)) {
    if (!is.null(keep_attrs[[nm]])) attr(out, nm) <- keep_attrs[[nm]]
  }
  class(out) <- "data.frame"
  # A missing value in a logical row index selects a row of missing values,
  # which belongs to no journey and breaks every function downstream.
  na_rows <- is.na(out$path_id)
  if (any(na_rows)) {
    cli::cli_inform(
      "Dropped {sum(na_rows)} row{?s} selected by a missing ({.code NA}) \\
       row index."
    )
    out <- out[!na_rows, , drop = FALSE]
  }
  # Construction counts describe the table `build_paths()` produced. Once rows
  # change they no longer describe this object, and carrying them would make
  # `path_summary()` mix parent counts with subset statistics. They are
  # removed explicitly because `[.data.frame` keeps unknown attributes.
  out <- .mm_rerank(out, rows_changed = nrow(out) != n_before || any(na_rows))
  class(out) <- c("mm_paths", "data.frame")
  out
}

# dplyr verbs bypass `[`. Row verbs (filter(), slice(), arrange(),
# distinct()) call dplyr_row_slice(), which here subsets with `[` so that
# both routes give the same journeys. mutate(), the joins and bind_rows()
# rebuild the result with dplyr_reconstruct(). Both methods are registered
# only once dplyr is loaded.
#' @exportS3Method dplyr::dplyr_row_slice
#' @noRd
dplyr_row_slice.mm_paths <- function(data, i, ...) {
  out <- data[i, , drop = FALSE]
  if (is.data.frame(out)) rownames(out) <- NULL
  out
}

#' @exportS3Method dplyr::dplyr_reconstruct
#' @noRd
dplyr_reconstruct.mm_paths <- function(data, template) {
  out <- NextMethod()
  if (!all(.mm_path_cols() %in% names(out))) return(.mm_as_plain(out))
  rows_changed <- nrow(out) != nrow(template)
  if (nrow(out) == 0L || anyNA(out$path_id)) {
    if (rows_changed) attr(out, "mm_counts") <- NULL
    return(out)
  }
  st <- .mm_rank_state(out)
  # Added rows that repeat a journey's rank come from binding tables whose
  # ids collide, or from a join that matched a touch twice. Re-ranking would
  # merge different journeys without a trace.
  if (nrow(out) > nrow(template) && length(st$repeated) > 0L) {
    cli::cli_abort(c(
      "{length(st$repeated)} journey{?s} would hold two touches with the \\
       same {.field touch_rank}, for example \\
       {.val {utils::head(st$repeated, 3)}}.",
      i = "Every {.fn build_paths} call numbers journeys from 1, so binding \\
           tables built separately merges different journeys. Combine them \\
           with {.fn bind_paths}.",
      i = "A join that matches a touch more than once repeats it the same \\
           way."
    ), call = NULL)
  }
  if (any(st$stale)) {
    out$touch_rank <- st$rank
    out$touch_n <- st$total
    rows_changed <- TRUE
  }
  if (rows_changed) attr(out, "mm_counts") <- NULL
  out
}

#' Combine journey tables
#'
#' Stacks journey tables built by separate [build_paths()] calls, such as
#' monthly batches of one event log. Every call numbers each customer's
#' journeys from 1, so the same `path_id` names different journeys in
#' different tables, and stacking them with `rbind()` would merge those
#' journeys. `bind_paths()` keeps them apart by prefixing each table's journey
#' ids with a batch label.
#'
#' @param ... `mm_paths` objects to combine, or a single list of them. When
#'   every table is named the names label the batches; otherwise the batches
#'   are labelled `1`, `2` and so on.
#' @param rekey Prefix journey ids with the batch label? Defaults to `TRUE`,
#'   which turns `"a#1"` from the batch labelled `jan` into `"jan:a#1"`. With
#'   `FALSE` ids are kept as they are, and an id found in more than one table
#'   is an error.
#' @param deparse.level Ignored; present for compatibility with [rbind()].
#'
#' @return An `mm_paths` object with every table's rows. Construction counts
#'   describe one [build_paths()] call each, so they are dropped and
#'   [path_summary()] reports the input-side columns as `NA`. Settings are kept
#'   when every table was built with the same ones.
#'
#' @details
#' A journey that crosses a batch boundary is two journeys here, because each
#' batch was split on its own. When that happens often, build the journeys
#' from the combined event log instead.
#'
#' The tables must share their columns, their time units and the class of
#' their timestamps. `rbind()` on journey tables behaves like
#' `bind_paths(rekey = FALSE)`: ids are kept, and an id found in more than one
#' table is an error rather than a merged journey. `dplyr::bind_rows()`
#' cannot tell the tables apart, and aborts when the combined rows repeat a
#' journey's touch ranks.
#'
#' @seealso [build_paths()]
#'
#' @examples
#' jan <- data.frame(id = "a", ch = c("tv", "search"), ts = c(1, 2),
#'                   conv = c(0, 0))
#' feb <- data.frame(id = "a", ch = c("email", "display"), ts = c(40, 41),
#'                   conv = c(0, 1))
#' p_jan <- build_paths(jan, id = "id", channel = "ch", timestamp = "ts",
#'                      conversion = "conv")
#' p_feb <- build_paths(feb, id = "id", channel = "ch", timestamp = "ts",
#'                      conversion = "conv")
#' unique(p_jan$path_id)
#' unique(p_feb$path_id)
#'
#' both <- bind_paths(jan = p_jan, feb = p_feb)
#' unique(both$path_id)
#' sum(credit_linear(both)$credit)
#' @export
bind_paths <- function(..., rekey = TRUE) {
  batches <- list(...)
  if (length(batches) == 1L && is.list(batches[[1L]]) &&
      !is.data.frame(batches[[1L]])) {
    batches <- batches[[1L]]
  }
  .mm_check_flag(rekey, "rekey")
  batches <- batches[!vapply(batches, is.null, logical(1))]
  if (length(batches) == 0L) {
    cli::cli_abort("{.fn bind_paths} needs at least one {.cls mm_paths} object.")
  }
  not_paths <- which(!vapply(batches, inherits, logical(1), "mm_paths"))
  if (length(not_paths) > 0L) {
    cli::cli_abort(c(
      "Every table given to {.fn bind_paths} must be an {.cls mm_paths} \\
       object.",
      x = "Table{?s} {not_paths} {?is/are} not."
    ))
  }
  labels <- NULL
  if (rekey) {
    nms <- names(batches)
    if (is.null(nms)) {
      labels <- as.character(seq_along(batches))
    } else if (any(is.na(nms) | !nzchar(nms)) || anyDuplicated(nms) > 0L) {
      cli::cli_abort(
        "Name every table given to {.fn bind_paths} or none, with unique \\
         names."
      )
    } else if (any(grepl(":", nms, fixed = TRUE))) {
      cli::cli_abort(
        'Table names become the prefix of {.code name:id} journey ids, so \\
         they cannot contain ":".'
      )
    } else {
      labels <- nms
    }
  }
  .mm_bind(unname(batches), labels)
}

#' @rdname bind_paths
#' @export
rbind.mm_paths <- function(..., deparse.level = 1) {
  batches <- list(...)
  batches <- batches[!vapply(batches, is.null, logical(1))]
  if (length(batches) == 1L) return(batches[[1L]])
  not_df <- which(!vapply(batches, is.data.frame, logical(1)))
  if (length(not_df) > 0L) {
    cli::cli_abort(
      "{.fn rbind} on a journey table takes data frames only; argument{?s} \\
       {not_df} {?is/are} not."
    )
  }
  .mm_bind(unname(batches), labels = NULL)
}

# Stack journey tables, prefixing ids with `labels` or, when `labels` is
# NULL, refusing ids found in more than one table.
#' @keywords internal
#' @noRd
.mm_bind <- function(batches, labels = NULL, call = parent.frame()) {
  lacking <- which(!vapply(batches, function(b) {
    all(.mm_path_cols() %in% names(b))
  }, logical(1)))
  if (length(lacking) > 0L) {
    cli::cli_abort(
      "Table{?s} {lacking} {?lacks/lack} the journey columns of an \\
       {.cls mm_paths} object.",
      call = call
    )
  }
  units <- unique(unlist(lapply(batches, attr, "mm_units")))
  if (length(units) > 1L) {
    cli::cli_abort(c(
      "The tables measure time in different units: {.val {units}}.",
      i = "Build them with the same {.arg units}, so that \\
           {.field time_to_conversion} means one thing."
    ), call = call)
  }
  ts_class <- unique(vapply(batches, function(b) class(b$timestamp)[1L],
                            character(1)))
  if (length(ts_class) > 1L) {
    cli::cli_abort(c(
      "The tables store timestamps in different classes: {.cls {ts_class}}.",
      i = "Convert them to one class before combining."
    ), call = call)
  }
  cols <- names(batches[[1L]])
  differ <- which(!vapply(batches, function(b) setequal(names(b), cols),
                          logical(1)))
  if (length(differ) > 0L) {
    cli::cli_abort(
      "Table{?s} {differ} {?has/have} different columns from the first.",
      call = call
    )
  }
  if (is.null(labels)) {
    ids <- unlist(lapply(batches, function(b) unique(b$path_id)),
                  use.names = FALSE)
    shared <- unique(ids[duplicated(ids)])
    if (length(shared) > 0L) {
      cli::cli_abort(c(
        "{length(shared)} journey id{?s} appear{?s/} in more than one table, \\
         for example {.val {utils::head(shared, 3)}}.",
        i = "Every {.fn build_paths} call numbers journeys from 1, so the \\
             same id can name different journeys.",
        i = "Use {.fn bind_paths}, which prefixes each table's ids with a \\
             label."
      ), call = call)
    }
  }
  settings <- lapply(batches, attr, "mm_settings")
  same_settings <- all(vapply(settings, identical, logical(1), settings[[1L]]))
  plain <- lapply(seq_along(batches), function(k) {
    b <- as.data.frame(batches[[k]])[cols]
    if (!is.null(labels)) b$path_id <- paste0(labels[[k]], ":", b$path_id)
    b
  })
  out <- as.data.frame(data.table::rbindlist(plain, use.names = TRUE))
  rownames(out) <- NULL
  out <- structure(
    out,
    class = c("mm_paths", "data.frame"),
    mm_units = if (length(units) > 0L) units else "days",
    mm_settings = if (same_settings && !is.null(settings[[1L]])) {
      settings[[1L]]
    } else {
      list()
    }
  )
  .mm_rerank(out, rows_changed = TRUE)
}

# The journey structure the rows present imply: the rank each touch should
# have and its journey's length, the rows whose stored values disagree, the
# journeys whose touches disagree on `converted`, and the journeys holding
# two touches with the same stored rank. Every credit rule and diagnostic
# reads `touch_rank` and `touch_n`, so after a subset that removes part of a
# journey the stored values would describe touches that are no longer there:
# first-touch credit would go to nobody, and a two-touch remnant of a
# five-touch journey would be scored as a middle. Order within a journey is
# kept.
#' @keywords internal
#' @noRd
.mm_rank_state <- function(x) {
  n <- nrow(x)
  if (n == 0L) {
    return(list(rank = integer(0), total = integer(0), stale = logical(0),
                mixed = character(0), repeated = character(0)))
  }
  o <- order(x$path_id, x$touch_rank, method = "radix")
  pid <- x$path_id[o]
  starts <- c(TRUE, pid[-1L] != pid[-n])
  run <- cumsum(starts)
  first_pos <- which(starts)
  rank <- integer(n)
  rank[o] <- seq_len(n) - first_pos[run] + 1L
  total <- integer(n)
  total[o] <- tabulate(run)[run]
  stale <- x$touch_rank != rank | x$touch_n != total
  stale[is.na(stale)] <- TRUE
  conv <- x$converted[o]
  mixed <- conv != conv[first_pos][run]
  stored <- x$touch_rank[o]
  repeated <- !starts & stored == c(stored[1L], stored[-n])
  list(rank = rank, total = total, stale = stale,
       mixed = unique(pid[!is.na(mixed) & mixed]),
       repeated = unique(pid[!is.na(repeated) & repeated]))
}

# Recompute `touch_rank` and `touch_n` where they no longer match the rows
# present, and drop the construction counts once the rows have changed.
#' @keywords internal
#' @noRd
.mm_rerank <- function(x, rows_changed = FALSE) {
  if (nrow(x) > 0L) {
    st <- .mm_rank_state(x)
    if (any(st$stale)) {
      x[["touch_rank"]] <- st$rank
      x[["touch_n"]] <- st$total
      rows_changed <- TRUE
    }
  }
  if (rows_changed) attr(x, "mm_counts") <- NULL
  x
}

# A data frame that has lost the journey columns is no longer a journey table.
#' @keywords internal
#' @noRd
.mm_as_plain <- function(x) {
  for (nm in c("mm_units", "mm_counts", "mm_settings")) attr(x, nm) <- NULL
  class(x) <- "data.frame"
  x
}

#' @keywords internal
#' @noRd
.mm_path_cols <- function() {
  c("path_id", "channel", "timestamp", "touch_rank", "touch_n", "converted",
    "conversion_value", "time_to_conversion")
}
