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
#'   be `POSIXct`, `Date`, or numeric (a period index).
#' @param conversion Name of a column flagging conversion events, as a string,
#'   or `NULL`. Logical, or numeric where non-zero means converted.
#' @param value Name of a column holding conversion value (revenue), as a
#'   string, or `NULL`. Only read on rows where `conversion` is true.
#' @param lookback Maximum age of a touchpoint relative to the end of its
#'   journey. Touches older than this are dropped. `NULL` (the default) keeps
#'   everything. Numeric, interpreted in `units`, or a `difftime`.
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
#'   For a numeric `timestamp` column the index is taken at face value and
#'   `units` has no effect; passing a `difftime` against a numeric index is an
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
#' @param tz Time zone used when coercing a character `timestamp` column.
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
#' often a path ends in nothing. Dropping them does not merely lose data, it
#' biases every channel's estimated effect upward, and it does so silently.
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
#' the first of a run pushes each channel's apparent recency backward.
#'
#' **Deterministic ordering.** Events sharing a timestamp are broken by their
#' original row order, so the same input always yields the same journeys. Ties
#' are common in logs written at second resolution, and a non-deterministic
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
                        tz = "UTC") {
  direct <- match.arg(direct)
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

  if (!is.data.frame(events)) {
    cli::cli_abort("{.arg events} must be a data frame, not \\
                    {.obj_type_friendly {events}}.")
  }
  cols <- .mm_resolve_cols(events, id = id, channel = channel,
                           timestamp = timestamp, conversion = conversion,
                           value = value)
  if (nrow(events) == 0L) return(.mm_empty_paths(units))

  dt <- data.table::data.table(
    .row_id = seq_len(nrow(events)),
    id = as.character(events[[cols$id]]),
    channel = as.character(events[[cols$channel]]),
    timestamp = events[[cols$timestamp]]
  )
  n_events_in <- nrow(dt)

  if (anyNA(dt$id)) cli::cli_abort("{.arg id} column {.val {cols$id}} contains \\
                                    missing values.")
  dt[, "timestamp" := .mm_coerce_time(dt$timestamp, cols$timestamp, tz)]
  if (anyNA(dt$timestamp)) {
    cli::cli_abort("{.arg timestamp} column {.val {cols$timestamp}} contains \\
                    missing values.")
  }
  time_num <- .mm_index_numeric(dt$timestamp)
  scale <- .mm_time_scale(dt$timestamp, units)
  is_calendar <- inherits(dt$timestamp, "POSIXct") || inherits(dt$timestamp, "Date")

  dt[, "conversion" := if (is.null(cols$conversion)) 0L else
    .mm_as_flag(events[[cols$conversion]])]
  dt[, "value" := if (is.null(cols$value)) NA_real_ else
    .mm_as_number(events[[cols$value]], cols$value)]
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
  ch <- dt$channel
  is_direct <- is.na(ch) |
    tolower(trimws(ifelse(is.na(ch), "", ch))) %in% tolower(trimws(direct_labels))
  n_direct <- sum(is_direct)
  if (direct == "label") {
    ch[is_direct] <- "(direct)"
  } else {
    # Under "keep", labels are preserved as they are -- except that an absent
    # label is given a name so it can be referred to at all. A channel called
    # "" cannot be indexed by name in a table or a matrix, so leaving it
    # nameless would make the diagnostics unable to talk about it.
    ch[is.na(ch)] <- "(missing)"
    ch[trimws(ch) == ""] <- "(blank)"
  }
  dt[, "channel" := ch]
  dt[, ".is_direct" := is_direct]

  # --- deterministic ordering ------------------------------------------------
  dt[, ".t" := time_num]
  data.table::setorderv(dt, c("id", ".t", ".row_id"))

  # --- journey assignment ----------------------------------------------------
  gap_num <- if ("gap" %in% split_on) {
    if (is.null(gap)) {
      cli::cli_abort('{.arg gap} is required when {.arg split_on} includes "gap".')
    }
    .mm_time_amount(gap, units, scale, "gap", is_calendar)
  } else NA_real_

  dt[, "journey" := .mm_journey_index(.SD, split_on, gap_num), by = "id"]
  dt[, "path_id" := paste(id, journey, sep = "#")]

  n_conv_events <- sum(dt$conversion == 1L)

  # --- journey-level conversion facts, computed from every event -------------
  dt[, "conv_t" := {
    ct <- .t[conversion == 1L]
    if (length(ct) == 0L) NA_real_ else max(ct)
  }, by = "path_id"]
  dt[, "conversion_value" := {
    v <- value[conversion == 1L]
    v <- v[!is.na(v)]
    if (length(v) == 0L) NA_real_ else sum(v)
  }, by = "path_id"]
  dt[, "converted" := !is.na(conv_t)]

  # The lookback anchor is fixed here, alongside the conversion facts and
  # before any filtering. Computing a non-converting journey's anchor after
  # direct touches were dropped would let `direct = "drop"` slide the window
  # backwards and readmit touches a lookback had excluded.
  dt[, "anchor" := if (is.na(conv_t[1L])) max(.t) else conv_t[1L], by = "path_id"]

  n_journeys_raw <- data.table::uniqueN(dt$path_id)
  n_conv_journeys <- data.table::uniqueN(dt$path_id[dt$converted])

  # Everything known before filtering. Carried into the early returns so that a
  # configuration which removes every touchpoint still reports how many events
  # went in and how many conversions can no longer be attributed, rather than
  # reporting zeroes.
  counts <- list(
    events_in = n_events_in, events_out = 0L, direct_events = n_direct,
    journeys = 0L, journeys_before_filtering = n_journeys_raw,
    converting_journeys = 0L, conversion_events = n_conv_events,
    conversions_observed = n_conv_journeys,
    conversions_unattributable = n_conv_journeys
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
      return(.mm_empty_paths(units, counts))
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
      return(.mm_empty_paths(units, counts))
    }
  }
  dt[, "anchor" := NULL]

  # --- collapse consecutive duplicates ---------------------------------------
  if (collapse_repeats) {
    dt[, "keep_row" := c(TRUE, channel[-1L] != channel[-.N]), by = "path_id"]
    dt <- dt[keep_row == TRUE]
    dt[, "keep_row" := NULL]
  }

  # --- null paths ------------------------------------------------------------
  if (!keep_null_paths) dt <- dt[converted == TRUE]
  if (nrow(dt) == 0L) {
    cli::cli_warn("No journeys remain after filtering.")
    return(.mm_empty_paths(units, counts))
  }

  # --- ranks -----------------------------------------------------------------
  dt[, "touch_rank" := seq_len(.N), by = "path_id"]
  dt[, "touch_n" := .N, by = "path_id"]
  dt[, "time_to_conversion" := (conv_t - .t) / scale]

  n_conv_attributable <- data.table::uniqueN(dt$path_id[dt$converted])

  keep <- c("path_id", "id", "channel", "timestamp", "touch_rank", "touch_n",
            "converted", "conversion_value", "time_to_conversion")
  out <- as.data.frame(dt[, keep, with = FALSE])
  rownames(out) <- NULL

  structure(
    out,
    class = c("mm_paths", "data.frame"),
    mm_units = units,
    mm_counts = list(
      events_in = n_events_in,
      events_out = nrow(out),
      direct_events = n_direct,
      journeys = data.table::uniqueN(out$path_id),
      journeys_before_filtering = n_journeys_raw,
      converting_journeys = n_conv_attributable,
      conversion_events = n_conv_events,
      conversions_observed = n_conv_journeys,
      conversions_unattributable = n_conv_journeys - n_conv_attributable
    ),
    mm_settings = list(split_on = split_on, lookback = lookback, gap = gap,
                       collapse_repeats = collapse_repeats,
                       keep_null_paths = keep_null_paths, direct = direct)
  )
}

#' @export
print.mm_paths <- function(x, ...) {
  cli::cli_h3("{.cls mm_paths}")
  if (nrow(x) == 0L) {
    cli::cli_text("No journeys.")
    return(invisible(x))
  }
  first <- !duplicated(x$path_id)
  n_j <- sum(first)
  n_conv <- sum(first & x$converted)
  cli::cli_text("{n_j} journey{?s} over {nrow(x)} touchpoint{?s} \\
                 ({n_conv} converting)")
  cli::cli_text("Mean journey length: {round(mean(x$touch_n[first]), 2)}")
  cnt <- attr(x, "mm_counts")
  if (!is.null(cnt) && isTRUE(cnt$conversions_unattributable > 0L)) {
    cli::cli_alert_warning(
      "{cnt$conversions_unattributable} conversion{?s} lost every touchpoint \\
       to filtering and cannot be attributed."
    )
  }
  print(utils::head(as.data.frame(x), 6L))
  if (nrow(x) > 6L) cli::cli_text("{.emph ... {nrow(x) - 6L} more row{?s}}")
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
.mm_coerce_time <- function(x, nm, tz, call = parent.frame()) {
  if (inherits(x, "POSIXct") || inherits(x, "Date") || is.numeric(x)) return(x)
  if (is.character(x) || is.factor(x)) {
    out <- suppressWarnings(as.POSIXct(as.character(x), tz = tz))
    if (anyNA(out)) {
      cli::cli_abort(c(
        "Column {.val {nm}} could not be parsed as a date-time.",
        i = "Convert it with {.fn as.POSIXct} before calling {.fn build_paths}."
      ), call = call)
    }
    return(out)
  }
  cli::cli_abort(
    "Column {.val {nm}} must be {.cls POSIXct}, {.cls Date} or numeric.",
    call = call
  )
}

#' @keywords internal
#' @noRd
.mm_as_flag <- function(x, call = parent.frame()) {
  if (is.logical(x)) return(as.integer(!is.na(x) & x))
  if (is.numeric(x)) return(as.integer(!is.na(x) & x != 0))
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    return(as.integer(!is.na(x) & tolower(trimws(x)) %in%
                        c("1", "true", "t", "yes", "y")))
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
    if (!is.finite(amount) || amount <= 0) {
      cli::cli_abort("{.arg {arg}} must be a positive duration.", call = call)
    }
    return(amount * scale)
  }
  v <- .mm_check_scalar(v, arg, lower = 0, inclusive = c(FALSE, TRUE),
                        call = call)
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

# Journey index within one customer's time-ordered events.
#' @keywords internal
#' @noRd
.mm_journey_index <- function(sd, split_on, gap_num) {
  n <- nrow(sd)
  if (n == 0L) return(integer(0))
  starts <- logical(n)
  if ("conversion" %in% split_on && n > 1L) {
    starts[-1L] <- starts[-1L] | (sd$conversion[-n] == 1L)
  }
  if ("gap" %in% split_on && n > 1L) {
    starts[-1L] <- starts[-1L] | (diff(sd$.t) > gap_num)
  }
  cumsum(c(1L, as.integer(starts[-1L])))
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
                   conversions_observed = 0L, conversions_unattributable = 0L)
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
  invisible(paths)
}

#' Subset a journey table
#'
#' Behaves exactly like subsetting a data frame, with one addition: if the
#' result no longer carries the columns that make a journey table a journey
#' table, the `mm_paths` class is dropped and a plain data frame is returned.
#' This stops a column subset from producing an object that claims to be an
#' `mm_paths` but cannot answer any question about journeys.
#'
#' @param x An `mm_paths` object.
#' @param ... Passed to the data frame method.
#'
#' @return An `mm_paths` object when the structural columns survive, otherwise
#'   a plain data frame.
#'
#' @seealso [build_paths()], [path_summary()]
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
  if (!is.data.frame(out) || !all(.mm_path_cols() %in% names(out))) {
    if (is.data.frame(out)) class(out) <- "data.frame"
    return(out)
  }
  for (nm in names(keep_attrs)) {
    if (!is.null(keep_attrs[[nm]])) attr(out, nm) <- keep_attrs[[nm]]
  }
  # Construction counts describe the table `build_paths()` produced. Once rows
  # are removed they no longer describe this object, and carrying them would
  # make `path_summary()` mix parent counts with subset statistics. Strip them
  # explicitly: `[.data.frame` propagates unknown attributes by itself.
  if (!identical(nrow(out), n_before)) attr(out, "mm_counts") <- NULL
  class(out) <- c("mm_paths", "data.frame")
  out
}

#' @keywords internal
#' @noRd
.mm_path_cols <- function() {
  c("path_id", "channel", "timestamp", "touch_rank", "touch_n", "converted",
    "conversion_value", "time_to_conversion")
}
