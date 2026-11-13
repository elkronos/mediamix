#' Journey table summary
#'
#' The construction accounting for an `mm_paths` object: how many events went
#' in, how many journeys came out, and how many conversions were lost to
#' filtering along the way.
#'
#' @param paths An `mm_paths` object from [build_paths()].
#'
#' @return A one-row data frame with the columns
#'   \describe{
#'     \item{`events_in`, `events_out`}{Rows supplied, and rows retained.}
#'     \item{`journeys`, `converting_journeys`}{Journeys built, and how many
#'       ended in a conversion that can still be credited.}
#'     \item{`mean_length`, `median_length`, `max_length`}{Journey length in
#'       touchpoints.}
#'     \item{`direct_events`}{Events whose channel matched `direct_labels`.}
#'     \item{`conversion_events`}{Conversion *events* in the input. This
#'       exceeds `conversions_observed` when several conversions fall inside one
#'       journey, which happens under `split_on = "none"` or `"gap"`.}
#'     \item{`conversions_observed`}{Journeys that contained a conversion.}
#'     \item{`conversions_unattributable`}{Converting journeys that lost every
#'       touchpoint to a direct-traffic rule or to
#'       `conversion_touch = "drop"`, and so cannot be credited to any
#'       channel. That number should be reported, not absorbed.}
#'     \item{`journeys_before_filtering`}{Journeys found before post-conversion
#'       touches, direct traffic, conversion events under
#'       `conversion_touch = "drop"`, the lookback and null paths were removed.}
#'     \item{`duplicate_events`}{Exact duplicate events dropped before
#'       journeys were built (see the `dedupe` argument of [build_paths()]).
#'       `direct_events` and `conversion_events` count what remained.}
#'   }
#'
#'   On a subset of a journey table the construction counts no longer apply, and
#'   the columns describing the input are `NA` rather than the parent's values.
#'
#' @seealso [build_paths()], [path_diagnostics()]
#'
#' @examples
#' data(mm_events)
#' paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
#'                      timestamp = "timestamp", conversion = "conversion")
#' path_summary(paths)
#' @export
path_summary <- function(paths) {
  paths <- .mm_check_paths(paths)
  cnt <- attr(paths, "mm_counts")
  if (is.null(cnt)) {
    cnt <- list(events_out = nrow(paths),
                journeys = length(unique(paths$path_id)),
                converting_journeys = length(unique(paths$path_id[paths$converted])))
  }
  # A count the object does not carry, from a subset or from an older version
  # of the package, is NA rather than a missing column.
  get <- function(nm) if (is.null(cnt[[nm]])) NA_integer_ else cnt[[nm]]
  lens <- if (nrow(paths) == 0L) numeric(0) else
    vapply(split(paths$touch_n, paths$path_id), function(z) z[1L], numeric(1))
  data.frame(
    events_in = get("events_in"),
    events_out = get("events_out"),
    journeys = get("journeys"),
    converting_journeys = get("converting_journeys"),
    mean_length = if (length(lens)) mean(lens) else NA_real_,
    median_length = if (length(lens)) stats::median(lens) else NA_real_,
    max_length = if (length(lens)) max(lens) else NA_real_,
    direct_events = get("direct_events"),
    conversion_events = get("conversion_events"),
    conversions_observed = get("conversions_observed"),
    conversions_unattributable = get("conversions_unattributable"),
    journeys_before_filtering = get("journeys_before_filtering"),
    duplicate_events = get("duplicate_events"),
    stringsAsFactors = FALSE
  )
}

#' Journey length distribution
#'
#' @param paths An `mm_paths` object from [build_paths()].
#'
#' @return A data frame with one row per journey length: `length`, `journeys`,
#'   `converting`, and `conversion_rate`. The conversion rate by length is worth
#'   looking at before choosing a credit rule: if it is flat, extra touchpoints
#'   are recording exposure rather than driving it.
#'
#' @seealso [path_diagnostics()]
#'
#' @examples
#' data(mm_events)
#' paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
#'                      timestamp = "timestamp", conversion = "conversion")
#' path_lengths(paths)
#' @export
path_lengths <- function(paths) {
  paths <- .mm_check_paths(paths)
  if (nrow(paths) == 0L) {
    return(data.frame(length = integer(0), journeys = integer(0),
                      converting = integer(0), conversion_rate = numeric(0)))
  }
  first <- !duplicated(paths$path_id)
  len <- paths$touch_n[first]
  conv <- paths$converted[first]
  tab <- stats::aggregate(list(journeys = rep(1L, length(len)),
                               converting = as.integer(conv)),
                          by = list(length = len), FUN = sum)
  tab$conversion_rate <- tab$converting / tab$journeys
  tab[order(tab$length), , drop = FALSE]
}

#' Where each channel sits in the journey
#'
#' Counts how often each channel appears as the only touch, the first touch, a
#' middle touch or the last touch, and reports the ratio of first-touch to
#' last-touch appearances. A ratio well above 1 marks a channel that opens
#' journeys; well below 1 marks one that closes them.
#'
#' @param paths An `mm_paths` object from [build_paths()].
#' @param converting_only Restrict to converting journeys? Defaults to `TRUE`,
#'   which is what you want when reading these as a role description. Set
#'   `FALSE` to see raw exposure.
#'
#' @return A data frame with one row per channel, ordered by descending
#'   `touches`: `channel`, `only`, `first`, `middle`, `last`, `touches`, and
#'   `first_last_ratio` (the count of first-touch appearances divided by the
#'   count of last-touch appearances, `NA` when the channel never appears last).
#'
#'   This `first_last_ratio` is a ratio of touchpoint counts. The column of the
#'   same name in [attribution_spread()] is a ratio of credited shares; the two
#'   will not agree numerically.
#'
#' @seealso [path_diagnostics()], [assisted_conversions()]
#'
#' @examples
#' data(mm_events)
#' paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
#'                      timestamp = "timestamp", conversion = "conversion")
#' channel_positions(paths)
#' @export
channel_positions <- function(paths, converting_only = TRUE) {
  paths <- .mm_check_paths(paths)
  .mm_check_flag(converting_only, "converting_only")
  p <- if (converting_only) paths[paths$converted, , drop = FALSE] else paths
  if (nrow(p) == 0L) {
    return(data.frame(channel = character(0), only = integer(0),
                      first = integer(0), middle = integer(0),
                      last = integer(0), touches = integer(0),
                      first_last_ratio = numeric(0)))
  }
  pos <- ifelse(p$touch_n == 1L, "only",
                ifelse(p$touch_rank == 1L, "first",
                       ifelse(p$touch_rank == p$touch_n, "last", "middle")))
  chans <- sort(unique(p$channel), method = "radix")
  tab <- as.data.frame.matrix(table(
    factor(p$channel, levels = chans),
    factor(pos, levels = c("only", "first", "middle", "last"))
  ))
  out <- data.frame(channel = chans, tab, stringsAsFactors = FALSE)
  out$touches <- out$only + out$first + out$middle + out$last
  out$first_last_ratio <- ifelse(out$last > 0, out$first / out$last, NA_real_)
  out <- out[order(-out$touches, out$channel, method = "radix"), ,
             drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Assisted conversion matrix
#'
#' For each ordered pair of channels, how many converting journeys contain the
#' first channel somewhere before the second. Read a row as "this channel
#' assisted these channels".
#'
#' The diagonal counts journeys where a channel appears before *itself*, which
#' is repeat exposure. Under `build_paths()`'s default
#' `collapse_repeats = TRUE`, consecutive repeats have already been merged, so
#' the diagonal only picks up channels that recur after an intervening
#' different channel. Build the paths with `collapse_repeats = FALSE` if you
#' want back-to-back repeat exposure to show up here.
#'
#' @param paths An `mm_paths` object from [build_paths()].
#' @param normalise Divide each cell by the number of converting journeys
#'   containing the assisting channel, giving a rate rather than a count?
#'   Defaults to `FALSE`.
#'
#' @return A numeric matrix with assisting channels as rows and assisted
#'   channels as columns.
#'
#' @seealso [channel_positions()], [path_diagnostics()]
#'
#' @examples
#' data(mm_events)
#' paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
#'                      timestamp = "timestamp", conversion = "conversion")
#' round(assisted_conversions(paths, normalise = TRUE), 3)
#' @export
assisted_conversions <- function(paths, normalise = FALSE) {
  paths <- .mm_check_paths(paths)
  .mm_check_flag(normalise, "normalise")
  p <- paths[paths$converted, , drop = FALSE]
  chans <- sort(unique(paths$channel), method = "radix")
  m <- matrix(0, nrow = length(chans), ncol = length(chans),
              dimnames = list(assisting = chans, assisted = chans))
  if (nrow(p) == 0L) return(m)

  # Index by position, not by name. A channel label can still be the empty
  # string, as when `build_paths(direct = "label")` is given `direct_labels`
  # without "", and matrix name-indexing throws on "" even when it is a real
  # dimname.
  code <- match(p$channel, chans)
  dt <- data.table::data.table(pid = p$path_id, rank = p$touch_rank,
                               code = code)
  # Channel a precedes channel b somewhere in a journey exactly when a's first
  # touch comes before b's last, so each journey needs only the first and last
  # rank of each channel in it. Pairing those keeps the work per journey at
  # the square of its distinct channels however long it is; pairing every
  # touch grew with the square of its length.
  span <- dt[, list(first = min(rank), last = max(rank)), by = c("pid", "code")]
  present_tab <- span[, .N, by = "code"]
  present <- numeric(length(chans))
  present[present_tab$code] <- present_tab$N
  pairs <- span[span, on = "pid", allow.cartesian = TRUE, nomatch = NULL]
  pairs <- pairs[pairs$first < pairs[["i.last"]], ]
  if (nrow(pairs) > 0L) {
    tab <- pairs[, .N, by = c("code", "i.code")]
    m[cbind(tab$code, tab[["i.code"]])] <- tab$N
  }
  if (normalise) {
    denom <- present
    denom[denom == 0] <- 1
    m <- m / denom
  }
  m
}

#' Most common journeys
#'
#' @param paths An `mm_paths` object from [build_paths()].
#' @param n Number of distinct channel sequences to return. Each row aggregates
#'   every journey that followed that sequence, so the `journeys` column will
#'   normally sum to far more than `n`.
#' @param converting_only Restrict to converting journeys? Defaults to `FALSE`,
#'   because the commonest non-converting journeys are usually the more
#'   interesting half.
#' @param sep Separator between channels in the rendered path string. A
#'   channel name that contains `sep` is an error, since two different channel
#'   sequences could then render as the same string.
#'
#' @return A data frame with one row per distinct channel sequence: `path`,
#'   `journeys` (how many journeys followed it), `conversions` and
#'   `conversion_rate`. Ordered by descending frequency, then alphabetically
#'   in the C locale, so ties fall the same way on every platform.
#'
#' @seealso [as_channel_paths()], [path_diagnostics()]
#'
#' @examples
#' data(mm_events)
#' paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
#'                      timestamp = "timestamp", conversion = "conversion")
#' top_paths(paths, n = 8)
#' @export
top_paths <- function(paths, n = 10, sep = " > ", converting_only = FALSE) {
  paths <- .mm_check_paths(paths)
  n <- .mm_check_count(n, "n", min = 1L)
  .mm_check_flag(converting_only, "converting_only")
  .mm_check_sep(sep)
  p <- if (converting_only) paths[paths$converted, , drop = FALSE] else paths
  if (nrow(p) == 0L) {
    return(data.frame(path = character(0), journeys = integer(0),
                      conversions = integer(0), conversion_rate = numeric(0)))
  }
  strs <- .mm_path_strings(p, sep)
  tab <- stats::aggregate(
    list(journeys = rep(1L, nrow(strs)), conversions = as.integer(strs$converted)),
    by = list(path = strs$path), FUN = sum
  )
  tab$conversion_rate <- tab$conversions / tab$journeys
  tab <- tab[order(-tab$journeys, tab$path, method = "radix"), , drop = FALSE]
  rownames(tab) <- NULL
  utils::head(tab, n)
}

#' Conversion lag distribution
#'
#' How long journeys take, measured from first touch to conversion. This is how
#' you choose a lookback window instead of reaching for thirty days because
#' everyone else does: pick the quantile you are willing to truncate at and read
#' the window off the table.
#'
#' @param paths An `mm_paths` object from [build_paths()].
#' @param probs Quantiles to report: probabilities in `[0, 1]`.
#'
#' @return A data frame with columns `quantile` and `lag`, in the journey
#'   table's time units, plus an attribute `units` naming them. Only converting
#'   journeys contribute.
#'
#'   Run it on journeys built without `lookback`. A lookback removes every
#'   touch older than the window, so no lag can exceed it, and a warning says
#'   so.
#'
#' @seealso [build_paths()], [path_diagnostics()]
#'
#' @examples
#' data(mm_events)
#' paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
#'                      timestamp = "timestamp", conversion = "conversion",
#'                      split_on = "conversion")
#'
#' # 90% of journeys complete within this many days
#' conversion_lag(paths)
#' @export
conversion_lag <- function(paths, probs = c(0.5, 0.75, 0.9, 0.95, 0.99, 1)) {
  paths <- .mm_check_paths(paths)
  if (!is.numeric(probs) || length(probs) == 0L || anyNA(probs) ||
      any(probs < 0 | probs > 1)) {
    cli::cli_abort(
      "{.arg probs} must be a numeric vector of probabilities between 0 and \\
       1, with no missing values."
    )
  }
  lb <- attr(paths, "mm_settings")$lookback
  if (!is.null(lb) && is.finite(as.numeric(lb))) {
    cli::cli_warn(c(
      "{.arg paths} was built with a lookback window, so no lag can be longer \\
       than the window.",
      i = "Build the journeys without {.arg lookback} to choose a window from \\
           {.fn conversion_lag}."
    ))
  }
  units <- attr(paths, "mm_units")
  p <- paths[paths$converted, , drop = FALSE]
  if (nrow(p) == 0L) {
    out <- data.frame(quantile = probs, lag = NA_real_)
    attr(out, "units") <- units
    return(out)
  }
  lag <- vapply(split(p$time_to_conversion, p$path_id), function(z)
    suppressWarnings(max(z, na.rm = TRUE)), numeric(1))
  lag <- lag[is.finite(lag)]
  out <- data.frame(
    quantile = probs,
    lag = unname(stats::quantile(lag, probs = probs, names = FALSE, na.rm = TRUE)),
    stringsAsFactors = FALSE
  )
  attr(out, "units") <- units
  out
}

#' Run every journey diagnostic at once
#'
#' A convenience wrapper returning the diagnostics an analyst normally presents
#' together. Each element is what the corresponding function returns with its
#' defaults, except `assists`, which is
#' `assisted_conversions(paths, normalise = TRUE)`.
#'
#' @param paths An `mm_paths` object from [build_paths()].
#' @param n Number of top paths to include.
#'
#' @return A named list with elements `summary`, `lengths`, `positions`,
#'   `assists`, `top_paths` and `conversion_lag`.
#'
#' @seealso [path_summary()], [path_lengths()], [channel_positions()],
#'   [assisted_conversions()], [top_paths()], [conversion_lag()]
#'
#' @examples
#' data(mm_events)
#' paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
#'                      timestamp = "timestamp", conversion = "conversion")
#' diag <- path_diagnostics(paths)
#' names(diag)
#' diag$summary
#' @export
path_diagnostics <- function(paths, n = 10) {
  paths <- .mm_check_paths(paths)
  list(
    summary = path_summary(paths),
    lengths = path_lengths(paths),
    positions = channel_positions(paths),
    assists = assisted_conversions(paths, normalise = TRUE),
    top_paths = top_paths(paths, n = n),
    conversion_lag = conversion_lag(paths)
  )
}

#' @keywords internal
#' @noRd
.mm_check_sep <- function(sep, call = parent.frame()) {
  if (!.mm_is_string(sep) || !nzchar(sep)) {
    cli::cli_abort(
      "{.arg sep} must be a single string of at least one character.",
      call = call
    )
  }
  invisible(sep)
}

# One path string per journey. A channel name containing the separator would
# let two different channel sequences render as the same string, and the
# aggregation by string would then merge them.
#' @keywords internal
#' @noRd
.mm_path_strings <- function(p, sep, call = parent.frame()) {
  chans <- unique(p$channel)
  clash <- chans[grepl(sep, chans, fixed = TRUE)]
  if (length(clash) > 0L) {
    cli::cli_abort(c(
      "{.arg sep} {.val {sep}} occurs inside {length(clash)} channel \\
       name{?s}, for example {.val {utils::head(clash, 3)}}.",
      i = "Choose a separator that no channel name contains, so that every \\
           path string splits back into its channels."
    ), call = call)
  }
  dt <- data.table::data.table(path_id = p$path_id, rank = p$touch_rank,
                               channel = p$channel, converted = p$converted,
                               value = p$conversion_value)
  data.table::setorderv(dt, c("path_id", "rank"))
  out <- dt[, list(path = paste(channel, collapse = sep),
                   converted = converted[1L],
                   value = if (is.na(value[1L])) 0 else value[1L]),
            by = "path_id"]
  as.data.frame(out, stringsAsFactors = FALSE)
}
