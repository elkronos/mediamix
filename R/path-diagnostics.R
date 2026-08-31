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
#'       touchpoint to a direct-traffic rule, and so cannot be credited to any
#'       channel. That number should be reported, not absorbed.}
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
  .mm_check_paths(paths)
  cnt <- attr(paths, "mm_counts")
  if (is.null(cnt)) {
    cnt <- list(events_in = NA_integer_, events_out = nrow(paths),
                direct_events = NA_integer_,
                journeys = length(unique(paths$path_id)),
                converting_journeys = length(unique(paths$path_id[paths$converted])),
                conversion_events = NA_integer_,
                conversions_observed = NA_integer_,
                conversions_unattributable = NA_integer_)
  }
  lens <- if (nrow(paths) == 0L) numeric(0) else
    vapply(split(paths$touch_n, paths$path_id), function(z) z[1L], numeric(1))
  data.frame(
    events_in = cnt$events_in,
    events_out = cnt$events_out,
    journeys = cnt$journeys,
    converting_journeys = cnt$converting_journeys,
    mean_length = if (length(lens)) mean(lens) else NA_real_,
    median_length = if (length(lens)) stats::median(lens) else NA_real_,
    max_length = if (length(lens)) max(lens) else NA_real_,
    direct_events = cnt$direct_events,
    conversion_events = cnt$conversion_events,
    conversions_observed = cnt$conversions_observed,
    conversions_unattributable = cnt$conversions_unattributable,
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
  .mm_check_paths(paths)
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
  .mm_check_paths(paths)
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
  tab <- as.data.frame.matrix(table(p$channel, factor(
    pos, levels = c("only", "first", "middle", "last"))))
  out <- data.frame(channel = rownames(tab), tab, stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out$touches <- out$only + out$first + out$middle + out$last
  out$first_last_ratio <- ifelse(out$last > 0, out$first / out$last, NA_real_)
  out[order(-out$touches), , drop = FALSE]
}

#' Assisted conversion matrix
#'
#' For each ordered pair of channels, how many converting journeys contain the
#' first channel somewhere before the second. Read a row as "this channel
#' assisted these channels".
#'
#' The diagonal counts journeys where a channel appears before *itself* --
#' repeat exposure. Note that under `build_paths()`'s default
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
  .mm_check_paths(paths)
  .mm_check_flag(normalise, "normalise")
  p <- paths[paths$converted, , drop = FALSE]
  chans <- sort(unique(paths$channel))
  m <- matrix(0, nrow = length(chans), ncol = length(chans),
              dimnames = list(assisting = chans, assisted = chans))
  if (nrow(p) == 0L) return(m)

  # Index by position, not by name. A channel label may legitimately be the
  # empty string -- `build_paths(direct = "keep")` preserves whatever the log
  # contained -- and matrix name-indexing throws on "" even when it is a real
  # dimname.
  code <- match(p$channel, chans)
  idx <- split(seq_len(nrow(p)), p$path_id)
  present <- numeric(length(chans))
  for (i in idx) {
    seqc <- code[i][order(p$touch_rank[i])]
    u <- unique(seqc)
    present[u] <- present[u] + 1
    if (length(seqc) < 2L) next
    pairs <- unique(do.call(rbind, lapply(seq_len(length(seqc) - 1L), function(j) {
      cbind(seqc[j], seqc[(j + 1L):length(seqc)])
    })))
    m[pairs] <- m[pairs] + 1
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
#' @param sep Separator between channels in the rendered path string.
#'
#' @return A data frame with one row per distinct channel sequence: `path`,
#'   `journeys` (how many journeys followed it), `conversions` and
#'   `conversion_rate`. Ordered by descending frequency, then alphabetically.
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
  .mm_check_paths(paths)
  n <- .mm_check_count(n, "n", min = 1L)
  .mm_check_flag(converting_only, "converting_only")
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
  tab <- tab[order(-tab$journeys, tab$path), , drop = FALSE]
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
#' @param probs Quantiles to report.
#'
#' @return A data frame with columns `quantile` and `lag`, in the journey
#'   table's time units, plus an attribute `units` naming them. Only converting
#'   journeys contribute.
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
  .mm_check_paths(paths)
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
#' together. Each element is exactly what the corresponding function returns.
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
  .mm_check_paths(paths)
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
.mm_path_strings <- function(p, sep) {
  ord <- order(p$path_id, p$touch_rank)
  p <- p[ord, , drop = FALSE]
  idx <- split(seq_len(nrow(p)), p$path_id)
  data.frame(
    path_id = names(idx),
    path = vapply(idx, function(i) paste(p$channel[i], collapse = sep),
                  character(1), USE.NAMES = FALSE),
    converted = vapply(idx, function(i) p$converted[i[1L]], logical(1),
                       USE.NAMES = FALSE),
    value = vapply(idx, function(i) {
      v <- p$conversion_value[i[1L]]
      if (is.na(v)) 0 else v
    }, numeric(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
}
