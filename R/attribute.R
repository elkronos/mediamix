#' Attribute conversions to channels under several rules at once
#'
#' Runs a set of credit rules over the same journey table and returns their
#' results stacked, so the spread between rules is visible rather than hidden by
#' a choice of default. That spread is the most useful output of rule-based
#' attribution: it bounds how much a channel's apparent value depends on the
#' convention rather than the data.
#'
#' @param paths An `mm_paths` object from [build_paths()].
#' @param rules Character vector of rules to apply: any of `"linear"`,
#'   `"first"`, `"last"`, `"position"`, `"time_decay"` and `"markov"`. The
#'   first five are heuristic conventions; `"markov"` is the data-driven
#'   removal-effect model of [markov_removal()]. All six are run by default. A
#'   rule named twice is run once.
#' @param first_weight,last_weight Passed to [credit_position()].
#' @param decay,period Passed to [credit_time_decay()].
#' @param by Optional names of columns of `paths` that divide the journeys
#'   into segments, such as a device column carried by the `keep` argument of
#'   [build_paths()]. Every rule then runs on each segment's journeys alone,
#'   so shares sum to 1 within each segment and rule (or are all 0 in a
#'   segment with no conversions), and the Markov chain is fitted per
#'   segment. Each column must take one value per journey and have
#'   no missing values. See *Segments*.
#' @param times Number of bootstrap replicates behind intervals on the
#'   shares: 0, the default, for no intervals, or from 2 to 1,000,000. See
#'   *Intervals*.
#' @param level Confidence level of the percentile intervals, in `(0, 1)`.
#' @param seed Optional random seed, one whole number, for reproducible
#'   intervals. The caller's random number stream is restored afterwards.
#'   Giving `level` or `seed` with `times = 0` has no effect, and a warning
#'   says so.
#'
#' @return A data frame with one row per rule and channel, or per segment,
#'   rule and channel when `by` is given:
#'   \describe{
#'     \item{The `by` columns}{The segment, in the columns' own types. Present
#'       only with `by`.}
#'     \item{`rule`}{Which credit rule produced the row.}
#'     \item{`channel`}{Channel label.}
#'     \item{`conversions`}{Fractional conversions credited to the channel.}
#'     \item{`share`}{`conversions` as a share of all conversions credited
#'       under that rule. Shares sum to 1 within each rule, and within each
#'       segment; where there is no conversion to share, every share is 0.}
#'     \item{`lower`, `upper`}{Percentile interval for `share` at `level`,
#'       present only when `times` is above 0. `NA` where fewer than two
#'       replicates define the share, as in a segment with no converting
#'       journey.}
#'     \item{`value`}{Credited conversion value, present only when the journey
#'       table carries values.}
#'     \item{`touches`}{Number of touchpoints on the channel across all
#'       journeys, converting or not.}
#'   }
#'   Rows are ordered by segment, then by rule in the order of `rules`, then
#'   by descending conversions, with ties in channel order (C locale, so the
#'   same on every platform). Segments are sorted by their values, factors in
#'   level order.
#'
#'   The result has class `mm_attribution`. With `by` it also has class
#'   `mm_attribution_segments` and the attribute `"segments"`, naming the
#'   segment columns. With intervals it also has class
#'   `mm_attribution_intervals` and the attributes `"level"` and `"times"`.
#'   For these two classes [plot()] draws each rule's share per channel; see
#'   [plot.mm_attribution_intervals()].
#'
#' @section Segments:
#' A segment holds whole journeys, so a column given to `by` must take one
#' value in each journey. A device column kept per touch can change within a
#' cross-device journey; give each journey one value first, for example the
#' device of its last touch:
#'
#' ```
#' last <- paths$touch_rank == paths$touch_n
#' at_last <- match(paths$path_id, paths$path_id[last])
#' paths$journey_device <- paths$device[last][at_last]
#' ```
#'
#' Each segment is attributed as if it were the whole table: a channel's
#' share is its share of that segment's credited conversions, `touches`
#' counts that segment's touches, and a channel appears only in segments
#' where it has a touch. With the `"markov"` rule, one warning names every
#' segment with no non-converting journeys, as [markov_removal()] warns about
#' a whole table without them.
#'
#' @section Intervals:
#' With `times` above 0, the journeys of each segment are resampled with
#' replacement `times` times, every rule is recomputed on each resample, and
#' `lower` and `upper` are percentile intervals of the resampled shares. They
#' show how far the shares move with the sample of journeys observed, with
#' the journey table's construction and each rule held fixed. They say
#' nothing about the choice of rule, which [attribution_spread()] measures.
#'
#' A resample gives the same shares as `attribute()` on a journey table
#' holding the drawn journeys, each draw a separate journey: a heuristic rule
#' credits each journey on its own, so its resampled totals reuse the credit
#' every journey received on the full data, and the Markov chain is refitted
#' from the transitions of the drawn journeys. Journeys are drawn
#' independently, so a customer's several journeys are treated as unrelated.
#' A resample with no converting journey defines no share; it is left out of
#' the intervals and counted in a message.
#'
#' Every draw comes after `seed` is set, and the caller's random number stream
#' is restored afterwards, as in [block_bootstrap()]. Without `seed` the
#' draws advance the session's stream.
#'
#' @seealso [credit_linear()] and friends, [markov_removal()],
#'   [attribution_spread()], [plot.mm_attribution()],
#'   [plot.mm_attribution_intervals()]
#'
#' @examples
#' data(mm_events)
#' paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
#'                      timestamp = "timestamp", conversion = "conversion",
#'                      value = "value")
#'
#' res <- attribute(paths)
#' head(res, 10)
#'
#' # Total credited conversions equals observed converting journeys, per rule
#' tapply(res$conversions, res$rule, sum)
#'
#' # How much does the answer depend on the rule?
#' attribution_spread(res)
#'
#' # How much does it depend on the sample? 90% intervals from 50 resamples
#' boot <- attribute(paths, rules = c("first", "last", "markov"), times = 50,
#'                   seed = 1)
#' head(boot)
#'
#' # Credit per segment, here a device recorded once per customer
#' ev <- mm_events
#' ev$device <- ifelse(grepl("[02468]$", ev$customer_id), "mobile", "desktop")
#' kept <- build_paths(ev, id = "customer_id", channel = "channel",
#'                     timestamp = "timestamp", conversion = "conversion",
#'                     keep = "device")
#' by_device <- attribute(kept, rules = c("first", "last"), by = "device")
#' by_device[by_device$channel == "display", ]
#' @export
attribute <- function(paths,
                      rules = c("linear", "first", "last", "position",
                                "time_decay", "markov"),
                      first_weight = 0.4, last_weight = 0.4,
                      decay = decay_from_half_life(7), period = 1,
                      by = NULL, times = 0L, level = 0.9, seed = NULL) {
  # Read before validation reassigns them, which makes missing() FALSE.
  unused <- c("level", "seed")[c(!missing(level), !is.null(seed))]
  paths <- .mm_check_paths(paths)
  # A rule named twice would be counted twice by attribution_spread()'s
  # mean_share.
  rules <- unique(match.arg(rules, c("linear", "first", "last", "position",
                                     "time_decay", "markov"),
                            several.ok = TRUE))
  by <- .mm_check_segment_cols(paths, by)
  times <- .mm_check_bounded_count(times, "times", min = 0L, max = 1e6)
  if (times == 1L) {
    cli::cli_abort(c(
      "{.arg times} must be 0, for no intervals, or at least 2.",
      i = "An interval needs at least two replicates."
    ))
  }
  level <- .mm_check_scalar(level, "level", lower = 0, upper = 1,
                            inclusive = c(FALSE, FALSE))
  seed <- .mm_check_seed(seed)
  if (times == 0L && length(unused) > 0L) {
    cli::cli_warn(c(
      "{.arg {unused}} {?has/have} no effect when {.arg times} is 0.",
      i = "Set {.arg times} to a number of bootstrap replicates to compute \\
           intervals."
    ))
  }

  if (nrow(paths) == 0L) {
    return(.mm_as_attribution(.mm_empty_attribution(paths, by, times), by,
                              times, level))
  }

  n_channels <- length(unique(paths$channel))
  if (as.numeric(times) * n_channels * length(rules) > 1e8) {
    cli::cli_abort(c(
      "{times} replicate{?s} of {n_channels} channel{?s} under \\
       {length(rules)} rule{?s} are too many to hold in memory.",
      i = "Use fewer replicates or fewer rules."
    ))
  }

  if (times > 0L && !is.null(seed)) {
    old <- if (exists(".Random.seed", envir = globalenv())) {
      get(".Random.seed", envir = globalenv())
    } else NULL
    on.exit({
      if (is.null(old)) rm(".Random.seed", envir = globalenv()) else
        assign(".Random.seed", old, envir = globalenv())
    }, add = TRUE)
    set.seed(seed)
  }

  has_value <- !all(is.na(paths$conversion_value))
  segs <- .mm_segments(paths, by)
  pieces <- vector("list", length(segs$rows))
  no_null <- character(0)
  undefined <- integer(0)
  for (g in seq_along(segs$rows)) {
    sub <- if (is.null(by)) paths else paths[segs$rows[[g]], , drop = FALSE]
    one <- .mm_attribute_rules(sub, rules, first_weight, last_weight, decay,
                               period, has_value, warn = is.null(by))
    if (!is.null(by) && one$no_null) no_null <- c(no_null, segs$labels[[g]])
    res <- one$result
    if (times > 0L) {
      reps <- .mm_share_replicates(sub, rules, one$credit, times)
      bounds <- .mm_share_intervals(reps$shares, res, level)
      res$lower <- bounds$lower
      res$upper <- bounds$upper
      if (reps$undefined > 0L && any(sub$converted)) {
        undefined <- c(undefined,
                       stats::setNames(reps$undefined, segs$labels[[g]]))
      }
    }
    pieces[[g]] <- res
  }

  if (length(no_null) > 0L) {
    cli::cli_warn(c(
      "{length(no_null)} segment{?s} {?has/have} no non-converting journeys: \\
       {.val {no_null}}.",
      i = "Without them the null state is never observed, so the baseline \\
           conversion probability is 1 there and each Markov removal effect \\
           moves up or down, depending on how the missing journeys differ.",
      i = "Build the paths with {.code keep_null_paths = TRUE}."
    ))
  }
  if (length(undefined) > 0L) .mm_note_undefined(undefined, times, by)

  out <- .mm_bind_segments(pieces, segs$values)
  front <- c(by, "rule", "channel", "conversions", "share",
             if (times > 0L) c("lower", "upper"))
  out <- out[, c(front, setdiff(names(out), front)), drop = FALSE]
  rownames(out) <- NULL
  .mm_as_attribution(out, by, times, level)
}

# The classes and attributes of an attribute() result, empty or not.
#' @keywords internal
#' @noRd
.mm_as_attribution <- function(out, by, times, level) {
  class(out) <- c(if (times > 0L) "mm_attribution_intervals",
                  if (!is.null(by)) "mm_attribution_segments",
                  "mm_attribution", "data.frame")
  if (!is.null(by)) attr(out, "segments") <- by
  if (times > 0L) {
    attr(out, "level") <- level
    attr(out, "times") <- times
  }
  out
}

#' Range of each channel's share across attribution rules
#'
#' Summarises the output of [attribute()] into one row per channel, showing the
#' range of its share of credited conversions across rules. A channel whose
#' share swings from 8% to 31% depending on the convention has not been
#' measured; it has been assigned a number.
#'
#' @param attribution The data frame returned by [attribute()].
#'
#' @return A data frame with one row per channel, ordered by descending spread:
#'   `channel`, `min_share`, `max_share`, `mean_share`, `spread`
#'   (`max_share` minus `min_share`) and, only when both the `"first"` and
#'   `"last"` rules were run, `first_last_ratio` (the first-touch share divided
#'   by the last-touch share, `NA` when the last-touch share is 0).
#'   For a result of `attribute(by = )`, one row per segment and channel
#'   instead, with the segment columns first and rows ordered by segment, then
#'   by descending spread.
#'
#'   `first_last_ratio` here is a ratio of credited *shares*, which is not the
#'   same quantity as the column of the same name in [channel_positions()], a
#'   ratio of touchpoint *counts*. They answer the same question in different
#'   currencies and will not agree numerically.
#'
#' @seealso [attribute()]
#'
#' @examples
#' data(mm_events)
#' paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
#'                      timestamp = "timestamp", conversion = "conversion")
#' attribution_spread(attribute(paths))
#' @export
attribution_spread <- function(attribution) {
  if (!is.data.frame(attribution) ||
      !all(c("rule", "channel", "share") %in% names(attribution))) {
    cli::cli_abort(c(
      "{.arg attribution} must be the data frame returned by {.fn attribute}.",
      i = "It needs {.field rule}, {.field channel} and {.field share} columns."
    ))
  }
  by <- attr(attribution, "segments")
  by <- by[by %in% names(attribution)]
  if (length(by) == 0L) return(.mm_spread_one(attribution))
  cols <- .mm_plain_cols(attribution, by)
  key <- .mm_paste_key(cols)
  first <- which(!duplicated(key))
  pieces <- lapply(first, function(f) {
    .mm_spread_one(attribution[key == key[f], , drop = FALSE])
  })
  if (length(pieces) == 0L) pieces <- list(.mm_spread_one(attribution))
  out <- .mm_bind_segments(pieces, cols[first, , drop = FALSE])
  out[, c(by, setdiff(names(out), by)), drop = FALSE]
}

# ---- internals ---------------------------------------------------------------

# attribution_spread() for one segment, or for a result without segments.
#' @keywords internal
#' @noRd
.mm_spread_one <- function(attribution) {
  if (nrow(attribution) == 0L) {
    return(data.frame(channel = character(0), min_share = numeric(0),
                      max_share = numeric(0), mean_share = numeric(0),
                      spread = numeric(0), stringsAsFactors = FALSE))
  }
  ch <- unique(attribution$channel)
  agg <- function(f) vapply(ch, function(c0)
    f(attribution$share[attribution$channel == c0]), numeric(1))

  out <- data.frame(
    channel = ch,
    min_share = agg(min),
    max_share = agg(max),
    mean_share = agg(mean),
    stringsAsFactors = FALSE
  )
  out$spread <- out$max_share - out$min_share

  if (all(c("first", "last") %in% attribution$rule)) {
    # Matched by position, not by name: a channel label may legitimately be the
    # empty string, and `x[""]` never matches in R.
    pick <- function(rule) {
      sub <- attribution[attribution$rule == rule, , drop = FALSE]
      sub$share[match(ch, sub$channel)]
    }
    fs <- pick("first")
    ls <- pick("last")
    out$first_last_ratio <- ifelse(!is.na(ls) & ls > 0, fs / ls, NA_real_)
  }
  out <- out[order(-out$spread), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Every rule on one journey table: the stacked result, the credit each touch
# received under each heuristic rule (one column per rule, for the
# bootstrap), and whether the table lacks non-converting journeys. `has_value`
# comes from the whole table, so that every segment has the same columns.
#' @keywords internal
#' @noRd
.mm_attribute_rules <- function(paths, rules, first_weight, last_weight,
                                decay, period, has_value, warn = TRUE) {
  touch_tab <- table(paths$channel)
  touch_lab <- names(touch_tab)
  heuristic <- setdiff(rules, "markov")
  credit <- matrix(0, nrow(paths), length(heuristic),
                   dimnames = list(NULL, heuristic))
  out <- vector("list", length(rules))
  for (i in seq_along(rules)) {
    r <- rules[[i]]
    if (r == "markov") {
      mk <- .mm_markov_removal(paths, warn = warn)
      agg <- data.frame(channel = mk$channel, conversions = mk$conversions,
                        share = mk$share, stringsAsFactors = FALSE)
      if (has_value) agg$value <- if (is.null(mk$value)) 0 else mk$value
    } else {
      scored <- switch(
        r,
        linear = credit_linear(paths),
        first = credit_first(paths),
        last = credit_last(paths),
        position = credit_position(paths, first_weight, last_weight),
        time_decay = credit_time_decay(paths, decay = decay, period = period)
      )
      credit[, r] <- scored$credit
      agg <- stats::aggregate(list(conversions = scored$credit),
                              by = list(channel = scored$channel), FUN = sum)
      total <- sum(agg$conversions)
      agg$share <- if (total > 0) agg$conversions / total else 0
      if (has_value) {
        # A segment whose journeys carry no value has no credit_value column.
        cv <- scored$credit_value
        if (is.null(cv)) cv <- numeric(nrow(scored))
        av <- stats::aggregate(list(value = cv),
                               by = list(channel = scored$channel), FUN = sum)
        agg <- merge(agg, av, by = "channel", sort = FALSE)
      }
    }
    agg$touches <- as.integer(touch_tab[match(agg$channel, touch_lab)])
    agg$rule <- r
    out[[i]] <- agg[order(-agg$conversions, agg$channel, method = "radix"), ,
                    drop = FALSE]
  }
  res <- do.call(rbind, out)
  front <- c("rule", "channel", "conversions", "share")
  res <- res[, c(front, setdiff(names(res), front)), drop = FALSE]
  rownames(res) <- NULL
  first <- !duplicated(paths$path_id)
  list(result = res, credit = credit,
       no_null = "markov" %in% rules && !any(!paths$converted[first]))
}

# The zero-row result, with the columns a non-empty one would have.
#' @keywords internal
#' @noRd
.mm_empty_attribution <- function(paths, by, times) {
  out <- data.frame(rule = character(0), channel = character(0),
                    conversions = numeric(0), share = numeric(0),
                    stringsAsFactors = FALSE)
  if (times > 0L) {
    out$lower <- numeric(0)
    out$upper <- numeric(0)
  }
  out$touches <- integer(0)
  if (is.null(by)) return(out)
  segs <- lapply(stats::setNames(by, by), function(b) paths[[b]][0])
  cbind(as.data.frame(segs, stringsAsFactors = FALSE, optional = TRUE), out)
}

# Columns of `paths` that define segments. Each must be a complete vector
# that is constant within every journey, and must not take the name of a
# column of the result.
#' @keywords internal
#' @noRd
.mm_check_segment_cols <- function(paths, by, call = parent.frame()) {
  if (is.null(by)) return(NULL)
  if (!is.character(by) || length(by) == 0L || anyNA(by) ||
      !all(nzchar(by))) {
    cli::cli_abort(
      "{.arg by} must be a character vector of column names, not \\
       {.obj_type_friendly {by}}.",
      call = call
    )
  }
  by <- unique(by)
  absent <- setdiff(by, names(paths))
  if (length(absent) > 0L) {
    cli::cli_abort(c(
      "{cli::qty(length(absent))}Column{?s} {.val {absent}} (given as \\
       {.arg by}) {?is/are} not in {.arg paths}.",
      i = "Carry event columns into the journey table with the {.arg keep} \\
           argument of {.fn build_paths}."
    ), call = call)
  }
  # Names of the columns attribute() and attribution_spread() produce, which
  # a segment column of the same name would overwrite.
  taken <- intersect(by, c("rule", "channel", "conversions", "share", "lower",
                           "upper", "value", "touches", "spread",
                           "min_share", "max_share", "mean_share",
                           "first_last_ratio"))
  if (length(taken) > 0L) {
    cli::cli_abort(c(
      "{.val {taken}} cannot define segments: the result has a column of \\
       that name.",
      i = "Copy the column under another name first."
    ), call = call)
  }
  nested <- by[!vapply(by, function(b) {
    is.atomic(paths[[b]]) && is.null(dim(paths[[b]]))
  }, logical(1))]
  if (length(nested) > 0L) {
    cli::cli_abort(
      "{.arg by} takes vector columns; {.val {nested}} {?is a/are} list, \\
       matrix or data frame column{?s}.",
      call = call
    )
  }
  cols <- .mm_plain_cols(paths, by)
  .mm_check_key_columns(cols, "by", call = call)
  if (nrow(paths) > 0L) {
    key <- .mm_paste_key(cols)
    varies <- key != key[match(paths$path_id, paths$path_id)]
    if (any(varies)) {
      bad <- unique(paths$path_id[varies])
      cli::cli_abort(c(
        "{.arg by} must take one value per journey, but {length(bad)} \\
         journey{?s} change{?s/} value part-way, for example \\
         {.val {utils::head(bad, 3)}}.",
        i = "A segment holds whole journeys. Give each journey one value \\
             first, such as the value on its last touch."
      ), call = call)
    }
  }
  by
}

# The segments of a journey table: their values as a data frame, sorted, the
# rows of each, and a label for messages. Without `by` the whole table is one
# segment.
#' @keywords internal
#' @noRd
.mm_segments <- function(paths, by) {
  if (is.null(by)) {
    return(list(values = NULL, rows = list(seq_len(nrow(paths))),
                labels = ""))
  }
  cols <- .mm_plain_cols(paths, by)
  key <- .mm_paste_key(cols)
  first <- which(!duplicated(key))
  first <- first[do.call(order, c(unname(lapply(cols, `[`, first)),
                                  list(method = "radix")))]
  rows <- split(seq_len(nrow(paths)), factor(key, levels = key[first]))
  values <- cols[first, , drop = FALSE]
  rownames(values) <- NULL
  list(values = values, rows = unname(rows),
       labels = .mm_show_key(key[first]))
}

# Stack per-segment results, adding each segment's values as columns.
#' @keywords internal
#' @noRd
.mm_bind_segments <- function(pieces, values) {
  out <- do.call(rbind, pieces)
  if (is.null(values)) return(out)
  at <- rep(seq_along(pieces), vapply(pieces, nrow, integer(1)))
  for (b in names(values)) out[[b]] <- values[[b]][at]
  rownames(out) <- NULL
  out
}

# Bootstrap replicates of every share in one segment, as an array of
# replicates by channels by rules. Journeys are drawn with replacement. A
# heuristic rule credits each journey on its own and the Markov counts are
# sums over journeys, so a replicate weights each journey's credit and
# transitions by the number of times it was drawn.
#' @keywords internal
#' @noRd
.mm_share_replicates <- function(paths, rules, credit, times) {
  pid <- paths$path_id
  ids <- unique(pid)
  n_j <- length(ids)
  jid <- match(pid, ids)
  chans <- sort(unique(paths$channel), method = "radix")
  k <- length(chans)
  code <- match(paths$channel, chans)
  converted <- paths$converted[match(ids, pid)]
  heuristic <- colnames(credit)
  # Touches of non-converting journeys hold no credit and are left out.
  held <- rowSums(credit) > 0
  cr <- credit[held, , drop = FALSE]
  cj <- jid[held]
  cc <- code[held]
  markov <- "markov" %in% rules
  if (markov) {
    o <- order(pid, paths$touch_rank, method = "radix")
    walks <- .mm_markov_walks(
      data.frame(path_id = pid[o], channel = paths$channel[o],
                 converted = paths$converted[o], stringsAsFactors = FALSE),
      chans
    )
    wj <- jid[o][walks$row]
  }
  shares <- array(NA_real_, c(times, k, length(rules)),
                  dimnames = list(NULL, chans, rules))
  undefined <- 0L
  for (r in seq_len(times)) {
    w <- tabulate(sample.int(n_j, n_j, replace = TRUE), n_j)
    if (!any(w > 0L & converted)) {
      undefined <- undefined + 1L
      next
    }
    if (length(heuristic) > 0L) {
      s <- rowsum(cr * w[cj], cc)
      tot <- matrix(0, k, length(heuristic))
      tot[as.integer(rownames(s)), ] <- s
      shares[r, , heuristic] <- sweep(tot, 2L, colSums(tot), "/")
    }
    if (markov) {
      fit <- .mm_markov_fit(.mm_markov_counts(walks$cell, k, w[wj]))
      re <- pmax(0, 1 - fit$p_without / fit$base)
      shares[r, , "markov"] <- re / sum(re)
    }
  }
  list(shares = shares, undefined = undefined)
}

# Percentile bounds for the share on each row of `res`, from the replicate
# array of its segment.
#' @keywords internal
#' @noRd
.mm_share_intervals <- function(shares, res, level) {
  a <- (1 - level) / 2
  ch <- match(res$channel, dimnames(shares)[[2L]])
  ru <- match(res$rule, dimnames(shares)[[3L]])
  bounds <- vapply(seq_len(nrow(res)), function(i) {
    v <- shares[, ch[[i]], ru[[i]]]
    v <- v[is.finite(v)]
    if (length(v) < 2L) return(c(NA_real_, NA_real_))
    stats::quantile(v, probs = c(a, 1 - a), names = FALSE)
  }, numeric(2))
  list(lower = bounds[1L, ], upper = bounds[2L, ])
}

# Report replicates that drew no converting journey, by segment.
#' @keywords internal
#' @noRd
.mm_note_undefined <- function(undefined, times, by) {
  if (is.null(by)) {
    cli::cli_inform(
      "{undefined[[1L]]} of {times} replicate{?s} drew no converting \\
       journey, so {cli::qty(undefined[[1L]])}{?it defines/they define} no \\
       share and the intervals leave {?it/them} out."
    )
    return(invisible(NULL))
  }
  by_segment <- paste0(names(undefined), " (", undefined, ")", collapse = ", ")
  cli::cli_inform(c(
    "{length(undefined)} segment{?s} drew no converting journey in some \\
     replicates, which the intervals leave out.",
    i = "Replicates left out, by segment: {by_segment}."
  ))
  invisible(NULL)
}
